/*
 * ipf.c
 * 
 * SPS "Interchangeable Preservation Format" (IPF) files.
 * 
 * Written & released by Keir Fraser <keir.xen@gmail.com>
 * 
 * This is free and unencumbered software released into the public domain.
 * See the file COPYING for more details, or visit <http://unlicense.org>.
 */

struct ipf_header {
    uint8_t id[4];
    uint32_t len;
    uint32_t crc;
};

struct ipf_info {
    uint32_t type;        /* 1 = FDD */
    uint32_t encoder;     /* 1 */
    uint32_t encrev;      /* 1 */
    uint32_t release;     /* 0x6666 (bogus, fake) */
    uint32_t revision;    /* 1 (bogus, fake)*/
    uint32_t origin;      /* 0 (bogus, fake) */
    uint32_t mincylinder; /* 0 */
    uint32_t maxcylinder; /* 83 */
    uint32_t minhead;     /* 0 */
    uint32_t maxhead;     /* 1 */
    uint32_t date;        /* year[2011-]*10000 + month[1-12]*100 + day[1-31] */
    uint32_t time;        /* h*10000000 + m*100000 + s*1000 + ms */
    uint32_t platform[4]; /* 1,0,0,0 */
    uint32_t disknum;     /* 0 */
    uint32_t userid;      /* 0 */
    uint32_t reserved[3]; /* 0,0,0 */
};

struct ipf_img {
    uint32_t cylinder; /* 0 - 83 */
    uint32_t head;     /* 0 or 1 */
    uint32_t dentype;  /* 1 = noise, 2 = auto, 3 = copylock */
    uint32_t sigtype;  /* 1 */
    uint32_t trksize;  /* ceil(trkbits/8) */
    uint32_t startpos; /* floor(startbit/8) */
    uint32_t startbit; /* bit offset from index of data start */
    uint32_t databits;
    uint32_t gapbits;
    uint32_t trkbits;  /* databits + gapbits */
    uint32_t blkcnt;   /* e.g., 11 for DOS */
    uint32_t process;  /* 0 */
    uint32_t flag;     /* 0 (unless flaky) */
    uint32_t dat_chunk;
    uint32_t reserved[3]; /* 0,0,0 */
};

struct ipf_data {
    uint32_t size;  /* ceil(bsize/8) */
    uint32_t bsize;
    uint32_t dcrc;  /* data area crc */
    uint32_t dat_chunk;
    /* Followed by #blocks ipf_block structures */
};

struct ipf_block {
    uint32_t blockbits;  /* decoded block size in bits */
    uint32_t gapbits;    /* decoded gap size in bits */
    union {
        struct {
            uint32_t blocksize;  /* ceil(blockbits/8) */
            uint32_t gapsize;    /* ceil(gapbits/8) */
        } caps;
        struct {
            uint32_t gapoffset;  /* 0 unless there is a gap stream */
            uint32_t celltype;   /* 1 for 2us MFM */
        } sps;
    } u;
    uint32_t enctype;    /* 1 */
    uint32_t flag;       /* 0 (bit 2 set means we count stream in bits!) */
    uint32_t gapvalue;   /* 0 */
    uint32_t dataoffset; /* offset of data stream in data area */
    /* Data is a set of chunks */
    /* Chunk start bytes is count_len[7:5], code[4:0] */
    /* count_len says how many following bytes contain (big endian) count */
    /* code is 0=end,1=sync,2=data,3=gap,4=raw,5=flakey */
};

/* Data stream chunk codes. */
enum chkcode { chkEnd=0, chkSync, chkData, chkGap, chkRaw, chkFlaky };

/* Decoder modes. */
enum mode { modeData, modeGap };

struct ipf_track {
    uint32_t imge_off;
    uint32_t data_off;
};

struct ipf_blkinf {
    uint32_t block_bits;
    uint32_t gap_bits:24;
    uint32_t gapvalue:8;
};

/* Prefetch buffer size */
#define PF_BUFSZ 2048
#define MAXTRK 170
#define MAXBLK 86

static bool_t read_header(struct image *im, const char *id,
                          struct ipf_header *header)
{
    UINT nr;
    im->fr = f_read(&im->fp, header, sizeof(*header), &nr);
    header->len = be32toh(header->len);
    return !(im->fr || (nr != sizeof(*header))
             || (id && strncmp((char *)header->id, id, 4)));
}

static bool_t read_block(struct image *im, void *blk, unsigned int sz)
{
    uint32_t *p = blk;
    UINT i, nr;
    im->fr = f_read(&im->fp, blk, sz, &nr);
    for (i = 0; i < (nr/4); i++)
        p[i] = be32toh(p[i]);
    return !(im->fr || (nr != sz));
}

static bool_t skip_fwd(struct image *im, unsigned int bytes)
{
    uint32_t pos = f_tell(&im->fp) + bytes;
    if (pos > f_size(&im->fp))
        return FALSE;
    im->fr = f_lseek(&im->fp, pos);
    return !im->fr;
}

static bool_t ipf_open(struct image *im)
{
    struct ipf_header header;
    struct ipf_info info;
    uint32_t trk = 0, _trk, off;

    if (!read_header(im, "CAPS", &header) || (header.len != sizeof(header))
        || !read_header(im, "INFO", &header)
        || (header.len < (sizeof(header) + sizeof(info)))
        || !read_block(im, &info, sizeof(info))
        || !skip_fwd(im, header.len - sizeof(header) - sizeof(info)))
        return FALSE;

    if ((info.mincylinder != 0) || (info.minhead != 0) || (info.maxhead != 1))
        return FALSE;
    im->nr_tracks = min_t(uint16_t, (info.maxcylinder + 1) * 2, MAXTRK);

    /* HACK: Allocate half the (overly-large) read buffer for per-track 
     * chunk offset info. */
    im->ipf.tracks = (struct ipf_track *)((char *)im->buf + PF_BUFSZ);
    memset(im->ipf.tracks, 0, 2048);
    im->ipf.blks = (struct ipf_blkinf *)((char *)im->ipf.tracks
                                         + MAXTRK*sizeof(struct ipf_track));

    /* Find start and step-size of IMGE "array" within the container file. */
    while (read_header(im, NULL, &header)) {
        off = f_tell(&im->fp) - sizeof(header);
        if (!strncmp((char *)header.id, "IMGE", 4)) {
            struct ipf_img imge;
            if (!read_block(im, &imge, sizeof(imge)))
                return FALSE;
            trk = imge.cylinder*2 + imge.head;
            if (trk < MAXTRK) {
                im->ipf.tracks[trk].imge_off = off;
                im->ipf.tracks[trk].data_off = imge.dat_chunk;
            }
        } else if (!strncmp((char *)header.id, "DATA", 4)) {
            struct ipf_data data;
            if (!read_block(im, &data, sizeof(data)))
                return FALSE;
            _trk = trk;
            do {
                trk = (trk+1)&255;
                if (im->ipf.tracks[trk].data_off == data.dat_chunk) {
                    im->ipf.tracks[trk].data_off = off;
                    break;
                }
            } while (trk != _trk);
            header.len += data.size;
        }
        if (!skip_fwd(im, header.len - (f_tell(&im->fp) - off)))
            return FALSE;
    }

    return TRUE;
}

static bool_t ipf_seek_track(struct image *im, uint8_t track,
                             stk_time_t *ptime_after_index)
{
    struct ipf_header header;
    struct ipf_img imge;
    struct ipf_data data;
    struct ipf_block block;
    uint32_t pos;

    /* TODO: Fake out unformatted tracks. */
    track = min_t(uint8_t, track, im->nr_tracks-1);

    pos = im->ipf.tracks[track].imge_off;
    if (!pos
        || (im->fr = f_lseek(&im->fp, pos))
        || !read_header(im, "IMGE", &header)
        || !read_block(im, &imge, sizeof(imge)))
        return FALSE;

    if (imge.blkcnt > 32)
        return FALSE;

    pos = im->ipf.tracks[track].data_off;
    if (!pos
        || (im->fr = f_lseek(&im->fp, pos))
        || !read_header(im, "DATA", &header)
        || !read_block(im, &data, sizeof(data))
        || !read_block(im, &block, sizeof(block)))
        return FALSE;

    /* DATA, then N * BLK, then Gap streams, then Data streams. */
    im->ipf.trk_off = pos + block.dataoffset; /* Blk0 data stream */
    im->ipf.trk_len = data.size - im->ipf.trk_off; /* End of Blk[N-1] stream */
    im->ipf.raw_cons = 512;
    im->tracklen_bc = imge.databits + imge.gapbits;
    im->ipf.ticks_per_cell = ((sysclk_ms(DRIVE_MS_PER_REV) * 16u)
                              / im->tracklen_bc);
    im->ticks_since_flux = 0;
    im->cur_track = track;

    im->cur_bc = imge.startbit;
    im->cur_ticks = im->cur_bc * im->ipf.ticks_per_cell;

    im->prod = im->cons = 0;
    image_prefetch_data(im);

    im->ipf.blk = 0;
    im->ipf.mode = modeData;
    im->ipf.chk_todo = 0;

    return TRUE;
}

static void ipf_prefetch_data(struct image *im)
{
    UINT _nr, nr;
    uint8_t *buf = (uint8_t *)im->buf;

    if ((uint32_t)(im->prod - im->cons) > (PF_BUFSZ-512))
        return;

    f_lseek(&im->fp, im->ipf.trk_off + im->ipf.pf_pos);
    nr = min_t(UINT, 512, im->ipf.trk_len - im->ipf.pf_pos);
    f_read(&im->fp, &buf[im->prod % PF_BUFSZ], nr, &_nr);
    ASSERT(nr == _nr);
    im->prod += nr;
    im->ipf.pf_pos += nr;
    if (im->ipf.pf_pos >= im->ipf.trk_len)
        im->ipf.pf_pos = 0;
}

static uint16_t ipf_load_flux(struct image *im, uint16_t *tbuf, uint16_t nr)
{
    uint32_t ticks = im->ticks_since_flux;
    uint32_t ticks_per_cell = im->ipf.ticks_per_cell;
    uint32_t x, y = 32, todo = nr;

    for (;;) {
        /* Convert pre-generated raw bitcells into flux timings. */
        while (im->ipf.raw_cons != 512) {
            y = im->ipf.raw_cons % 32;
            x = im->ipf.raw[im->ipf.raw_cons/32] << y;
            im->ipf.raw_cons += 32 - y;
            im->cur_bc += 32 - y;
            im->cur_ticks += (32 - y) * ticks_per_cell;
            while (y < 32) {
                y++;
                ticks += ticks_per_cell;
                if ((int32_t)x < 0) {
                    *tbuf++ = (ticks >> 4) - 1;
                    ticks &= 15;
                    if (!--todo)
                        goto out;
                }
                x <<= 1;
            }
        }

        ASSERT(y == 32);
        if (im->cur_bc >= im->tracklen_bc) {
            im->cur_bc -= im->tracklen_bc;
            im->cur_ticks = im->cur_bc * im->ipf.ticks_per_cell;
        }

        /* We need more bitcells: ensure we have buffered data to convert. */
        if ((im->prod - im->cons) < 128)
            goto out;
        im->ipf.raw_cons = 0;

        /* Generate raw bitcells in a small holding buffer. */
        switch (im->ipf.mode) {
        case modeData: {
            uint8_t blen;
            if (!im->ipf.chk_todo) {
                im->ipf.code = im->buf[im->cons++ % PF_BUFSZ];
                blen = im->ipf.code >> 4;
                im->ipf.code &= 0xf;
                while (blen--) {
                    im->ipf.chk_todo <<= 8;
                    im->ipf.chk_todo |= im->buf[im->cons++ % PF_BUFSZ];
                }
                im->ipf.chk_len = im->ipf.chk_todo;
            }
            switch (im->ipf.code) {
            case chkSync: /* raw bits */
            case chkRaw:
                dat = im->buf[im->cons++];
                while (todo--) {
                    x <<= 1;
                    x |= !!(dat & 0x80);
                    dat <<= 1;
                    if (++raw_prod & 31 == 0) {
                        im->ipf.raw[raw_prod/32-1] = x;
                        if (raw_prod == ARRAY_SIZE(im->ipf.raw))
                            goto done;
                    }
                }
                break;
            case chkData: /* data bits; generate clock bits */
            case chkGap:
                dat = im->buf[im->cons++];
                while (todo--) {
                    x <<= 1;
                    if (clk) {
                        x |= !((dat & 0x80) || (x & 2));
                    } else {
                        x |= !!(dat & 0x80);
                        dat <<= 1;
                    }
                    clk ^= 1;
                    if (++raw_prod & 31 == 0) {
                        im->ipf.raw[raw_prod/32-1] = x;
                        if (raw_prod == ARRAY_SIZE(im->ipf.raw))
                            goto done;
                    }
                }
                break;
            case chkFlaky: /* no data; random bits */
                dat = 0x19;
                while (todo--) {
                    x <<= 1;
                    x |= !!(dat & 0x80);
                    dat <<= 1;
                    if (++raw_prod & 31 == 0) {
                        im->ipf.raw[raw_prod/32-1] = x;
                        if (raw_prod == ARRAY_SIZE(im->ipf.raw))
                            goto done;
                    }
                }
                break;
            case chkEnd:
                im->ipf.mode = modeGap;
                im->ipf.chk_todo = im->ipf.chk_len =
                    im->ipf.blks[im->ipf.blk].gap_bits;
                break;
            }
            break;
        }
        case modeGap:
            if (!im->ipf.chk_todo) {
                im->ipf.mode = modeData;
                im->ipf.blk = (im->ipf.blk + 1) % im->ipf.blkcnt;
                break;
            }
            break;
        }
    }

out:
    im->ipf.raw_cons -= 32 - y;
    im->cur_bc -= 32 - y;
    if (im->cur_bc >= im->tracklen_bc) {
        im->cur_bc -= im->tracklen_bc;
        im->cur_ticks = im->cur_bc * im->ipf.ticks_per_cell;
    } else {
        im->cur_ticks -= (32 - y) * ticks_per_cell;
    }
    im->ticks_since_flux = ticks;
    return nr - todo;
}

struct image_handler ipf_image_handler = {
    .open = ipf_open,
    .seek_track = ipf_seek_track,
    .prefetch_data = ipf_prefetch_data,
    .load_flux = ipf_load_flux
};

/*
 * Local variables:
 * mode: C
 * c-file-style: "Linux"
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 */
