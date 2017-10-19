/*
 * hfe.c
 * 
 * HxC Floppy Emulator (HFE) image files.
 * 
 * Written & released by Keir Fraser <keir.xen@gmail.com>
 * 
 * This is free and unencumbered software released into the public domain.
 * See the file COPYING for more details, or visit <http://unlicense.org>.
 */

/* NB. Fields are little endian. */
struct disk_header {
    char sig[8];
    uint8_t formatrevision;
    uint8_t nr_tracks, nr_sides;
    uint8_t track_encoding;
    uint16_t bitrate; /* kB/s, approx */
    uint16_t rpm; /* unused, can be zero */
    uint8_t interface_mode;
    uint8_t rsvd; /* set to 1? */
    uint16_t track_list_offset;
    /* from here can write 0xff to end of block... */
    uint8_t write_allowed;
    uint8_t single_step;
    uint8_t t0s0_altencoding, t0s0_encoding;
    uint8_t t0s1_altencoding, t0s1_encoding;
};

/* track_encoding */
enum {
    ENC_ISOIBM_MFM,
    ENC_Amiga_MFM,
    ENC_ISOIBM_FM,
    ENC_Emu_FM,
    ENC_Unknown = 0xff
};

/* interface_mode */
enum {
    IFM_IBMPC_DD,
    IFM_IBMPC_HD,
    IFM_AtariST_DD,
    IFM_AtariST_HD,
    IFM_Amiga_DD,
    IFM_Amiga_HD,
    IFM_CPC_DD,
    IFM_GenericShugart_DD,
    IFM_IBMPC_ED,
    IFM_MSX2_DD,
    IFM_C64_DD,
    IFM_EmuShugart_DD,
    IFM_S950_DD,
    IFM_S950_HD,
    IFM_Disable = 0xfe
};

struct track_header {
    uint16_t offset;
    uint16_t len;
};

/* HFEv3 opcodes */
enum {
    OP_nop = 0,     /* no effect */
    OP_index = 8,   /* index mark */
    OP_bitrate = 4, /* +1byte: new bitrate */
    OP_skip = 12    /* +1byte: skip 0-8 bits in next byte */
};

static bool_t hfe_open(struct image *im)
{
    struct image_buf *mfm = im->hfe.read_mfm;
    struct disk_header dhdr;

    F_read(&im->fp, &dhdr, sizeof(dhdr), NULL);
    if (dhdr.formatrevision != 0)
        return FALSE;
    if (!strncmp(dhdr.sig, "HXCHFEV3", sizeof(dhdr.sig))) {
        im->hfe.is_v3 = TRUE;
    } else if (!strncmp(dhdr.sig, "HXCPICFE", sizeof(dhdr.sig))) {
        im->hfe.is_v3 = FALSE;
    } else {
        return FALSE;
    }

    im->hfe.tlut_base = le16toh(dhdr.track_list_offset);
    im->nr_cyls = dhdr.nr_tracks;
    im->nr_sides = dhdr.nr_sides;
    im->write_bc_ticks = sysclk_us(500) / le16toh(dhdr.bitrate);

    /* Set up the split side0/side1 read mfm buffers. */
    mfm[0] = im->bufs.read_mfm;
    mfm[0].len /= 2;
    mfm[1] = mfm[0];
    mfm[1].p = (char *)mfm[1].p + mfm[0].len;

    return TRUE;
}

static bool_t hfe_seek_track(
    struct image *im, uint16_t track, stk_time_t *start_pos)
{
    struct image_buf *rd = &im->bufs.read_data;
    struct image_buf *mfm = im->hfe.read_mfm;
    uint32_t sys_ticks = start_pos ? *start_pos : 0;
    struct track_header thdr;
    uint8_t cyl = track/2, side = track&1;

    /* TODO: Fake out unformatted tracks. */
    cyl = min_t(uint8_t, cyl, im->nr_cyls-1);
    side = min_t(uint8_t, side, im->nr_sides-1);
    track = cyl*2 + side;

    F_lseek(&im->fp, im->hfe.tlut_base*512 + (track/2)*4);
    F_read(&im->fp, &thdr, sizeof(thdr), NULL);

    im->hfe.trk_off = le16toh(thdr.offset);
    im->hfe.trk_len = le16toh(thdr.len) / 2;
    im->tracklen_bc = im->hfe.trk_len * 8;
    im->hfe.ticks_per_cell = ((sysclk_ms(DRIVE_MS_PER_REV) * 16u)
                              / im->tracklen_bc);
    im->ticks_since_flux = 0;
    im->cur_track = track;

    /* HFEv3: the track data length is not the same as track bit length as 
     * it contains opcode metadata in the stream. Instead use a sensible 
     * default (based on the kB/s bitrate in the image header) and expect this 
     * to be updated via the opcode stream as necessary. */
    if (im->hfe.is_v3)
        im->hfe.ticks_per_cell = im->write_bc_ticks * 16;

    im->cur_bc = (sys_ticks * 16) / im->hfe.ticks_per_cell;
    if (im->cur_bc >= im->tracklen_bc)
        im->cur_bc = 0;
    im->cur_ticks = im->cur_bc * im->hfe.ticks_per_cell;

    sys_ticks = im->cur_ticks / 16;

    im->hfe.trk_pos = (im->cur_bc/8) & ~255;
    rd->prod = rd->cons = 0;
    mfm[0].cons = mfm[1].cons = mfm[0].prod = mfm[1].prod = 0;

    if (start_pos) {
        image_read_track(im);
        mfm[0].cons = mfm[1].cons = im->cur_bc & 2047;
        *start_pos = sys_ticks;
    }

    return FALSE;
}

static bool_t hfe_read_track(struct image *im)
{
    struct image_buf *rd = &im->bufs.read_data;
    struct image_buf *mfm = im->hfe.read_mfm;
    uint8_t *buf = rd->p;
    unsigned int buflen = (rd->len-256) & ~255;
    unsigned int side, mfmlen, mfmp, mfmc;
    uint32_t mfm_curr;

    now_pos <<= 4;
    if (now_pos > im->cur_ticks)
        XXX;

    mfm_curr = mfm->cons -
        (im->cur_ticks - now_pos) / im->hfe.ticks_per_cell;

    if (rd->prod == rd->cons) {
        F_lseek(&im->fp, im->hfe.trk_off * 512 + im->hfe.trk_pos * 2);
        F_read(&im->fp, &buf[(rd->prod/8) % buflen], 512, NULL);
        rd->prod += 256 * 8;
        im->hfe.trk_pos += 256;
        if (im->hfe.trk_pos >= im->hfe.trk_len)
            im->hfe.trk_pos = 0;
    }

    mfmp = mfm->prod / 8;
    mfmc = mfm_curr / 8;
    mfmlen = mfm->len;

    if (mfmlen - (mfmp - mfmc) < 256)
        return FALSE;

    for (side = 0; side < 2; side++) {
        unsigned int done, bytes = 0;
        for (done = 0; done < 256; done -= bytes) {
            bytes = min_t(unsigned int, 256, mfmlen - mfmp);
            memcpy((char *)mfm[side].p + ((mfmp + done) % mfmlen),
                   buf + (rd->cons/8) % buflen + side*256 + done,
                   bytes);
        }
        mfm[side].prod += 256 * 8;
    }

    rd->cons += 256 * 8;

    return TRUE;
}

static uint16_t hfe_rdata_flux(struct image *im, uint16_t *tbuf, uint16_t nr)
{
    struct image_buf *mfm = &im->hfe.read_mfm[im->cur_track & 1];
    uint32_t ticks = im->ticks_since_flux;
    uint32_t ticks_per_cell = im->hfe.ticks_per_cell;
    uint32_t y = 8, todo = nr;
    uint8_t x, *mfmb = mfm->p;
    unsigned int mfmlen = mfm->len;
    bool_t is_v3 = im->hfe.is_v3;

    while ((mfm->prod - mfm->cons) >= 3*8) {
        ASSERT(y == 8);
        if (im->cur_bc >= im->tracklen_bc) {
            ASSERT(im->cur_bc == im->tracklen_bc);
            im->tracklen_ticks = im->cur_ticks;
            im->cur_bc = im->cur_ticks = 0;
            /* Skip tail of current 256-byte block. */
            mfm->cons = (mfm->cons + 256*8-1) & ~(256*8-1);
            continue;
        }
        y = mfm->cons % 8;
        x = mfmb[(mfm->cons/8) % mfmlen] >> y;
        if (is_v3 && (y == 0) && ((x & 0xf) == 0xf)) {
            /* V3 byte-aligned opcode processing. */
            switch (x >> 4) {
            case OP_nop:
            case OP_index:
                mfm->cons += 8;
                im->cur_bc += 8;
                y = 8;
                continue;
            case OP_bitrate:
                x = _rbit32(mfmb[(mfm->cons/8+1) % mfmlen]) >> 24;
                im->hfe.ticks_per_cell = ticks_per_cell = 
                    (sysclk_us(2) * 16 * x) / 72;
                mfm->cons += 2*8;
                im->cur_bc += 2*8;
                y = 8;
                continue;
            case OP_skip:
                x = (_rbit32(mfmb[(mfm->cons/8+1) % mfmlen]) >> 24) & 7;
                mfm->cons += 2*8 + x;
                im->cur_bc += 2*8 + x;
                y = mfm->cons % 8;
                x = mfmb[(mfm->cons/8) % mfmlen] >> y;
                break;
            default:
                /* ignore and process as normal data */
                break;
            }
        }
        mfm->cons += 8 - y;
        im->cur_bc += 8 - y;
        im->cur_ticks += (8 - y) * ticks_per_cell;
        while (y < 8) {
            y++;
            ticks += ticks_per_cell;
            if (x & 1) {
                *tbuf++ = (ticks >> 4) - 1;
                ticks &= 15;
                if (!--todo)
                    goto out;
            }
            x >>= 1;
        }
    }

out:
    mfm->cons -= 8 - y;
    im->cur_bc -= 8 - y;
    im->cur_ticks -= (8 - y) * ticks_per_cell;
    im->ticks_since_flux = ticks;
    return nr - todo;
}

static void hfe_write_track(struct image *im, bool_t flush)
{
    struct image_buf *wr = &im->bufs.write_mfm;
    uint8_t *buf = wr->p;
    unsigned int buflen = wr->len;
    uint8_t *w, *wrbuf = im->bufs.write_data.p;
    uint32_t base = (im->write_start*(16/8)) / im->hfe.ticks_per_cell;
    uint32_t i, c = wr->cons / 8, p = wr->prod / 8;
    stk_time_t t;

    /* Even when we can buffer the whole track in memory, it still performs
     * better to then write out sectors as they're dirtied. Otherwise we
     * suffer 30ms+ of latency as the track buffer is written out. */
    const bool_t write_whole_track = 0;

    /* Round-to-nearest the producer index if we are processing final data. */
    if (flush && (wr->prod & 4))
        p++;

    if (!im->bufs.write_data.prod) {
        /* How many bytes is the full track data? */
        im->bufs.write_data.prod = ((im->hfe.trk_len * 2) + 511) & ~511;
        if (im->bufs.write_data.prod > im->bufs.write_data.len) {
            /* It doesn't fit in our staging buffer. Write block at a time. */
            im->bufs.write_data.prod = 256;
        } else {
            /* Whole track fits in our buffer! Stream it in immediately. */
            t = stk_now();
            printk("Read whole track %u... ", im->cur_track);
            F_lseek(&im->fp, im->hfe.trk_off * 512);
            F_read(&im->fp, wrbuf, im->bufs.write_data.prod, NULL);
            F_lseek(&im->fp, im->hfe.trk_off * 512);
            printk("%u us\n", stk_diff(t, stk_now()) / STK_MHZ);
        }
    }

    for (;;) {

        uint32_t off = (c + base) % im->hfe.trk_len;
        UINT nr;

        /* All bytes remaining in the MFM buffer. */
        nr = p - c;
        /* Limit to end of current 256-byte HFE block. */
        nr = min_t(UINT, nr, 256 - (off & 255));
        /* Limit to end of HFE track. */
        nr = min_t(UINT, nr, im->hfe.trk_len - off);

        /* Bail if no bytes to write, or if we could batch some more. */
        if ((nr == 0) || ((nr == (p - c)) && !flush))
            break;

        if (im->bufs.write_data.prod == 256) {

            /* Encode into a 256-byte area in our staging buffer. */
            w = wrbuf;
            for (i = 0; i < nr; i++)
                *w++ = _rbit32(buf[c++ % buflen]) >> 24;

            /* Write it back to mass storage straight away. */
            t = stk_now();
            printk("Write %u-%u (%u)... ", off, off+nr-1, nr);
            F_lseek(&im->fp,
                    im->hfe.trk_off * 512
                    + (im->cur_track & 1) * 256
                    + ((off & ~255) << 1) + (off & 255));
            F_write(&im->fp, wrbuf, nr, NULL);
            printk("%u us\n", stk_diff(t, stk_now()) / STK_MHZ);

        } else {

            /* Encode into the whole-track buffer for later write-out. */
            w = wrbuf
                + (im->cur_track & 1) * 256
                + ((off & ~255) << 1) + (off & 255);
            for (i = 0; i < nr; i++)
                *w++ = _rbit32(buf[c++ % buflen]) >> 24;

            if (!write_whole_track) {
                w = wrbuf + ((off & ~255) << 1);
                t = stk_now();
                printk("Write %u-%u (%u)... ", off, off+nr-1, nr);
                F_lseek(&im->fp, im->hfe.trk_off * 512 + ((off & ~255) << 1));
                F_write(&im->fp, w, 512, NULL);
                printk("%u us\n", stk_diff(t, stk_now()) / STK_MHZ);
            }
        }
    }

    wr->cons = c * 8;

    if (flush && write_whole_track) {
        /* Whole track mode: flush dirty buffer in one go. */
        t = stk_now();
        printk("Write whole track %u... ", im->cur_track);
        F_write(&im->fp, wrbuf, im->bufs.write_data.prod, NULL);
        printk("%u us\n", stk_diff(t, stk_now()) / STK_MHZ);
    }
}

const struct image_handler hfe_image_handler = {
    .open = hfe_open,
    .seek_track = hfe_seek_track,
    .read_track = hfe_read_track,
    .rdata_flux = hfe_rdata_flux,
    .write_track = hfe_write_track,
    .syncword = 0xffffffff
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
