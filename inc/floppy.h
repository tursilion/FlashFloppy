/*
 * floppy.h
 * 
 * Floppy interface control and image management.
 * 
 * Written & released by Keir Fraser <keir.xen@gmail.com>
 * 
 * This is free and unencumbered software released into the public domain.
 * See the file COPYING for more details, or visit <http://unlicense.org>.
 */

#define DRIVE_RPM 300u
#define DRIVE_MS_PER_REV (60000u/DRIVE_RPM)
#define DRIVE_SETTLE_MS 12

#define FINTF_SHUGART 0
#define FINTF_IBMPC   1

struct image_buf {
    void *p;
    uint32_t len;
    uint32_t prod, cons;
};

struct adf_image {
    uint32_t trk_off;
    uint16_t trk_pos, trk_len;
    uint32_t mfm[16], mfm_cons;
};

struct hfe_image {
    uint16_t tlut_base;
    uint16_t trk_off;
    uint16_t trk_pos, trk_len;
    uint32_t ticks_per_cell;
    bool_t is_v3;
    struct image_buf read_mfm[2];
};

struct img_image {
    uint32_t trk_off;
    uint16_t trk_pos, trk_len;
    int32_t decode_pos;
    bool_t has_iam;
    uint8_t gap3;
    int8_t write_sector;
    uint8_t sec_base, sec_map[64];
    uint8_t nr_sectors;
    uint16_t data_rate, gap4;
    uint32_t ticks_per_cell;
    uint32_t idx_sz, idam_sz, dam_sz;
};

struct directaccess {
    uint32_t lba;
};

struct image_bufs {
    /* Buffering for MFM being written to disk. */
    struct image_buf write_mfm;
    /* Buffering for MFM we generate from read_data. */
    struct image_buf read_mfm;
    /* Staging area for writeout to mass storage. */
    struct image_buf write_data;
    /* Read buffer for track data to be used for generating flux pattern. */
    struct image_buf read_data;
};

struct image {
    const struct image_handler *handler;
    const struct image_handler *_handler;

    /* FatFS. */
    FIL fp;

    /* Info about image as a whole. */
    uint8_t nr_cyls, nr_sides;

    /* Data buffers. */
    struct image_bufs bufs;

    /* Info about current track. */
    uint16_t cur_track;
    uint16_t write_bc_ticks; /* Nr systicks per bitcell in write stream */
    uint32_t tracklen_bc, cur_bc; /* Track length and cursor, in bitcells */
    uint32_t tracklen_ticks; /* Timing of previous revolution, in 'ticks' */
    uint32_t cur_ticks; /* Offset from index, in 'ticks' */
    uint32_t ticks_since_flux; /* Ticks since last flux sample/reversal */
    uint32_t write_start; /* Ticks past index when current write started */
    uint32_t write_mfm_window; /* Sliding window at head of MFM write stream */

    struct directaccess da;

    union {
        struct adf_image adf;
        struct hfe_image hfe;
        struct img_image img;
    };
};

struct image_handler {
    bool_t (*open)(struct image *im);
    bool_t (*seek_track)(
        struct image *im, uint16_t track, stk_time_t *start_pos);
    bool_t (*read_track)(struct image *im);
    uint16_t (*rdata_flux)(struct image *im, uint16_t *tbuf, uint16_t nr);
    void (*write_track)(struct image *im, bool_t flush);
    uint32_t syncword;
};

/* Is given file valid to open as an image? */
bool_t image_valid(FILINFO *fp);

/* Open specified image file on mass storage device. */
bool_t image_open(struct image *im, const struct v2_slot *slot);

/* Seek to given track and start reading track data at specified rotational
 * position (specified as number of SYSCLK ticks past the index mark).
 * 
 * If start_pos is NULL then the caller is in write mode and thus is not
 * interested in fetching data from a particular rotational position.
 * 
 * Returns TRUE if the config file needs to be re-read (exiting D-A mode). */
bool_t image_seek_track(
    struct image *im, uint16_t track, stk_time_t *start_pos);

/* Read track data into memory. Returns TRUE if any new data was read. */
bool_t image_read_track(struct image *im);

/* Generate flux timings for the RDATA timer and output pin. */
uint16_t image_rdata_flux(struct image *im, uint16_t *tbuf, uint16_t nr);

/* Write track data from memory to mass storage. If flush is TRUE then all 
 * remaining data must be written to mass storage. */
void image_write_track(struct image *im, bool_t flush);

/* Rotational position of last-generated flux (SYSCLK ticks past index). */
uint32_t image_ticks_since_index(struct image *im);

/* MFM conversion. */
extern const uint16_t mfmtab[];
static inline uint16_t bintomfm(uint8_t x) { return mfmtab[x]; }
uint8_t mfmtobin(uint16_t x);

/* External API. */
void floppy_init(uint8_t fintf_mode);
void floppy_insert(unsigned int unit, struct v2_slot *slot);
void floppy_cancel(void);
bool_t floppy_handle(void); /* TRUE -> re-read config file */
void floppy_set_cyl(uint8_t unit, uint8_t cyl);
void floppy_get_track(uint8_t *p_cyl, uint8_t *p_side, uint8_t *p_sel);
void floppy_set_fintf_mode(uint8_t fintf_mode);

/*
 * Local variables:
 * mode: C
 * c-file-style: "Linux"
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 */
