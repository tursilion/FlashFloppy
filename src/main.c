/*
 * main.c
 * 
 * Bootstrap the STM32F103C8T6 and get things moving.
 * 
 * Written & released by Keir Fraser <keir.xen@gmail.com>
 * 
 * This is free and unencumbered software released into the public domain.
 * See the file COPYING for more details, or visit <http://unlicense.org>.
 */

int EXC_reset(void) __attribute__((alias("main")));

static FATFS fatfs;
static struct {
    FIL file;
    DIR dp;
    FILINFO fp;
    char buf[512];
} *fs;

static struct {
    uint16_t slot_nr, max_slot_nr;
    uint8_t slot_map[1000/8];
    struct v2_slot autoboot, hxcsdfe, slot;
    uint32_t cfg_cdir, cur_cdir;
    uint16_t cdir_stack[20];
    uint8_t depth;
    uint8_t hxc_mode:1;
    uint8_t ejected:1;
    uint8_t ima_ej_flag:1; /* "\\EJ" flag in IMAGE_A.CFG? */
    /* FF.CFG values which override HXCSDFE.CFG. */
    uint8_t ffcfg_has_step_volume:1;
    uint8_t ffcfg_has_display_off_secs:1;
    uint8_t ffcfg_has_display_scroll_rate:1;
} cfg;

/* FF.CFG: Compiled default values. */
const struct ff_cfg dfl_ff_cfg = {
    .version = FFCFG_VERSION,
    .size = sizeof(struct ff_cfg),
#define x(n,o,v) .o = v,
#include "ff_cfg_defaults.h"
#undef x
};

/* FF.CFG: User-specified values, and defaults where not specified. */
struct ff_cfg ff_cfg;

uint8_t board_id;

#define LCD_SCROLL_PAUSE_MSEC  2000

static uint32_t backlight_ticks;
static uint8_t backlight_state;
#define BACKLIGHT_OFF          0
#define BACKLIGHT_SWITCHING_ON 1
#define BACKLIGHT_ON           2

/* Turn the LCD backlight on, reset the switch-off handler and ticker. */
static void lcd_on(void)
{
    if (display_mode != DM_LCD_1602)
        return;
    backlight_ticks = 0;
    barrier();
    backlight_state = BACKLIGHT_ON;
    barrier();
    lcd_backlight(ff_cfg.display_off_secs != 0);
}

static bool_t slot_type(const char *str)
{
    char ext[4];
    filename_extension(cfg.slot.name, ext, sizeof(ext));
    if (!strcmp(ext, str))
        return TRUE;
    memcpy(ext, cfg.slot.type, 3);
    ext[3] = '\0';
    return !strcmp(ext, str);
}

/* Write slot info to display. */
static void display_write_slot(void)
{
    char msg[17], *type;
    if (display_mode != DM_LCD_1602) {
        if (display_mode == DM_LED_7SEG)
            led_7seg_write_decimal(cfg.slot_nr);
        return;
    }
    snprintf(msg, sizeof(msg), "%s", cfg.slot.name);
    lcd_write(0, 0, 16, msg);
    type = (cfg.slot.attributes & AM_DIR) ? "DIR"
        : slot_type("adf") ? "ADF"
        : slot_type("hfe") ? "HFE"
        : slot_type("img") ? "IMG"
        : slot_type("ima") ? "IMA"
        : slot_type("st") ? "ST "
        : "UNK";
    if (cfg.hxc_mode) {
        snprintf(msg, sizeof(msg), "%03u/%03u %s",
                 cfg.slot_nr, cfg.max_slot_nr, type);
    } else {
        snprintf(msg, sizeof(msg), "%03u/%03u %s D:%u",
                 cfg.slot_nr, cfg.max_slot_nr, type, cfg.depth);
    }
    lcd_write(0, 1, 16, msg);
    lcd_on();
}

/* Write track number to LCD. */
static uint8_t lcd_cyl, lcd_side;
static int32_t lcd_update_ticks;
static void lcd_write_track_info(bool_t force)
{
    uint8_t cyl, side, sel;
    char msg[17];
    if (display_mode != DM_LCD_1602)
        return;
    floppy_get_track(&cyl, &side, &sel);
    cyl = min_t(uint8_t, cyl, 99);
    side = min_t(uint8_t, side, 1);
    if (force || (cyl != lcd_cyl) || ((side != lcd_side) && sel)) {
        snprintf(msg, sizeof(msg), "T:%02u S:%u", cyl, side);
        lcd_write(8, 1, 0, msg);
        if (ff_cfg.display_on_activity)
            lcd_on();
        lcd_cyl = cyl;
        lcd_side = side;
    }
}

/* Scroll long filename within 16-character window. */
static uint8_t lcd_scroll_off, lcd_scroll_end;
static int32_t lcd_scroll_ticks;
static void lcd_scroll_name(void)
{
    char msg[17];
    if (lcd_scroll_ticks > 0)
        return;
    if (++lcd_scroll_off > lcd_scroll_end)
        lcd_scroll_off = 0;
    snprintf(msg, sizeof(msg), "%s", cfg.slot.name + lcd_scroll_off);
    lcd_write(0, 0, 16, msg);
    lcd_scroll_ticks =
        ((lcd_scroll_off == 0)
         || (lcd_scroll_off == lcd_scroll_end))
        ? stk_ms(LCD_SCROLL_PAUSE_MSEC) : stk_ms(ff_cfg.display_scroll_rate);
}

/* Handle switching the LCD backlight. */
static uint8_t lcd_handle_backlight(uint8_t b)
{
    if ((display_mode != DM_LCD_1602)
        || (ff_cfg.display_off_secs == 0)
        || (ff_cfg.display_off_secs == 0xff))
        return b;

    switch (backlight_state) {
    case BACKLIGHT_OFF:
        if (!b)
            break;
        /* First button press is to turn on the backlight. Nothing more. */
        b = 0;
        backlight_state = BACKLIGHT_SWITCHING_ON;
        lcd_backlight(TRUE);
        break;
    case BACKLIGHT_SWITCHING_ON:
        /* We sit in this state until the button is released. */
        if (!b)
            backlight_state = BACKLIGHT_ON;
        b = 0;
        backlight_ticks = 0;
        break;
    case BACKLIGHT_ON:
        /* After a period with no button activity we turn the backlight off. */
        if (b)
            backlight_ticks = 0;
        if (backlight_ticks++ >= 200*ff_cfg.display_off_secs) {
            lcd_backlight(FALSE);
            backlight_state = BACKLIGHT_OFF;
        }
        break;
    }

    return b;
}

static struct timer button_timer;
static volatile uint8_t buttons;
#define B_LEFT 1
#define B_RIGHT 2
#define B_SELECT 4
static void button_timer_fn(void *unused)
{
    static uint16_t bl, br, bs;
    static uint8_t rotary;
    uint8_t b = 0;

    /* We debounce the switches by waiting for them to be pressed continuously 
     * for 16 consecutive sample periods (16 * 5ms == 80ms) */

    bl <<= 1;
    bl |= gpio_read_pin(gpioc, 8);
    if (bl == 0)
        b |= B_LEFT;

    br <<= 1;
    br |= gpio_read_pin(gpioc, 7);
    if (br == 0)
        b |= B_RIGHT;

    bs <<= 1;
    bs |= gpio_read_pin(gpioc, 6);
    if (bs == 0)
        b |= B_SELECT;

    /* Rotary encoder: we look for a 1->0 edge (falling edge) on pin A. 
     * Pin B then tells us the direction (left or right). */
    rotary <<= 1;
    rotary |= gpio_read_pin(gpioc, 10);
    if ((rotary & 0x03) == 0x02)
        b |= (gpio_read_pin(gpioc, 11) == 0) ? B_LEFT : B_RIGHT;

    b = lcd_handle_backlight(b);

    /* Latch final button state and reset the timer. */
    buttons = b;
    timer_set(&button_timer, stk_add(button_timer.deadline, stk_ms(5)));
}

static void canary_init(void)
{
    _irq_stackbottom[0] = _thread_stackbottom[0] = 0xdeadbeef;
}

static void canary_check(void)
{
    ASSERT(_irq_stackbottom[0] == 0xdeadbeef);
    ASSERT(_thread_stackbottom[0] == 0xdeadbeef);
}

void fatfs_from_slot(FIL *file, const struct v2_slot *slot, BYTE mode)
{
    memset(file, 0, sizeof(*file));
    file->obj.fs = &fatfs;
    file->obj.id = fatfs.id;
    file->obj.attr = slot->attributes;
    file->obj.sclust = slot->firstCluster;
    file->obj.objsize = slot->size;
    file->flag = mode;
    /* WARNING: dir_ptr, dir_sect are unknown. */
}

static void fatfs_to_slot(struct v2_slot *slot, FIL *file, const char *name)
{
    const char *dot = strrchr(name, '.');
    unsigned int i;

    slot->attributes = file->obj.attr;
    slot->firstCluster = file->obj.sclust;
    slot->size = file->obj.objsize;
    snprintf(slot->name, sizeof(slot->name), "%s", name);
    memcpy(slot->type, dot+1, 3);
    for (i = 0; i < 3; i++)
        slot->type[i] = tolower(slot->type[i]);
}

static void dump_file(void)
{
    F_lseek(&fs->file, 0);
#ifndef NDEBUG
    printk("[");
    do {
        F_read(&fs->file, fs->buf, sizeof(fs->buf), NULL);
        printk("%s", fs->buf);
    } while (!f_eof(&fs->file));
    printk("]\n");
    F_lseek(&fs->file, 0);
#endif
}

static bool_t native_dir_next(void)
{
    do {
        F_readdir(&fs->dp, &fs->fp);
        if (fs->fp.fname[0] == '\0')
            return FALSE;
        if ((fs->fp.fattrib & AM_DIR) && (display_mode == DM_LCD_1602)
            && ((cfg.depth != 0) || strcmp(fs->fp.fname, "FF")))
            break;
    } while (!image_valid(&fs->fp));
    return TRUE;
}

static void read_ff_cfg(void)
{
    enum {
#define x(n,o,v) FFCFG_##o,
#include "ff_cfg_defaults.h"
#undef x
        FFCFG_nr
    };

    const static struct opt ff_cfg_opts[FFCFG_nr+1] = {
#define x(n,o,v) [FFCFG_##o] = { #n },
#include "ff_cfg_defaults.h"
#undef x
    };

    FRESULT fr;
    int option;
    struct opts opts = {
        .file = &fs->file,
        .opts = ff_cfg_opts,
        .arg = fs->buf,
        .argmax = sizeof(fs->buf)-1
    };

    fatfs.cdir = cfg.cfg_cdir;
    fr = F_try_open(&fs->file, "FF.CFG", FA_READ);
    if (fr)
        return;

    while ((option = get_next_opt(&opts)) != -1) {

        switch (option) {

        case FFCFG_interface:
            ff_cfg.interface =
                !strcmp(opts.arg, "ibmpc") ? FINTF_IBMPC
                : !strcmp(opts.arg, "shugart") ? FINTF_SHUGART
                : FINTF_JC;
            break;

        case FFCFG_ejected_on_startup:
            ff_cfg.ejected_on_startup = !strcmp(opts.arg, "yes");
            break;

        case FFCFG_da_report_version:
            memset(ff_cfg.da_report_version, 0,
                   sizeof(ff_cfg.da_report_version));
            snprintf(ff_cfg.da_report_version,
                     sizeof(ff_cfg.da_report_version),
                     "%s", opts.arg);
            break;

        case FFCFG_autoselect_file_secs:
            ff_cfg.autoselect_file_secs = strtol(opts.arg, NULL, 10);
            break;

        case FFCFG_autoselect_folder_secs:
            ff_cfg.autoselect_folder_secs = strtol(opts.arg, NULL, 10);
            break;

        case FFCFG_nav_loop:
            ff_cfg.nav_loop = !strcmp(opts.arg, "yes");
            break;

        case FFCFG_display_off_secs:
            ff_cfg.display_off_secs = strtol(opts.arg, NULL, 10);
            cfg.ffcfg_has_display_off_secs = TRUE;
            break;

        case FFCFG_display_on_activity:
            ff_cfg.display_on_activity = !strcmp(opts.arg, "yes");
            break;

        case FFCFG_display_scroll_rate:
            ff_cfg.display_scroll_rate = strtol(opts.arg, NULL, 10);
            if (ff_cfg.display_scroll_rate < 100)
                ff_cfg.display_scroll_rate = 100;
            cfg.ffcfg_has_display_scroll_rate = TRUE;
            break;

        case FFCFG_oled_font:
            ff_cfg.oled_font = !strcmp(opts.arg, "7x16")
                ? FONT_7x16 : FONT_8x16;
            break;

        case FFCFG_step_volume: {
            int volume = strtol(opts.arg, NULL, 10);
            if (volume <= 0) volume = 0;
            if (volume >= 20) volume = 20;
            ff_cfg.step_volume = volume;
            cfg.ffcfg_has_step_volume = TRUE;
            break;

        case FFCFG_image_on_startup:
            ff_cfg.image_on_startup =
                !strcmp(opts.arg, "static") ? IMGS_static
                : !strcmp(opts.arg, "last") ? IMGS_last : IMGS_init;
            break;

        }
        }
    }

    F_close(&fs->file);

    flash_ff_cfg_update();
}

static void process_ff_cfg_opts(void)
{
    /* interface: Inform the floppy subsystem. */
    if (ff_cfg.interface != FINTF_JC)
        floppy_set_fintf_mode(ff_cfg.interface);

    /* ejected-on-startup: Set the ejected state appropriately. */
    if (ff_cfg.ejected_on_startup)
        cfg.ejected = TRUE;
}

static void cfg_init(void)
{
    struct hxcsdfe_cfg hxc_cfg;
    unsigned int sofar;
    char *p;
    BYTE mode;
    FRESULT fr;

    cfg.hxc_mode = FALSE;
    cfg.ima_ej_flag = FALSE;
    cfg.slot_nr = cfg.depth = 0;
    cfg.cur_cdir = fatfs.cdir;

    fr = f_chdir("FF");
    cfg.cfg_cdir = fatfs.cdir;

    read_ff_cfg();
    process_ff_cfg_opts();

    /* Probe for HxC compatibility mode. */
    fatfs.cdir = cfg.cur_cdir;
    fr = F_try_open(&fs->file, "HXCSDFE.CFG", FA_READ|FA_WRITE);
    if (fr)
        goto native_mode;
    fatfs_to_slot(&cfg.hxcsdfe, &fs->file, "HXCSDFE.CFG");
    F_read(&fs->file, &hxc_cfg, sizeof(hxc_cfg), NULL);
    if (hxc_cfg.startup_mode & HXCSTARTUP_slot0) {
        /* Startup mode: slot 0. */
        hxc_cfg.slot_index = hxc_cfg.cur_slot_number = 0;
        F_lseek(&fs->file, 0);
        F_write(&fs->file, &hxc_cfg, sizeof(hxc_cfg), NULL);
    }
    if (hxc_cfg.startup_mode & HXCSTARTUP_ejected) {
        /* Startup mode: eject. */
        cfg.ejected = TRUE;
    }
        
    F_close(&fs->file);

    /* Indexed mode (DSKAxxxx.HFE) does not need AUTOBOOT.HFE. */
    if (!strncmp("HXCFECFGV", hxc_cfg.signature, 9) && hxc_cfg.index_mode) {
        memset(&cfg.autoboot, 0, sizeof(cfg.autoboot));
        cfg.hxc_mode = TRUE;
        goto out;
    }

    fr = F_try_open(&fs->file, "AUTOBOOT.HFE", FA_READ);
    if (fr)
        goto native_mode;
    fatfs_to_slot(&cfg.autoboot, &fs->file, "AUTOBOOT.HFE");
    F_close(&fs->file);

    cfg.hxc_mode = TRUE;
    goto out;

native_mode:
    /* Native mode (direct navigation). */
    if (ff_cfg.image_on_startup == IMGS_init)
        goto out;

    fatfs.cdir = cfg.cfg_cdir;
    mode = FA_READ;
    if (ff_cfg.image_on_startup == IMGS_last)
        mode |= FA_WRITE | FA_OPEN_ALWAYS;
    fr = F_try_open(&fs->file, "IMAGE_A.CFG", mode);
    if (fr)
        goto out;

    /* Process IMAGE_A.CFG file. */
    sofar = 0; /* bytes consumed so far */
    fatfs.cdir = cfg.cur_cdir;
    for (;;) {
        /* Read next pathname section, search for its terminating slash. */
        F_read(&fs->file, fs->buf, sizeof(fs->buf), NULL);
        fs->buf[sizeof(fs->buf)-1] = '\0';
        for (p = fs->buf; *p && (*p != '/'); p++)
            continue;
        /* No terminating slash: we're done. */
        if ((p == fs->buf) || !*p)
            break;
        /* Terminate the name section, push curdir onto stack, then chdir. */
        *p++ = '\0';
        printk("%u:D: '%s'\n", cfg.depth, fs->buf);
        cfg.cdir_stack[cfg.depth++] = fatfs.cdir;
        fr = f_chdir(fs->buf);
        if (fr)
            goto clear_image_a;
        /* Seek on to next pathname section. */
        sofar += p - fs->buf;
        F_lseek(&fs->file, sofar);
    }
    if (cfg.depth != 0) {
        /* No subfolder support on LED display. */
        if (display_mode != DM_LCD_1602)
            goto clear_image_a; /* no subfolder support on LED display */
        /* Skip '..' entry. */
        cfg.slot_nr = 1;
    }
    while ((p != fs->buf) && isspace(p[-1]))
        *--p = '\0'; /* Strip trailing whitespace */
    if (((p - fs->buf) >= 3) && !strcmp(p-3, "\\EJ")) {
        /* Eject flag "\\EJ" is found. Act on it and then skip over it. */
        cfg.ejected = TRUE;
        cfg.ima_ej_flag = TRUE;
        p -= 3;
        *p = '\0';
    }
    if (p != fs->buf) {
        /* If there was a non-empty non-terminated pathname section, it 
         * must be the name of the currently-selected image file. */
        bool_t ok;
        printk("%u:F: '%s' %s\n", cfg.depth, fs->buf,
               cfg.ima_ej_flag ? "(EJ)" : "");
        F_opendir(&fs->dp, "");
        while ((ok = native_dir_next()) && strcmp(fs->fp.fname, fs->buf))
            cfg.slot_nr++;
        F_closedir(&fs->dp);
        if (!ok)
            goto clear_image_a;
    }
    F_close(&fs->file);
    cfg.cur_cdir = fatfs.cdir;

out:
    printk("Mode: %s\n", cfg.hxc_mode ? "HxC" : "Native");
    fatfs.cdir = cfg.cur_cdir;
    return;

clear_image_a:
    /* Error! Clear the IMAGE_A.CFG file. */
    printk("IMAGE_A.CFG is bad: %sring it\n",
           (ff_cfg.image_on_startup == IMGS_last) ? "clea" : "igno");
    F_lseek(&fs->file, 0);
    if (ff_cfg.image_on_startup == IMGS_last)
        F_truncate(&fs->file);
    F_close(&fs->file);
    cfg.slot_nr = cfg.depth = 0;
    cfg.ima_ej_flag = FALSE;
    goto out;
}

#define CFG_KEEP_SLOT_NR  0 /* Do not re-read slot number from config */
#define CFG_READ_SLOT_NR  1 /* Read slot number afresh from config */
#define CFG_WRITE_SLOT_NR 2 /* Write new slot number to config */

static void native_update(uint8_t slot_mode)
{
    int i;

    if (slot_mode == CFG_READ_SLOT_NR) {
        /* Populate slot_map[]. */
        memset(&cfg.slot_map, 0xff, sizeof(cfg.slot_map));
        cfg.max_slot_nr = cfg.depth ? 1 : 0;
        F_opendir(&fs->dp, "");
        while (native_dir_next())
            cfg.max_slot_nr++;
        /* Adjust max_slot_nr. Must be at least one 'slot'. */
        if (!cfg.max_slot_nr)
            F_die();
        cfg.max_slot_nr--;
        F_closedir(&fs->dp);
        /* Select last disk_index if not greater than available slots. */
        cfg.slot_nr = (cfg.slot_nr <= cfg.max_slot_nr) ? cfg.slot_nr : 0;
    }

    if ((ff_cfg.image_on_startup == IMGS_last)
        && (slot_mode == CFG_WRITE_SLOT_NR)) {
        char *p, *q;
        fatfs.cdir = cfg.cfg_cdir;
        F_open(&fs->file, "IMAGE_A.CFG", FA_READ|FA_WRITE);
        printk("Before: "); dump_file();
        /* Read final section of the file. */
        if (f_size(&fs->file) > sizeof(fs->buf))
            F_lseek(&fs->file, f_size(&fs->file) - sizeof(fs->buf));
        F_read(&fs->file, fs->buf, sizeof(fs->buf), NULL);
        F_lseek(&fs->file, (f_size(&fs->file) > sizeof(fs->buf)
                            ? f_size(&fs->file) - sizeof(fs->buf) : 0));
        /* Find end of last subfolder name, if any. */
        if ((p = strrchr(fs->buf, '/')) != NULL) {
            /* Found: seek to after the trailing '/'. */
            F_lseek(&fs->file, f_tell(&fs->file) + (p+1 - fs->buf));
        } else {
            /* No subfolder: we overwrite the entire file. */
            F_lseek(&fs->file, 0);
        }
        if (cfg.slot.attributes & AM_DIR) {
            if (!strcmp(fs->fp.fname, "..")) {
                /* Strip back to next '/' */
                if (!p) F_die(); /* must exist */
                *p = '\0';
                if ((q = strrchr(fs->buf, '/')) != NULL) {
                    F_lseek(&fs->file, f_tell(&fs->file) - (p-q));
                } else {
                    F_lseek(&fs->file, 0);
                }
            } else {
                /* Add name plus '/' */
                F_write(&fs->file, fs->fp.fname,
                        strnlen(fs->fp.fname, sizeof(fs->fp.fname)), NULL);
                F_write(&fs->file, "/", 1, NULL);
            }
        } else {
            /* Add name */
            F_write(&fs->file, fs->fp.fname,
                    strnlen(fs->fp.fname, sizeof(fs->fp.fname)), NULL);
        }
        F_truncate(&fs->file);
        printk("After: "); dump_file();
        F_close(&fs->file);
        fatfs.cdir = cfg.cur_cdir;
        cfg.ima_ej_flag = FALSE;
    }
    
    /* Populate current slot. */
    i = cfg.depth ? 1 : 0;
    F_opendir(&fs->dp, "");
    while (native_dir_next()) {
        if (i >= cfg.slot_nr)
            break;
        i++;
    }
    F_closedir(&fs->dp);
    if (i > cfg.slot_nr) {
        /* Must be the ".." folder. */
        snprintf(fs->fp.fname, sizeof(fs->fp.fname), "..");
        fs->fp.fattrib = AM_DIR;
    }
    if (fs->fp.fattrib & AM_DIR) {
        /* Leave the full pathname cached in fs->fp. */
        cfg.slot.attributes = fs->fp.fattrib;
        snprintf(cfg.slot.name, sizeof(cfg.slot.name), "%s", fs->fp.fname);
    } else {
        F_open(&fs->file, fs->fp.fname, FA_READ);
        fs->file.obj.attr = fs->fp.fattrib;
        fatfs_to_slot(&cfg.slot, &fs->file, fs->fp.fname);
        F_close(&fs->file);
    }
}

static void ima_mark_ejected(bool_t ej)
{
    if (cfg.hxc_mode || (ff_cfg.image_on_startup != IMGS_last)
        || (cfg.ima_ej_flag == ej))
        return;

    fatfs.cdir = cfg.cfg_cdir;
    F_open(&fs->file, "IMAGE_A.CFG", FA_READ|FA_WRITE);
    printk("Before: "); dump_file();
    if (ej) {
        F_lseek(&fs->file, f_size(&fs->file));
        F_write(&fs->file, "\\EJ", 3, NULL);
    } else {
        F_lseek(&fs->file, max_t(int, f_size(&fs->file)-3, 0));
        F_truncate(&fs->file);
    }
    printk("After: "); dump_file();
    F_close(&fs->file);
    fatfs.cdir = cfg.cur_cdir;
    cfg.ima_ej_flag = ej;
}

static void hxc_cfg_update(uint8_t slot_mode)
{
    struct hxcsdfe_cfg hxc_cfg;
    BYTE mode = FA_READ;
    int i;

    if (slot_mode == CFG_WRITE_SLOT_NR)
        mode |= FA_WRITE;

    fatfs_from_slot(&fs->file, &cfg.hxcsdfe, mode);
    F_read(&fs->file, &hxc_cfg, sizeof(hxc_cfg), NULL);
    if (strncmp("HXCFECFGV", hxc_cfg.signature, 9))
        goto bad_signature;

    if (slot_mode == CFG_READ_SLOT_NR) {
        /* buzzer_step_duration seems to range 0xFF-0xD8. */
        if (!cfg.ffcfg_has_step_volume)
            ff_cfg.step_volume = hxc_cfg.step_sound
                ? (0x100 - hxc_cfg.buzzer_step_duration) / 2 : 0;
        if (!cfg.ffcfg_has_display_off_secs)
            ff_cfg.display_off_secs = hxc_cfg.back_light_tmr;
        /* Interpret HxC scroll speed as updates per minute. */
        if (!cfg.ffcfg_has_display_scroll_rate && hxc_cfg.lcd_scroll_speed)
            ff_cfg.display_scroll_rate = 60000u / hxc_cfg.lcd_scroll_speed;
    }

    switch (hxc_cfg.signature[9]-'0') {

    case 1: {
        struct v1_slot v1_slot;
        if (slot_mode != CFG_READ_SLOT_NR) {
            /* Keep the already-configured slot number. */
            hxc_cfg.slot_index = cfg.slot_nr;
            if (slot_mode == CFG_WRITE_SLOT_NR) {
                /* Update the config file with new slot number. */
                F_lseek(&fs->file, 0);
                F_write(&fs->file, &hxc_cfg, sizeof(hxc_cfg), NULL);
            }
        }
        cfg.slot_nr = hxc_cfg.slot_index;
        if (hxc_cfg.index_mode)
            break;
        /* Slot mode: initialise slot map and current slot. */
        if (slot_mode == CFG_READ_SLOT_NR) {
            cfg.max_slot_nr = hxc_cfg.number_of_slot - 1;
            memset(&cfg.slot_map, 0xff, sizeof(cfg.slot_map));
        }
        /* Slot mode: read current slot file info. */
        if (cfg.slot_nr == 0) {
            memcpy(&cfg.slot, &cfg.autoboot, sizeof(cfg.slot));
        } else {
            F_lseek(&fs->file, 1024 + cfg.slot_nr*128);
            F_read(&fs->file, &v1_slot, sizeof(v1_slot), NULL);
            memcpy(&cfg.slot.type, &v1_slot.name[8], 3);
            memcpy(&cfg.slot.attributes, &v1_slot.attributes, 1+4+4+17);
            cfg.slot.name[17] = '\0';
        }
        break;
    }

    case 2:
        if (slot_mode != CFG_READ_SLOT_NR) {
            hxc_cfg.cur_slot_number = cfg.slot_nr;
            if (slot_mode == CFG_WRITE_SLOT_NR) {
                F_lseek(&fs->file, 0);
                F_write(&fs->file, &hxc_cfg, sizeof(hxc_cfg), NULL);
            }
        }
        cfg.slot_nr = hxc_cfg.cur_slot_number;
        if (hxc_cfg.index_mode)
            break;
        /* Slot mode: initialise slot map and current slot. */
        if (slot_mode == CFG_READ_SLOT_NR) {
            cfg.max_slot_nr = hxc_cfg.max_slot_number - 1;
            F_lseek(&fs->file, hxc_cfg.slots_map_position*512);
            F_read(&fs->file, &cfg.slot_map, sizeof(cfg.slot_map), NULL);
            cfg.slot_map[0] |= 0x80; /* slot 0 always available */
            /* Find true max_slot_nr: */
            while (!(cfg.slot_map[cfg.max_slot_nr/8]
                     & (0x80>>(cfg.max_slot_nr&7))))
                cfg.max_slot_nr--;
        }
        /* Slot mode: read current slot file info. */
        if (cfg.slot_nr == 0) {
            memcpy(&cfg.slot, &cfg.autoboot, sizeof(cfg.slot));
        } else {
            F_lseek(&fs->file, hxc_cfg.slots_position*512
                    + cfg.slot_nr*64*hxc_cfg.number_of_drive_per_slot);
            F_read(&fs->file, &cfg.slot, sizeof(cfg.slot), NULL);
        }
        break;

    default:
    bad_signature:
        hxc_cfg.signature[15] = '\0';
        printk("Bad signature '%s'\n", hxc_cfg.signature);
        F_die();

    }

    F_close(&fs->file);

    if (hxc_cfg.index_mode) {

        char name[16];

        /* Index mode: populate slot_map[]. */
        if (slot_mode == CFG_READ_SLOT_NR) {
            memset(&cfg.slot_map, 0, sizeof(cfg.slot_map));
            cfg.max_slot_nr = 0;
            for (F_findfirst(&fs->dp, &fs->fp, "", "DSKA*.*");
                 fs->fp.fname[0] != '\0';
                 F_findnext(&fs->dp, &fs->fp)) {
                const char *p = fs->fp.fname + 4; /* skip "DSKA" */
                unsigned int idx = 0;
                /* Skip directories. */
                if (fs->fp.fattrib & AM_DIR)
                    continue;
                /* Parse 4-digit index number. */
                for (i = 0; i < 4; i++) {
                    if ((*p < '0') || (*p > '9'))
                        break;
                    idx *= 10;
                    idx += *p++ - '0';
                }
                /* Expect a 4-digit number range 0-999 followed by a period. */
                if ((i != 4) || (*p++ != '.') || (idx > 999))
                    continue;
                /* Expect 3-char extension followed by nul. */
                for (i = 0; (i < 3) && *p; i++, p++)
                    continue;
                if ((i != 3) || (*p != '\0'))
                    continue;
                /* A file type we support? */
                if (!image_valid(&fs->fp))
                    continue;
                /* All is fine, populate the 'slot'. */
                cfg.slot_map[idx/8] |= 0x80 >> (idx&7);
                cfg.max_slot_nr = max_t(
                    uint16_t, cfg.max_slot_nr, idx);
            }
            F_closedir(&fs->dp);
        }

        /* Index mode: populate current slot. */
        snprintf(name, sizeof(name), "DSKA%04u.*", cfg.slot_nr);
        F_findfirst(&fs->dp, &fs->fp, "", name);
        F_closedir(&fs->dp);
        if (fs->fp.fname[0]) {
            F_open(&fs->file, fs->fp.fname, FA_READ);
            fs->file.obj.attr = fs->fp.fattrib;
            fatfs_to_slot(&cfg.slot, &fs->file, fs->fp.fname);
            F_close(&fs->file);
        }
    }

    for (i = 0; i < 3; i++)
        cfg.slot.type[i] = tolower(cfg.slot.type[i]);
}

static void cfg_update(uint8_t slot_mode)
{
    if (cfg.hxc_mode)
        hxc_cfg_update(slot_mode);
    else
        native_update(slot_mode);
}

/* Based on button presses, change which floppy image is selected. */
static void choose_new_image(uint8_t init_b)
{
    uint8_t b, prev_b;
    stk_time_t last_change = 0;
    int i, changes = 0;

    for (prev_b = 0, b = init_b;
         (b &= (B_LEFT|B_RIGHT)) != 0;
         prev_b = b, b = buttons) {

        if (prev_b == b) {
            /* Decaying delay between image steps while button pressed. */
            stk_time_t delay = stk_ms(1000) / (changes + 1);
            if (delay < stk_ms(50))
                delay = stk_ms(50);
            if (stk_diff(last_change, stk_now()) < delay)
                continue;
            changes++;
        } else {
            /* Different button pressed. Takes immediate effect, resets 
             * the continuous-press decaying delay. */
            changes = 0;
        }
        last_change = stk_now();

        i = cfg.slot_nr;
        if (!(b ^ (B_LEFT|B_RIGHT))) {
            i = cfg.slot_nr = 0;
            cfg_update(CFG_KEEP_SLOT_NR);
            display_write_slot();
            /* Ignore changes while user is releasing the buttons. */
            while ((stk_diff(last_change, stk_now()) < stk_ms(1000))
                   && buttons)
                continue;
        } else if (b & B_LEFT) {
        b_left:
            do {
                if (i-- == 0) {
                    if (!ff_cfg.nav_loop)
                        goto b_right;
                    i = cfg.max_slot_nr;
                }
            } while (!(cfg.slot_map[i/8] & (0x80>>(i&7))));
        } else { /* b & B_RIGHT */
        b_right:
            do {
                if (i++ >= cfg.max_slot_nr) {
                    if (!ff_cfg.nav_loop)
                        goto b_left;
                    i = 0;
                }
            } while (!(cfg.slot_map[i/8] & (0x80>>(i&7))));
        }

        cfg.slot_nr = i;
        cfg_update(CFG_KEEP_SLOT_NR);
        display_write_slot();
    }
}

static void assert_usbh_msc_connected(void)
{
    if (!usbh_msc_connected())
        F_die();
}

static int run_floppy(void *_b)
{
    volatile uint8_t *pb = _b;
    stk_time_t t_now, t_prev, t_diff;

    floppy_insert(0, &cfg.slot);

    lcd_update_ticks = stk_ms(20);
    lcd_scroll_ticks = stk_ms(LCD_SCROLL_PAUSE_MSEC);
    lcd_scroll_off = 0;
    lcd_scroll_end = max_t(
        int, strnlen(cfg.slot.name, sizeof(cfg.slot.name)) - 16, 0);
    t_prev = stk_now();
    while (((*pb = buttons) == 0) && !floppy_handle()) {
        t_now = stk_now();
        t_diff = stk_diff(t_prev, t_now);
        if (display_mode == DM_LCD_1602) {
            lcd_update_ticks -= t_diff;
            if (lcd_update_ticks <= 0) {
                lcd_write_track_info(FALSE);
                lcd_update_ticks = stk_ms(20);
            }
            lcd_scroll_ticks -= t_diff;
            lcd_scroll_name();
        }
        canary_check();
        assert_usbh_msc_connected();
        t_prev = t_now;
    }

    return 0;
}

static int floppy_main(void *unused)
{
    FRESULT fres;
    char msg[10];
    uint8_t b;
    uint32_t i;

    /* If any buttons are pressed when USB drive is mounted then we start 
     * in ejected state. */
    if (buttons)
        cfg.ejected = TRUE;

    arena_init();
    fs = arena_alloc(sizeof(*fs));
    
    cfg_init();
    cfg_update(CFG_READ_SLOT_NR);

    /* If we start on a folder, go directly into the image selector. */
    if (cfg.slot.attributes & AM_DIR) {
        display_write_slot();
        b = buttons;
        goto select;
    }

    for (;;) {

        /* Make sure slot index is on a valid slot. Find next valid slot if 
         * not (and update config). */
        i = cfg.slot_nr;
        if (!(cfg.slot_map[i/8] & (0x80>>(i&7)))) {
            while (!(cfg.slot_map[i/8] & (0x80>>(i&7))))
                if (i++ >= cfg.max_slot_nr)
                    i = 0;
            printk("Updated slot %u -> %u\n", cfg.slot_nr, i);
            cfg.slot_nr = i;
            cfg_update(CFG_WRITE_SLOT_NR);
        }

        if (cfg.slot.attributes & AM_DIR) {
            if (!strcmp(fs->fp.fname, "..")) {
                fatfs.cdir = cfg.cur_cdir = cfg.cdir_stack[--cfg.depth];
                cfg.slot_nr = 0;
            } else {
                cfg.cdir_stack[cfg.depth++] = cfg.cur_cdir;
                F_chdir(fs->fp.fname);
                cfg.cur_cdir = fatfs.cdir;
                cfg.slot_nr = 1;
            }
            cfg_update(CFG_READ_SLOT_NR);
            display_write_slot();
            b = buttons;
            goto select;
        }

        fs = NULL;

        display_write_slot();
        if (display_mode == DM_LCD_1602)
            lcd_write_track_info(TRUE);

        printk("Current slot: %u/%u\n", cfg.slot_nr, cfg.max_slot_nr);
        memcpy(msg, cfg.slot.type, 3);
        msg[3] = '\0';
        printk("Name: '%s' Type: %s\n", cfg.slot.name, msg);
        printk("Attr: %02x Clus: %08x Size: %u\n",
               cfg.slot.attributes, cfg.slot.firstCluster, cfg.slot.size);

        if (cfg.ejected) {
            cfg.ejected = FALSE;
            b = B_SELECT;
        } else {
            fres = F_call_cancellable(run_floppy, &b);
            floppy_cancel();
            assert_usbh_msc_connected();
        }

        arena_init();
        fs = arena_alloc(sizeof(*fs));

        /* When an image is loaded, select button means eject. */
        if (fres || (b & B_SELECT)) {
            /* ** EJECT STATE ** */
            unsigned int wait = 0;
            snprintf(msg, sizeof(msg), "EJECT");
            switch (display_mode) {
            case DM_LED_7SEG:
                if (fres)
                    snprintf(msg, sizeof(msg), "E%02u", fres);
                led_7seg_write_string(msg);
                break;
            case DM_LCD_1602:
                if (fres)
                    snprintf(msg, sizeof(msg), "ERR %02u", fres);
                lcd_write(0, 1, 8, msg);
                break;
            }
            if (fres == FR_OK)
                ima_mark_ejected(TRUE);
            fres = FR_OK;
            /* Wait for buttons to be released. */
            while (buttons != 0)
                continue;
            /* Wait for any button to be pressed. */
            while ((b = buttons) == 0) {
                /* Bail if USB disconnects. */
                assert_usbh_msc_connected();
                /* Alternate the 7-segment display if it's connected. */
                delay_ms(1);
                if ((display_mode == DM_LED_7SEG) && ((++wait % 1000) == 0)) {
                    switch (wait / 1000) {
                    case 1:
                        led_7seg_write_decimal(cfg.slot_nr);
                        break;
                    default:
                        led_7seg_write_string(msg);
                        wait = 0;
                        break;
                    }
                }
            }
            /* Reload same image immediately if eject pressed again. */
            if (b & B_SELECT) {
                while (buttons & B_SELECT)
                    continue;
                ima_mark_ejected(FALSE);
                continue;
            }
        }

        /* No buttons pressed: re-read config and carry on. */
        if (b == 0) {
            cfg_update(CFG_READ_SLOT_NR);
            continue;
        }

    select:
        do {
            unsigned int wait_secs;

            /* While buttons are pressed we poll them and update current image
             * accordingly. */
            choose_new_image(b);

            /* Wait a few seconds for further button presses before acting on 
             * the new image selection. */
            wait_secs = (cfg.slot.attributes & AM_DIR) ?
                ff_cfg.autoselect_folder_secs : ff_cfg.autoselect_file_secs;
            for (i = 0; (wait_secs == 0) || (i < wait_secs*1000); i++) {
                b = buttons;
                if (b != 0)
                    break;
                assert_usbh_msc_connected();
                delay_ms(1);
            }

            /* Flash the LED display to indicate loading the new image. */
            if (!(b & (B_LEFT|B_RIGHT)) && (display_mode == DM_LED_7SEG)) {
                led_7seg_display_setting(FALSE);
                delay_ms(200);
                led_7seg_display_setting(TRUE);
                b = buttons;
            }

            /* Wait for select button to be released. */
            while ((b = buttons) & B_SELECT)
                continue;

        } while (b != 0);

        cfg.ejected = FALSE;

        /* Write the slot number resulting from the latest round of button 
         * presses back to the config file. */
        cfg_update(CFG_WRITE_SLOT_NR);
    }

    ASSERT(0);
    return 0;
}

static void cfg_factory_reset(void)
{
    unsigned int i;

    /* Buttons must be pressed for three seconds. */
    for (i = 0; i < 3000; i++) {
        if (buttons != (B_LEFT|B_RIGHT))
            break;
        delay_ms(1);
    }
    if (i != 3000)
        return;

    /* Inform user that factory reset is about to occur. */
    switch (display_mode) {
    case DM_LED_7SEG:
        led_7seg_write_string("RST");
        break;
    case DM_LCD_1602:
        lcd_clear();
        lcd_write(0, 0, 0, "Reset Flash");
        lcd_write(0, 1, 0, "Configuration");
        lcd_on();
        break;
    }

    /* Wait for buttons to be released... */
    while (buttons == (B_LEFT|B_RIGHT))
        continue;

    /* ...and then do the Flash erase. */
    flash_ff_cfg_erase();

    /* Linger so user sees it is done. */
    delay_ms(2000);

    /* Reset so that changes take effect. */
    system_reset();
}

static void banner(void)
{
    switch (display_mode) {
    case DM_LED_7SEG:
        led_7seg_write_string((led_7seg_nr_digits() == 3) ? "F-F" : "FF");
        break;
    case DM_LCD_1602:
        lcd_clear();
        lcd_write(0, 0, 0, "FlashFloppy");
        lcd_write(0, 1, 0, "v");
        lcd_write(1, 1, 0, FW_VER);
        lcd_on();
        break;
    }
}

int main(void)
{
    FRESULT fres;
    uint8_t fintf_mode;

    /* Relocate DATA. Initialise BSS. */
    if (_sdat != _ldat)
        memcpy(_sdat, _ldat, _edat-_sdat);
    memset(_sbss, 0, _ebss-_sbss);

    canary_init();

    stm32_init();
    timers_init();

    console_init();
    console_crash_on_input();

    board_init();

    /* Wait for 5v power to stabilise before initing external peripherals. */
    delay_ms(200);

    printk("\n** FlashFloppy v%s for Gotek\n", FW_VER);
    printk("** Keir Fraser <keir.xen@gmail.com>\n");
    printk("** https://github.com/keirf/FlashFloppy\n\n");

    speaker_init();

    flash_ff_cfg_read();

    fintf_mode = ff_cfg.interface;
    if (fintf_mode == FINTF_JC) {
        /* Jumper JC selects default floppy interface configuration:
         *   - No Jumper: Shugart
         *   - Jumpered:  IBM PC */
        fintf_mode = gpio_read_pin(gpiob, 1) ? FINTF_SHUGART : FINTF_IBMPC;
    }
    floppy_init(fintf_mode);

    display_init();

    usbh_msc_init();

    timer_init(&button_timer, button_timer_fn, NULL);
    timer_set(&button_timer, stk_now());

    for (;;) {

        banner();

        while (f_mount(&fatfs, "", 1) != FR_OK) {
            if (buttons == (B_LEFT|B_RIGHT))
                cfg_factory_reset();
            usbh_msc_process();
        }

        arena_init();
        fres = F_call_cancellable(floppy_main, NULL);
        floppy_cancel();
        printk("FATFS RETURN CODE: %u\n", fres);
    }

    return 0;
}

/*
 * Local variables:
 * mode: C
 * c-file-style: "Linux"
 * c-basic-offset: 4
 * tab-width: 4
 * indent-tabs-mode: nil
 * End:
 */
