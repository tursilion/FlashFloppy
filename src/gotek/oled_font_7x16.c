/*
 * oled_font_7x16.c
 * 
 * Font and font-specific render code contributed by Kingstener for his
 * OLED display "hard hack". This font is narrower than the 8x16 default
 * and thus fits fully within the confines of the Gotek's 3-digit display
 * cutout.
 * 
 * Written & released for FlashFloppy by Kingstener.
 * 
 * This is free and unencumbered software released into the public domain.
 * See the file COPYING for more details, or visit <http://unlicense.org>.
 */

/* ASCII 0x20-0x7e inclusive. */
const uint8_t oled_font_7x16[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* ' ' 00 */
    0x00, 0x00, 0xFC, 0xFC, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x37, 0x37, 0x00, 0x00, 0x00,  /* '!' 01 */
    0x1E, 0x1E, 0x00, 0x00, 0x1E, 0x1E, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* '"' 02 */
    0x60, 0xF8, 0x60, 0x60, 0xF8, 0x60, 0x00,
    0x06, 0x1F, 0x06, 0x06, 0x1F, 0x06, 0x00,  /* '#' 03 */
    0xE0, 0x90, 0xFC, 0xFC, 0xB0, 0x30, 0x00,
    0x0C, 0x0D, 0x3F, 0x3F, 0x09, 0x07, 0x00,  /* '$' 04 */
    0x18, 0x24, 0xA4, 0xD8, 0x70, 0x18, 0x00,
    0x18, 0x0E, 0x1B, 0x25, 0x24, 0x18, 0x00,  /* '%' 05 */
    0xF0, 0xF8, 0x08, 0xF8, 0xF0, 0x00, 0x00,
    0x1E, 0x3F, 0x31, 0x3F, 0x1E, 0x36, 0x00,  /* '&' 06 */
    0x00, 0x00, 0x1E, 0x1E, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* ''' 07 */
    0x00, 0xE0, 0xF8, 0x1C, 0x06, 0x02, 0x00,
    0x00, 0x07, 0x1F, 0x38, 0x60, 0x40, 0x00,  /* '(' 08 */
    0x00, 0x02, 0x06, 0x1C, 0xF8, 0xE0, 0x00,
    0x00, 0x40, 0x60, 0x38, 0x1F, 0x07, 0x00,  /* ')' 09 */
    0x00, 0x40, 0x80, 0xE0, 0x80, 0x40, 0x00,
    0x00, 0x05, 0x03, 0x0F, 0x03, 0x05, 0x00,  /* '*' 10 */
    0x80, 0x80, 0xE0, 0xE0, 0x80, 0x80, 0x00,
    0x01, 0x01, 0x07, 0x07, 0x01, 0x01, 0x00,  /* '+' 11 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0xE0, 0x70, 0x30, 0x00, 0x00, 0x00,  /* ',' 12 */
    0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00,
    0x00, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00,  /* '-' 13 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x10, 0x38, 0x38, 0x10, 0x00, 0x00,  /* '.' 14 */
    0x00, 0x00, 0x00, 0xE0, 0xF8, 0x1C, 0x00,
    0x38, 0x1E, 0x07, 0x01, 0x00, 0x00, 0x00,  /* '/' 15 */
    0xF0, 0xF8, 0x0C, 0x8C, 0xF8, 0xF0, 0x00,
    0x0F, 0x1F, 0x31, 0x30, 0x1F, 0x0F, 0x00,  /* '0' 16 */
    0x20, 0x30, 0xF8, 0xFC, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x3F, 0x3F, 0x00, 0x00, 0x00,  /* '1' 17 */
    0x18, 0x1C, 0x0C, 0x8C, 0xFC, 0x78, 0x00,
    0x3C, 0x3E, 0x33, 0x31, 0x30, 0x30, 0x00,  /* '2' 18 */
    0x18, 0x1C, 0x0C, 0x8C, 0xFC, 0x78, 0x00,
    0x18, 0x38, 0x30, 0x31, 0x3F, 0x1E, 0x00,  /* '3' 19 */
    0xC0, 0xE0, 0x30, 0x18, 0xFC, 0xFC, 0x00,
    0x03, 0x03, 0x03, 0x03, 0x3F, 0x3F, 0x00,  /* '4' 20 */
    0xFC, 0xFC, 0xCC, 0xCC, 0xCC, 0x8C, 0x00,
    0x18, 0x38, 0x30, 0x30, 0x3F, 0x1F, 0x00,  /* '5' 21 */
    0xF8, 0xFC, 0x8C, 0x8C, 0x9C, 0x18, 0x00,
    0x1F, 0x3F, 0x31, 0x31, 0x3F, 0x1F, 0x00,  /* '6' 22 */
    0x0C, 0x0C, 0x0C, 0xCC, 0xFC, 0x3C, 0x00,
    0x30, 0x3C, 0x0F, 0x03, 0x00, 0x00, 0x00,  /* '7' 23 */
    0x78, 0xFC, 0x8C, 0x8C, 0xFC, 0x78, 0x00,
    0x1E, 0x3F, 0x31, 0x31, 0x3F, 0x1E, 0x00,  /* '8' 24 */
    0xF8, 0xFC, 0x8C, 0x8C, 0xFC, 0xF8, 0x00,
    0x18, 0x39, 0x31, 0x31, 0x3F, 0x1F, 0x00,  /* '9' 25 */
    0x00, 0x40, 0xE0, 0xE0, 0x40, 0x00, 0x00,
    0x00, 0x08, 0x1C, 0x1C, 0x08, 0x00, 0x00,  /* ':' 26 */
    0x00, 0x00, 0x80, 0x80, 0x00, 0x00, 0x00,
    0x80, 0xE1, 0x71, 0x31, 0x11, 0x00, 0x00,  /* ';' 27 */
    0x00, 0x80, 0xC0, 0x60, 0x30, 0x10, 0x00,
    0x01, 0x03, 0x06, 0x0C, 0x18, 0x10, 0x00,  /* '<' 28 */
    0x60, 0x60, 0x60, 0x60, 0x60, 0x60, 0x00,
    0x06, 0x06, 0x06, 0x06, 0x06, 0x06, 0x00,  /* '=' 29 */
    0x10, 0x30, 0x60, 0xC0, 0x80, 0x00, 0x00,
    0x10, 0x18, 0x0C, 0x06, 0x03, 0x01, 0x00,  /* '>' 30 */
    0x18, 0x0C, 0x0C, 0x8C, 0xFC, 0x38, 0x00,
    0x00, 0x00, 0x36, 0x37, 0x01, 0x00, 0x00,  /* '?' 31 */
    0xF8, 0xFC, 0x0C, 0xCC, 0x2C, 0xF8, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x39, 0x19, 0x00,  /* '@' 32 */
    0xF8, 0xFC, 0x8C, 0x8C, 0xFC, 0xF8, 0x00,
    0x3F, 0x3F, 0x01, 0x01, 0x3F, 0x3F, 0x00,  /* 'A' 33 */
    0xFC, 0xFC, 0x8C, 0x8C, 0xFC, 0x78, 0x00,
    0x3F, 0x3F, 0x31, 0x31, 0x3F, 0x0E, 0x00,  /* 'B' 34 */
    0xF8, 0xFC, 0x0C, 0x0C, 0x1C, 0x18, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x38, 0x18, 0x00,  /* 'C' 35 */
    0xFC, 0xFC, 0x0C, 0x0C, 0xF8, 0xF0, 0x00,
    0x3F, 0x3F, 0x30, 0x30, 0x1F, 0x0F, 0x00,  /* 'D' 36 */
    0xFC, 0xFC, 0x8C, 0x8C, 0x8C, 0x8C, 0x00,
    0x3F, 0x3F, 0x31, 0x31, 0x31, 0x31, 0x00,  /* 'E' 37 */
    0xFC, 0xFC, 0x8C, 0x8C, 0x8C, 0x0C, 0x00,
    0x3F, 0x3F, 0x01, 0x01, 0x01, 0x00, 0x00,  /* 'F' 38 */
    0xF8, 0xFC, 0x0C, 0x8C, 0x9C, 0x98, 0x00,
    0x1F, 0x3F, 0x30, 0x31, 0x3F, 0x1F, 0x00,  /* 'G' 39 */
    0xFC, 0xFC, 0x80, 0x80, 0xFC, 0xFC, 0x00,
    0x3F, 0x3F, 0x01, 0x01, 0x3F, 0x3F, 0x00,  /* 'H' 40 */
    0x0C, 0x0C, 0xFC, 0xFC, 0x0C, 0x0C, 0x00,
    0x30, 0x30, 0x3F, 0x3F, 0x30, 0x30, 0x00,  /* 'I' 41 */
    0x00, 0x00, 0x0C, 0x0C, 0xFC, 0xFC, 0x00,
    0x18, 0x38, 0x30, 0x30, 0x3F, 0x1F, 0x00,  /* 'J' 42 */
    0xFC, 0xFC, 0x80, 0xE0, 0xFC, 0x1C, 0x00,
    0x3F, 0x3F, 0x01, 0x07, 0x3E, 0x38, 0x00,  /* 'K' 43 */
    0xFC, 0xFC, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x3F, 0x3F, 0x30, 0x30, 0x30, 0x30, 0x00,  /* 'L' 44 */
    0xFC, 0xF8, 0xF0, 0xF0, 0xF8, 0xFC, 0x00,
    0x3F, 0x3F, 0x01, 0x01, 0x3F, 0x3F, 0x00,  /* 'M' 45 */
    0xFC, 0xF8, 0xE0, 0x80, 0xFC, 0xFC, 0x00,
    0x3F, 0x3F, 0x03, 0x07, 0x1F, 0x3F, 0x00,  /* 'N' 46 */
    0xF8, 0xFC, 0x0C, 0x0C, 0xFC, 0xF8, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x3F, 0x1F, 0x00,  /* 'O' 47 */
    0xFC, 0xFC, 0x8C, 0x8C, 0xFC, 0xF8, 0x00,
    0x3F, 0x3F, 0x01, 0x01, 0x01, 0x00, 0x00,  /* 'P' 48 */
    0xF8, 0xFC, 0x0C, 0x0C, 0xFC, 0xF8, 0x00,
    0x1F, 0x3F, 0x30, 0x38, 0x7F, 0x5F, 0x00,  /* 'Q' 49 */
    0xFC, 0xFC, 0x8C, 0x8C, 0xFC, 0xF8, 0x00,
    0x3F, 0x3F, 0x03, 0x07, 0x3D, 0x38, 0x00,  /* 'R' 50 */
    0x78, 0xFC, 0xCC, 0x8C, 0x1C, 0x18, 0x00,
    0x18, 0x38, 0x31, 0x33, 0x3F, 0x1E, 0x00,  /* 'S' 51 */
    0x0C, 0x0C, 0xFC, 0xFC, 0x0C, 0x0C, 0x00,
    0x00, 0x00, 0x3F, 0x3F, 0x00, 0x00, 0x00,  /* 'T' 52 */
    0xFC, 0xFC, 0x00, 0x00, 0xFC, 0xFC, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x3F, 0x1F, 0x00,  /* 'U' 53 */
    0xFC, 0xFC, 0x00, 0x00, 0xFC, 0xFC, 0x00,
    0x03, 0x0F, 0x3C, 0x3C, 0x0F, 0x03, 0x00,  /* 'V' 54 */
    0xFC, 0xF8, 0x00, 0x00, 0xF8, 0xFC, 0x00,
    0x3F, 0x1F, 0x0E, 0x0E, 0x1F, 0x3F, 0x00,  /* 'W' 55 */
    0x1C, 0x7C, 0xE0, 0xE0, 0x7C, 0x1C, 0x00,
    0x38, 0x3E, 0x07, 0x07, 0x3E, 0x38, 0x00,  /* 'X' 56 */
    0xFC, 0xFC, 0x80, 0x80, 0xFC, 0xFC, 0x00,
    0x00, 0x01, 0x3F, 0x3F, 0x01, 0x00, 0x00,  /* 'Y' 57 */
    0x0C, 0x0C, 0x8C, 0xEC, 0x7C, 0x1C, 0x00,
    0x38, 0x3E, 0x37, 0x31, 0x30, 0x30, 0x00,  /* 'Z' 58 */
    0x00, 0xFE, 0xFE, 0x02, 0x02, 0x00, 0x00,
    0x00, 0x7F, 0x7F, 0x40, 0x40, 0x00, 0x00,  /* '[' 59 */
    0x1C, 0x78, 0xE0, 0x80, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x01, 0x07, 0x1E, 0x38, 0x00,  /* '\' 60 */
    0x00, 0x06, 0x06, 0xFE, 0xFE, 0x00, 0x00,
    0x00, 0x60, 0x60, 0x7F, 0x7F, 0x00, 0x00,  /* ']' 61 */
    0x30, 0x18, 0x0C, 0x0C, 0x18, 0x30, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* '^' 62 */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x00,  /* '_' 63 */
    0x00, 0x04, 0x0C, 0x1C, 0x38, 0x20, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  /* '`' 64 */
    0x00, 0xC0, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x1C, 0x3E, 0x36, 0x36, 0x1F, 0x3F, 0x00,  /* 'a' 65 */
    0xF0, 0xF0, 0x80, 0xC0, 0xC0, 0x80, 0x00,
    0x3F, 0x1F, 0x39, 0x30, 0x3F, 0x1F, 0x00,  /* 'b' 66 */
    0x80, 0xC0, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x39, 0x19, 0x00,  /* 'c' 67 */
    0x80, 0xC0, 0xC0, 0x80, 0xF0, 0xF0, 0x00,
    0x1F, 0x3F, 0x30, 0x39, 0x1F, 0x3F, 0x00,  /* 'd' 68 */
    0x80, 0xC0, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x1F, 0x3F, 0x34, 0x24, 0x27, 0x07, 0x00,  /* 'e' 69 */
    0x00, 0xE0, 0xF0, 0x30, 0x30, 0x60, 0x00,
    0x03, 0x3F, 0x3F, 0x03, 0x03, 0x00, 0x00,  /* 'f' 70 */
    0x80, 0xC0, 0xC0, 0xC0, 0x80, 0xC0, 0x00,
    0x4F, 0xDF, 0xD8, 0xC8, 0xFF, 0x7F, 0x00,  /* 'g' 71 */
    0xF0, 0xF0, 0x80, 0xC0, 0xC0, 0x80, 0x00,
    0x3F, 0x3F, 0x01, 0x00, 0x3F, 0x3F, 0x00,  /* 'h' 72 */
    0x00, 0xC0, 0xD8, 0xD8, 0x00, 0x00, 0x00,
    0x00, 0x30, 0x3F, 0x3F, 0x30, 0x00, 0x00,  /* 'i' 73 */
    0x00, 0x00, 0xC0, 0xD8, 0xD8, 0x00, 0x00,
    0x60, 0xC0, 0xC0, 0xFF, 0x7F, 0x00, 0x00,  /* 'j' 74 */
    0xF0, 0xF0, 0x00, 0xC0, 0xC0, 0x00, 0x00,
    0x3F, 0x3F, 0x06, 0x0F, 0x39, 0x30, 0x00,  /* 'k' 75 */
    0x00, 0x30, 0xF0, 0xF0, 0x00, 0x00, 0x00,
    0x00, 0x30, 0x3F, 0x3F, 0x30, 0x00, 0x00,  /* 'l' 76 */
    0xC0, 0xC0, 0x80, 0x80, 0xC0, 0xC0, 0x00,
    0x3F, 0x3F, 0x03, 0x03, 0x3F, 0x3F, 0x00,  /* 'm' 77 */
    0xC0, 0x80, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x3F, 0x3F, 0x01, 0x00, 0x3F, 0x3F, 0x00,  /* 'n' 78 */
    0x80, 0xC0, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x1F, 0x3F, 0x30, 0x30, 0x3F, 0x1F, 0x00,  /* 'o' 79 */
    0xC0, 0x80, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0xFF, 0xFF, 0x08, 0x18, 0x1F, 0x0F, 0x00,  /* 'p' 80 */
    0x80, 0xC0, 0xC0, 0xC0, 0x80, 0xC0, 0x00,
    0x0F, 0x1F, 0x18, 0x08, 0xFF, 0xFF, 0x00,  /* 'q' 81 */
    0xC0, 0x80, 0xC0, 0xC0, 0xC0, 0x80, 0x00,
    0x3F, 0x3F, 0x01, 0x00, 0x00, 0x01, 0x00,  /* 'r' 82 */
    0x80, 0xC0, 0xC0, 0x40, 0xC0, 0x80, 0x00,
    0x13, 0x37, 0x26, 0x36, 0x3E, 0x1C, 0x00,  /* 's' 83 */
    0xC0, 0xF0, 0xF0, 0xC0, 0xC0, 0x00, 0x00,
    0x00, 0x1F, 0x3F, 0x30, 0x30, 0x18, 0x00,  /* 't' 84 */
    0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00,
    0x1F, 0x3F, 0x30, 0x38, 0x1F, 0x3F, 0x00,  /* 'u' 85 */
    0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00,
    0x03, 0x0F, 0x3C, 0x3C, 0x0F, 0x03, 0x00,  /* 'v' 86 */
    0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00,
    0x3F, 0x3F, 0x1C, 0x1C, 0x3F, 0x3F, 0x00,  /* 'w' 87 */
    0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00,
    0x30, 0x39, 0x0F, 0x0F, 0x39, 0x30, 0x00,  /* 'x' 88 */
    0xC0, 0xC0, 0x00, 0x00, 0xC0, 0xC0, 0x00,
    0x67, 0xCF, 0xCC, 0xC6, 0xFF, 0x7F, 0x00,  /* 'y' 89 */
    0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0xC0, 0x00,
    0x38, 0x3C, 0x36, 0x36, 0x33, 0x31, 0x00,  /* 'z' 90 */
    0x80, 0x80, 0xFC, 0x7E, 0x06, 0x02, 0x00,
    0x01, 0x01, 0x3F, 0x7E, 0x60, 0x40, 0x00,  /* '{' 91 */
    0x00, 0x00, 0xFC, 0xFC, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x3F, 0x3F, 0x00, 0x00, 0x00,  /* '|' 92 */
    0x02, 0x06, 0x7E, 0xFC, 0x80, 0x80, 0x00,
    0x40, 0x60, 0x7E, 0x3F, 0x01, 0x01, 0x00,  /* '}' 93 */
    0x00, 0x80, 0x80, 0x00, 0x80, 0x80, 0x00,
    0x03, 0x03, 0x01, 0x03, 0x03, 0x01, 0x00   /* '~' 94 */
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
