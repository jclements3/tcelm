/*
 * framebuffer.h - Framebuffer graphics for SXGA display
 *
 * Uses multiboot video info to render text to framebuffer
 */

#ifndef FRAMEBUFFER_H
#define FRAMEBUFFER_H

#include "font8x16.h"

/* Multiboot info structure (partial) */
typedef struct {
    unsigned int flags;
    unsigned int mem_lower;
    unsigned int mem_upper;
    unsigned int boot_device;
    unsigned int cmdline;
    unsigned int mods_count;
    unsigned int mods_addr;
    unsigned int syms[4];
    unsigned int mmap_length;
    unsigned int mmap_addr;
    unsigned int drives_length;
    unsigned int drives_addr;
    unsigned int config_table;
    unsigned int boot_loader_name;
    unsigned int apm_table;
    /* VBE fields */
    unsigned int vbe_control_info;
    unsigned int vbe_mode_info;
    unsigned short vbe_mode;
    unsigned short vbe_interface_seg;
    unsigned short vbe_interface_off;
    unsigned short vbe_interface_len;
    /* Framebuffer fields (multiboot 1) */
    unsigned long long framebuffer_addr;
    unsigned int framebuffer_pitch;
    unsigned int framebuffer_width;
    unsigned int framebuffer_height;
    unsigned char framebuffer_bpp;
    unsigned char framebuffer_type;
} __attribute__((packed)) multiboot_info_t;

/* Framebuffer state */
static unsigned int *fb_addr = (void*)0;
static unsigned int fb_width = 0;
static unsigned int fb_height = 0;
static unsigned int fb_pitch = 0;
static unsigned int fb_bpp = 0;
static unsigned int cursor_x = 0;
static unsigned int cursor_y = 0;

/* Colors (ARGB format) */
#define FB_COLOR_BLACK      0x00000000
#define FB_COLOR_WHITE      0x00FFFFFF
#define FB_COLOR_GREEN      0x0000FF00
#define FB_COLOR_CYAN       0x0000FFFF
#define FB_COLOR_YELLOW     0x00FFFF00
#define FB_COLOR_ORANGE     0x00FF8800
#define FB_COLOR_GRAY       0x00808080
#define FB_COLOR_DARKGRAY   0x00404040

/* Default colors */
static unsigned int fg_color = FB_COLOR_GREEN;
static unsigned int bg_color = FB_COLOR_BLACK;

/* External reference to multiboot info pointer from boot.S */
extern unsigned int multiboot_info_ptr;

/* Initialize framebuffer from multiboot info */
static int fb_init(void) {
    multiboot_info_t *mbi = (multiboot_info_t *)(unsigned long)multiboot_info_ptr;

    if (!mbi) {
        return -1;
    }

    /* Check if framebuffer info is available (bit 12) */
    if (!(mbi->flags & (1 << 12))) {
        /* No framebuffer info - try VBE fallback or use default */
        /* For QEMU with -vga std, assume 1280x1024x32 at 0xFD000000 */
        fb_addr = (unsigned int *)0xFD000000;
        fb_width = 1280;
        fb_height = 1024;
        fb_pitch = 1280 * 4;
        fb_bpp = 32;
        return 0;
    }

    fb_addr = (unsigned int *)(unsigned long)mbi->framebuffer_addr;
    fb_width = mbi->framebuffer_width;
    fb_height = mbi->framebuffer_height;
    fb_pitch = mbi->framebuffer_pitch;
    fb_bpp = mbi->framebuffer_bpp;

    return 0;
}

/* Set pixel at (x, y) to color */
static inline void fb_putpixel(unsigned int x, unsigned int y, unsigned int color) {
    if (x < fb_width && y < fb_height) {
        unsigned int *pixel = (unsigned int *)((unsigned char *)fb_addr + y * fb_pitch + x * 4);
        *pixel = color;
    }
}

/* Fill rectangle with color */
static void fb_fillrect(unsigned int x, unsigned int y, unsigned int w, unsigned int h, unsigned int color) {
    for (unsigned int j = 0; j < h; j++) {
        for (unsigned int i = 0; i < w; i++) {
            fb_putpixel(x + i, y + j, color);
        }
    }
}

/* Clear screen with background color */
static void fb_clear(void) {
    fb_fillrect(0, 0, fb_width, fb_height, bg_color);
    cursor_x = 0;
    cursor_y = 0;
}

/* Draw a single character at (x, y) */
static void fb_drawchar(unsigned int x, unsigned int y, char c) {
    if (c < 32 || c > 126) {
        c = '?';  /* Replace unprintable characters */
    }

    const unsigned char *glyph = font8x16_data[c - 32];

    for (int row = 0; row < FONT_HEIGHT; row++) {
        unsigned char bits = glyph[row];
        for (int col = 0; col < FONT_WIDTH; col++) {
            unsigned int color = (bits & (0x80 >> col)) ? fg_color : bg_color;
            fb_putpixel(x + col, y + row, color);
        }
    }
}

/* Print a character, handling cursor movement */
static void fb_putc(char c) {
    unsigned int max_cols = fb_width / FONT_WIDTH;
    unsigned int max_rows = fb_height / FONT_HEIGHT;

    if (c == '\n') {
        cursor_x = 0;
        cursor_y++;
    } else if (c == '\r') {
        cursor_x = 0;
    } else if (c == '\t') {
        cursor_x = (cursor_x + 8) & ~7;
    } else {
        fb_drawchar(cursor_x * FONT_WIDTH, cursor_y * FONT_HEIGHT, c);
        cursor_x++;
    }

    /* Wrap at end of line */
    if (cursor_x >= max_cols) {
        cursor_x = 0;
        cursor_y++;
    }

    /* Scroll if at bottom (simple: just wrap to top for now) */
    if (cursor_y >= max_rows) {
        cursor_y = 0;
    }
}

/* Print a string */
static void fb_print(const char *s) {
    while (*s) {
        fb_putc(*s++);
    }
}

/* Print a string with newline */
static void fb_println(const char *s) {
    fb_print(s);
    fb_putc('\n');
}

/* Set foreground color */
static void fb_setfg(unsigned int color) {
    fg_color = color;
}

/* Set background color */
static void fb_setbg(unsigned int color) {
    bg_color = color;
}

/* Draw a horizontal line */
static void fb_hline(unsigned int x, unsigned int y, unsigned int len, unsigned int color) {
    for (unsigned int i = 0; i < len; i++) {
        fb_putpixel(x + i, y, color);
    }
}

/* Draw a vertical line */
static void fb_vline(unsigned int x, unsigned int y, unsigned int len, unsigned int color) {
    for (unsigned int i = 0; i < len; i++) {
        fb_putpixel(x, y + i, color);
    }
}

/* Draw a rectangle outline */
static void fb_rect(unsigned int x, unsigned int y, unsigned int w, unsigned int h, unsigned int color) {
    fb_hline(x, y, w, color);
    fb_hline(x, y + h - 1, w, color);
    fb_vline(x, y, h, color);
    fb_vline(x + w - 1, y, h, color);
}

/* Set cursor position */
static void fb_gotoxy(unsigned int x, unsigned int y) {
    cursor_x = x;
    cursor_y = y;
}

#endif /* FRAMEBUFFER_H */
