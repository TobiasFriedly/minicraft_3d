#include "mode4_fast.h"

static inline void mode4_plot_px(volatile u8* dst, int x, int y, u8 c){
    if((unsigned)x >= (unsigned)g_rw || (unsigned)y >= (unsigned)g_rh) return;
    volatile u16* cell = (volatile u16*)dst + (y * (SCREEN_W >> 1)) + (x >> 1);
    u16 px = *cell;
    if(x & 1) px = (px & 0x00FFu) | ((u16)c << 8);
    else      px = (px & 0xFF00u) | c;
    *cell = px;
}

static inline void fill_rect(volatile u8* dst,int x,int y,int w,int h,u8 c){
    int x1 = x + w - 1;
    int y1 = y + h - 1;
    if(w <= 0 || h <= 0) return;
    if(x >= g_rw || y >= g_rh || x1 < 0 || y1 < 0) return;
    if(x < 0){ w += x; x = 0; }
    if(y < 0){ h += y; y = 0; }
    if(x + w > g_rw) w = g_rw - x;
    if(y + h > g_rh) h = g_rh - y;
    if(w <= 0 || h <= 0) return;
    for(int yy = 0; yy < h; yy++){
        mode4_fill_span_asm((u16*)dst + ((y + yy) * (SCREEN_W >> 1)), x, w, c);
    }
}

static inline void draw_rect(volatile u8* dst,int x,int y,int w,int h,u8 c){
    if(w <= 0 || h <= 0) return;
    fill_rect(dst, x, y, w, 1, c);
    fill_rect(dst, x, y + h - 1, w, 1, c);
    fill_rect(dst, x, y, 1, h, c);
    fill_rect(dst, x + w - 1, y, 1, h, c);
}

static const u8 font3x5_rows[128][5] = {
    [' '] = {0,0,0,0,0},
    ['!'] = {0b010,0b010,0b010,0b000,0b010},
    ['+'] = {0b000,0b010,0b111,0b010,0b000},
    ['-'] = {0b000,0b000,0b111,0b000,0b000},
    ['.'] = {0b000,0b000,0b000,0b000,0b010},
    ['/'] = {0b001,0b001,0b010,0b100,0b100},
    ['0'] = {0b111,0b101,0b101,0b101,0b111},
    ['1'] = {0b010,0b110,0b010,0b010,0b111},
    ['2'] = {0b111,0b001,0b111,0b100,0b111},
    ['3'] = {0b111,0b001,0b111,0b001,0b111},
    ['4'] = {0b101,0b101,0b111,0b001,0b001},
    ['5'] = {0b111,0b100,0b111,0b001,0b111},
    ['6'] = {0b111,0b100,0b111,0b101,0b111},
    ['7'] = {0b111,0b001,0b010,0b100,0b100},
    ['8'] = {0b111,0b101,0b111,0b101,0b111},
    ['9'] = {0b111,0b101,0b111,0b001,0b111},
    [':'] = {0b000,0b010,0b000,0b010,0b000},
    ['A'] = {0b010,0b101,0b111,0b101,0b101},
    ['B'] = {0b110,0b101,0b110,0b101,0b110},
    ['C'] = {0b011,0b100,0b100,0b100,0b011},
    ['D'] = {0b110,0b101,0b101,0b101,0b110},
    ['E'] = {0b111,0b100,0b110,0b100,0b111},
    ['F'] = {0b111,0b100,0b110,0b100,0b100},
    ['G'] = {0b011,0b100,0b101,0b101,0b011},
    ['H'] = {0b101,0b101,0b111,0b101,0b101},
    ['I'] = {0b111,0b010,0b010,0b010,0b111},
    ['J'] = {0b111,0b001,0b001,0b101,0b010},
    ['K'] = {0b101,0b110,0b100,0b110,0b101},
    ['L'] = {0b100,0b100,0b100,0b100,0b111},
    ['M'] = {0b101,0b111,0b111,0b101,0b101},
    ['N'] = {0b101,0b111,0b111,0b111,0b101},
    ['O'] = {0b111,0b101,0b101,0b101,0b111},
    ['P'] = {0b110,0b101,0b110,0b100,0b100},
    ['Q'] = {0b111,0b101,0b101,0b111,0b011},
    ['R'] = {0b110,0b101,0b110,0b110,0b101},
    ['S'] = {0b011,0b100,0b010,0b001,0b110},
    ['T'] = {0b111,0b010,0b010,0b010,0b010},
    ['U'] = {0b101,0b101,0b101,0b101,0b111},
    ['V'] = {0b101,0b101,0b101,0b101,0b010},
    ['W'] = {0b101,0b101,0b111,0b111,0b101},
    ['X'] = {0b101,0b101,0b010,0b101,0b101},
    ['Y'] = {0b101,0b101,0b010,0b010,0b010},
    ['Z'] = {0b111,0b001,0b010,0b100,0b111},
    ['['] = {0b110,0b100,0b100,0b100,0b110},
    [']'] = {0b011,0b001,0b001,0b001,0b011},
    ['('] = {0b010,0b100,0b100,0b100,0b010},
    [')'] = {0b010,0b001,0b001,0b001,0b010},
};

static void draw_glyph3x5(volatile u8* dst, int x, int y, const u8* glyph, int scale, u8 col){
    for(int ry = 0; ry < 5; ry++){
        u8 row = glyph[ry];
        if(!row) continue;
        int rx = 0;
        while(rx < 3){
            if((row & (1u << (2 - rx))) == 0){
                rx++;
                continue;
            }
            int run = rx;
            do {
                rx++;
            } while(rx < 3 && (row & (1u << (2 - rx))));
            fill_rect(dst, x + run * scale, y + ry * scale, (rx - run) * scale, scale, col);
        }
    }
}

static void draw_text3x5(volatile u8* dst,int x,int y,const char* s,int scale,u8 col){
    int cx = x;
    int cy = y;
    for(; *s; ++s){
        unsigned char ch = (unsigned char)*s;
        if(ch == 'n' || ch == '\n'){
            cy += (5 * scale) + scale;
            cx = x;
            continue;
        }
        if(ch >= 'a' && ch <= 'z') ch = (unsigned char)(ch - ('a' - 'A'));
        draw_glyph3x5(dst, cx, cy, font3x5_rows[ch < 128 ? ch : ' '], scale, col);
        cx += (3 * scale) + scale;
    }
}

static int text3x5_width(const char* s, int scale){
    int line_w = 0;
    int max_w = 0;
    for(; *s; ++s){
        if(*s == 'n' || *s == '\n'){
            if(line_w > max_w) max_w = line_w;
            line_w = 0;
            continue;
        }
        line_w += 4 * scale;
    }
    if(line_w > max_w) max_w = line_w;
    return max_w;
}

static void draw_text3x5_shadowed(volatile u8* dst, int x, int y, const char* s, int scale, u8 col, u8 shadow){
    draw_text3x5(dst, x + 1, y + 1, s, scale, shadow);
    draw_text3x5(dst, x, y, s, scale, col);
}

static void draw_text3x5_both(const char* text, int x, int y, int scale, u8 color){
    draw_text3x5(VRAM_PAGE0, x, y, text, scale, color);
    draw_text3x5(VRAM_PAGE1, x, y, text, scale, color);
}

static void draw_text3x5_shadowed_both(const char* text, int x, int y, int scale, u8 color, u8 shadow){
    draw_text3x5_shadowed(VRAM_PAGE0, x, y, text, scale, color, shadow);
    draw_text3x5_shadowed(VRAM_PAGE1, x, y, text, scale, color, shadow);
}

static void draw_text3x5_centered_both(const char* text, int y, int scale, u8 color){
    int text_w = text3x5_width(text, scale);
    if(text_w <= 0) return;
    draw_text3x5_both(text, (g_rw - text_w) >> 1, y, scale, color);
}

static void draw_text3x5_shadowed_centered_both(const char* text, int y, int scale, u8 color, u8 shadow){
    int text_w = text3x5_width(text, scale);
    if(text_w <= 0) return;
    draw_text3x5_shadowed_both(text, (g_rw - text_w) >> 1, y, scale, color, shadow);
}

static void draw_text3x5_centered_bb(const char* text, int y, int scale, u8 color){
    int text_w = text3x5_width(text, scale);
    if(text_w <= 0) return;
    draw_text3x5(backbuffer, (g_rw - text_w) >> 1, y, text, scale, color);
}

static void draw_text3x5_shadowed_centered_bb(const char* text, int y, int scale, u8 color, u8 shadow){
    int text_w = text3x5_width(text, scale);
    if(text_w <= 0) return;
    draw_text3x5_shadowed(backbuffer, (g_rw - text_w) >> 1, y, text, scale, color, shadow);
}
