#ifndef MODE4_FAST_H
#define MODE4_FAST_H

extern void mode4_fill_span_asm(u16* dst_line, int x, int width, u32 color);
extern void mode4_draw_span_affine_asm(u16* dst_line, int x, int width,
                                       fix u, fix v, fix du_dx, fix dv_dx,
                                       const u8* tex);

static inline void mode4_blit_tex8_unclipped(volatile u8* dst, int x, int y, const u8* tex){
    for(int ty = 0; ty < 16; ty++){
        const u8* src = tex + (ty << 4);
        volatile u16* row = (volatile u16*)dst + ((y + ty) * (SCREEN_W >> 1)) + (x >> 1);
        if((x & 1) == 0){
            row[0] = (u16)(src[0] | ((u16)src[1] << 8));
            row[1] = (u16)(src[2] | ((u16)src[3] << 8));
            row[2] = (u16)(src[4] | ((u16)src[5] << 8));
            row[3] = (u16)(src[6] | ((u16)src[7] << 8));
            row[4] = (u16)(src[8] | ((u16)src[9] << 8));
            row[5] = (u16)(src[10] | ((u16)src[11] << 8));
            row[6] = (u16)(src[12] | ((u16)src[13] << 8));
            row[7] = (u16)(src[14] | ((u16)src[15] << 8));
        } else {
            u16 edge = row[0];
            row[0] = (u16)((edge & 0x00FFu) | ((u16)src[0] << 8));
            row[1] = (u16)(src[1] | ((u16)src[2] << 8));
            row[2] = (u16)(src[3] | ((u16)src[4] << 8));
            row[3] = (u16)(src[5] | ((u16)src[6] << 8));
            row[4] = (u16)(src[7] | ((u16)src[8] << 8));
            row[5] = (u16)(src[9] | ((u16)src[10] << 8));
            row[6] = (u16)(src[11] | ((u16)src[12] << 8));
            row[7] = (u16)(src[13] | ((u16)src[14] << 8));
            edge = row[8];
            row[8] = (u16)((edge & 0xFF00u) | src[15]);
        }
    }
}

static inline void mode4_blit_tex8_scaled(volatile u8* dst, int x, int y, int size, const u8* tex){
    if(size <= 0) return;
    for(int py0 = 0; py0 < size; py0++){
        int py = y + py0;
        if(py < 0 || py >= g_rh) continue;
        int sy = (py0 * 16) / size;
        const u8* src_row = tex + (sy << 4);
        int run_start = x;
        int sx0 = 0;
        u8 run_col = src_row[0];
        for(int px0 = 1; px0 < size; px0++){
            int sx = (px0 * 16) / size;
            u8 col = src_row[sx];
            if(col == run_col) continue;
            mode4_fill_span_asm((u16*)dst + (py * (SCREEN_W >> 1)), run_start, px0 - sx0, run_col);
            run_start = x + px0;
            sx0 = px0;
            run_col = col;
        }
        mode4_fill_span_asm((u16*)dst + (py * (SCREEN_W >> 1)), run_start, size - sx0, run_col);
    }
}

static inline void mode4_blit_pal8_scaled(volatile u8* dst, int x, int y, int size,
                                          const u8* pattern, const u8* colors){
    int scale = size / 8;
    if(scale < 1) scale = 1;
    int draw_w = 8 * scale;
    int draw_h = 8 * scale;
    int ox = x + ((size - draw_w) >> 1);
    int oy = y + ((size - draw_h) >> 1);
    for(int ty = 0; ty < 8; ty++){
        const u8* src_row = pattern + (ty << 3);
        int row_y = oy + ty * scale;
        for(int sy = 0; sy < scale; sy++){
            int py = row_y + sy;
            if(py < 0 || py >= g_rh) continue;
            int run_start = -1;
            u8 run_col = 0;
            int run_len = 0;
            for(int tx = 0; tx < 8; tx++){
                u8 idx = src_row[tx];
                if(!idx){
                    if(run_len){
                        mode4_fill_span_asm((u16*)dst + (py * (SCREEN_W >> 1)), run_start, run_len * scale, run_col);
                        run_len = 0;
                    }
                    continue;
                }
                u8 col = colors[idx];
                int px = ox + tx * scale;
                if(run_len && col == run_col && px == (run_start + run_len * scale)){
                    run_len++;
                    continue;
                }
                if(run_len){
                    mode4_fill_span_asm((u16*)dst + (py * (SCREEN_W >> 1)), run_start, run_len * scale, run_col);
                }
                run_start = px;
                run_col = col;
                run_len = 1;
            }
            if(run_len){
                mode4_fill_span_asm((u16*)dst + (py * (SCREEN_W >> 1)), run_start, run_len * scale, run_col);
            }
        }
    }
}

#endif
