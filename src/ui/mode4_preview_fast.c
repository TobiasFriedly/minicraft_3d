#include "mode4_fast.h"

static const u8 sprite_stick[64] = {
    0,0,0,0,0,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,1,2,0,0,0,
    0,0,0,0,0,0,0,0
};

static const u8 sprite_flint[64] = {
    0,0,0,0,0,0,0,0,
    0,0,1,1,0,0,0,0,
    0,1,2,2,1,0,0,0,
    0,1,2,3,2,1,0,0,
    0,0,1,2,3,1,0,0,
    0,0,0,1,2,1,0,0,
    0,0,0,0,1,0,0,0,
    0,0,0,0,0,0,0,0
};

static const u8 sprite_flint_steel[64] = {
    0,0,0,1,1,0,0,0,
    0,0,1,2,2,1,0,0,
    0,0,1,2,2,1,0,0,
    0,1,2,2,1,0,3,0,
    1,2,2,1,0,3,4,3,
    0,1,1,0,0,0,3,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0
};

static const u8 sprite_pickaxe[64] = {
    0,0,1,1,1,1,1,0,
    0,1,2,2,2,2,2,1,
    0,0,0,0,3,0,0,0,
    0,0,0,0,3,0,0,0,
    0,0,0,0,3,0,0,0,
    0,0,0,0,3,0,0,0,
    0,0,0,0,3,0,0,0,
    0,0,0,0,3,0,0,0
};

static bool item_sprite_art(u8 item, const u8** out_pattern, u8* colors){
    const u8* pattern = NULL;
    colors[0] = 0;
    colors[1] = 251;
    colors[2] = 250;
    colors[3] = 253;
    colors[4] = 254;
    switch(item){
        case ITEM_STICK:
            pattern = sprite_stick;
            colors[1] = (u8)((BLK_WOOD << 3) | 2);
            colors[2] = (u8)((BLK_WOOD << 3) | 5);
            break;
        case ITEM_FLINT:
            pattern = sprite_flint;
            colors[1] = 251;
            colors[2] = 250;
            colors[3] = 255;
            break;
        case ITEM_FLINT_STEEL:
            pattern = sprite_flint_steel;
            colors[1] = 251;
            colors[2] = 250;
            colors[3] = 249;
            colors[4] = 254;
            break;
        case ITEM_PICK_WOOD:
            pattern = sprite_pickaxe;
            colors[1] = 251;
            colors[2] = (u8)((BLK_PLANK << 3) | 5);
            colors[3] = (u8)((BLK_WOOD << 3) | 4);
            break;
        case ITEM_PICK_STONE:
            pattern = sprite_pickaxe;
            colors[1] = 251;
            colors[2] = (u8)((BLK_STONE << 3) | 6);
            colors[3] = (u8)((BLK_WOOD << 3) | 4);
            break;
        case ITEM_PICK_IRON:
            pattern = sprite_pickaxe;
            colors[1] = 251;
            colors[2] = 255;
            colors[3] = (u8)((BLK_WOOD << 3) | 4);
            break;
        default:
            return false;
    }
    *out_pattern = pattern;
    return true;
}

static void draw_pattern_sprite_to(volatile u8* dst, int x, int y, int size, const u8* pattern, const u8* colors){
    mode4_blit_pal8_scaled(dst, x, y, size, pattern, colors);
}

static void draw_pattern_sprite_perspective_to(volatile u8* dst, int x, int y, int size, const u8* pattern, const u8* colors){
    int height = size;
    int row_h = height / 8;
    int base_w = (size * 7) / 8;
    int shear = size / 18;
    int taper = size / 5;
    if(row_h < 2) row_h = 2;
    if(base_w < 24) base_w = 24;
    if(shear < 1) shear = 1;
    if(taper < 4) taper = 4;

    for(int py = 0; py < 8; py++){
        int row_y = y + py * row_h;
        int row_x = x + py * shear;
        int row_w = base_w - (py * taper) / 7;
        if(row_w < 12) row_w = 12;
        int run_start = -1;
        u8 run_col = 0;
        int run_width = 0;
        for(int px = 0; px < 8; px++){
            u8 idx = pattern[py * 8 + px];
            if(!idx){
                if(run_width){
                    fill_rect(dst, run_start, row_y, run_width, row_h, run_col);
                    run_width = 0;
                }
                continue;
            }
            u8 col = colors[idx];
            int px0 = row_x + (px * row_w) / 8;
            int px1 = row_x + ((px + 1) * row_w) / 8;
            if(px1 <= px0) px1 = px0 + 1;
            if(run_width && col == run_col && px0 == run_start + run_width){
                run_width += px1 - px0;
                continue;
            }
            if(run_width){
                fill_rect(dst, run_start, row_y, run_width, row_h, run_col);
            }
            run_start = px0;
            run_width = px1 - px0;
            run_col = col;
        }
        if(run_width){
            fill_rect(dst, run_start, row_y, run_width, row_h, run_col);
        }
    }
}

static void draw_item_sprite_to(volatile u8* dst, int x, int y, int size, u8 item){
    const u8* pattern = NULL;
    u8 colors[5];
    if(!item_sprite_art(item, &pattern, colors)) return;
    draw_pattern_sprite_to(dst, x, y, size, pattern, colors);
}

static void draw_hand_item_held(u8 item, int anim_timer){
    const u8* pattern = NULL;
    u8 colors[5];
    if(!item_sprite_art(item, &pattern, colors)) return;

    int size = 76 / g_render_scale;
    int base_x = g_rw - (48 / g_render_scale);
    int base_y = g_rh - (48 / g_render_scale);
    int anim_offset_y = 0;
    if(size < 34) size = 34;
    if(anim_timer > 0){
        fix s = fsin((15 - anim_timer) * (LUT_N / 30));
        anim_offset_y = F2I(fmuli(s, I2F(15 / g_render_scale)));
    }
    draw_pattern_sprite_perspective_to(backbuffer, base_x, base_y + anim_offset_y, size, pattern, colors);
}

static void draw_item_preview_to(volatile u8* dst, int x, int y, int size, u8 item){
    if(item_is_block_item(item)){
        draw_block_texture_preview_to(dst, x, y, size, item);
        return;
    }
    draw_item_sprite_to(dst, x, y, size, item);
}

static void draw_block_texture_preview_to(volatile u8* dst, int x, int y, int size, u8 blk_id){
    int blk_type = (int)blk_id - 1;
    u8 render_block = (u8)blk_type;
    if(blk_type == BLK_GRASS) render_block = BLK_GRASS_SIDE;
    if(blk_type == BLK_TRACK) render_block = BLK_TRACK;
    mode4_blit_tex8_scaled(dst, x, y, size, shaded_texture_for(render_block, 3));
}

static void draw_block_texture_preview(int x, int y, int size, u8 blk_id){
    draw_block_texture_preview_to(backbuffer, x, y, size, blk_id);
}

static void draw_item_preview(int x, int y, int size, u8 item){
    draw_item_preview_to(backbuffer, x, y, size, item);
}
