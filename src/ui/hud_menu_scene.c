/* ===================== HUD & MENUS ========================= */
static inline void memset_page8(volatile u8* page, u8 val){
    u32 pat = (u32)val; pat |= pat<<8; pat |= pat<<16;
    CpuFastSet(&pat, (void*)page, ((SCREEN_W*SCREEN_H)>>2) | COPY32 | FILL);
}

static void draw_text3x5(volatile u8* dst,int x,int y,const char* s,int scale,u8 col);
static void draw_block_texture_preview(int x, int y, int size, u8 blk_id);

static void draw_block_bar(int active_slot){
    const int bar_h = 20 / g_render_scale;
    const int bar_y = g_rh - bar_h;
    const int slot_w = 22 / g_render_scale;
    const int slots = HOTBAR_SLOTS;
    const int bar_start_x = (g_rw - slots * slot_w) >> 1;
    
    u8 bg_color = 252;
    for(int y = bar_y; y < g_rh; y++){
        u16* ptr = (u16*)backbuffer + (y * (SCREEN_W>>1));
        u16 c2 = bg_color | (bg_color << 8);
        for(int x = bar_start_x - 4; x < bar_start_x + slots * slot_w + 4; x+=2) ptr[x>>1] = c2;
    }
    
    for(int i = 0; i < slots; i++){
        int x = bar_start_x + i * slot_w;
        bool is_selected = (i == active_slot);
        u8 border_color = is_selected ? 254 : 253;
        int border_width = (is_selected ? 2 : 1);
        if(border_width < 1) border_width = 1;

        for(int bw = 0; bw < border_width; bw++){
             int y1 = bar_y + bw; int y2 = g_rh - 1 - bw;
             for (int bx = x+bw; bx < x+slot_w-2-bw; bx++) {
                 u16* ptr = (u16*)backbuffer + (y1*(SCREEN_W>>1)) + (bx>>1);
                 if (bx&1) *ptr = (*ptr & 0x00FF) | (border_color<<8); else *ptr = (*ptr & 0xFF00) | border_color;
                 ptr = (u16*)backbuffer + (y2*(SCREEN_W>>1)) + (bx>>1);
                 if (bx&1) *ptr = (*ptr & 0x00FF) | (border_color<<8); else *ptr = (*ptr & 0xFF00) | border_color;
             }
        }

        if(g_game_mode == MODE_CREATIVE && i < QUICKBAR_SLOTS){
            u8 blk_id = g_quickbar[i];
            int cube_size = slot_w - 6;
            int max_h = bar_h - 4;
            if(max_h < cube_size) cube_size = max_h;
            if(cube_size < 8) cube_size = 8;

            int cube_x =  x + ( (slot_w - cube_size) >> 1 );
            int cube_y =  bar_y + ( (bar_h - cube_size) >> 1 );
            draw_block_texture_preview(cube_x, cube_y, cube_size, blk_id);
        } else if(g_game_mode == MODE_CREATIVE) {
            int scale = (g_render_scale > 1) ? 1 : 2;
            int text_w = 3 * 4 * scale;
            int tx = x + (slot_w - text_w) / 2;
            int ty = bar_y + (bar_h - 5 * scale) / 2 - 2;
            if(ty < bar_y + 1) ty = bar_y + 1;
            draw_text3x5(backbuffer, tx, ty, "...", scale, 255);
        } else {
            InventorySlot slot = g_inventory[i];
            if(slot.item && slot.count){
                if(item_is_block_item(slot.item)){
                    int cube_size = slot_w - 6;
                    int max_h = bar_h - 4;
                    if(max_h < cube_size) cube_size = max_h;
                    if(cube_size < 8) cube_size = 8;
                    int cube_x = x + ((slot_w - cube_size) >> 1);
                    int cube_y = bar_y + ((bar_h - cube_size) >> 1);
                    draw_block_texture_preview(cube_x, cube_y, cube_size, slot.item);
                } else {
                    int icon_size = slot_w - 6;
                    int max_h = bar_h - 4;
                    if(max_h < icon_size) icon_size = max_h;
                    if(icon_size < 8) icon_size = 8;
                    draw_item_preview_to(backbuffer, x + ((slot_w - icon_size) >> 1), bar_y + ((bar_h - icon_size) >> 1), icon_size, slot.item);
                }
                char buf[4];
                snprintf(buf, sizeof(buf), "%d", slot.count);
                draw_text3x5(backbuffer, x + slot_w - 8, bar_y + bar_h - 7, buf, 1, 255);
            }
        }
    }
}

#define RGB332_IDX(r3,g3,b2) ((u8)(((r3)<<5)|((g3)<<2)|(b2)))
#define DIRT_COLOR RGB332_IDX(3,2,0)
#define MENU_TEXT_WHITE 255
#define MENU_TEXT_SELECTED 254
#define MENU_TEXT_UNSELECTED 253
#define MENU_TEXT_LABEL 253
#define MENU_VERSION_TEXT "V. 0.5.1"
#define QR_WIDTH 29
#define QR_HEIGHT 29
#define MENU_QR_QUIET 2
#define MENU_QR_W (QR_WIDTH + MENU_QR_QUIET * 2)
#define MENU_QR_H (QR_HEIGHT + MENU_QR_QUIET * 2)

static const uint8_t qr_code[QR_HEIGHT][QR_WIDTH] = {
    {1,1,1,1,1,1,1,0,1,0,0,1,1,1,0,1,0,1,1,1,1,0,1,1,1,1,1,1,1},
    {1,0,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,1,1,0,0,0,1,0,0,0,0,0,1},
    {1,0,1,1,1,0,1,0,0,0,1,1,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,0,1},
    {1,0,1,1,1,0,1,0,1,1,1,0,0,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1},
    {1,0,1,1,1,0,1,0,0,1,0,1,1,0,1,1,1,0,0,1,0,0,1,0,1,1,1,0,1},
    {1,0,0,0,0,0,1,0,0,0,1,1,0,1,0,0,1,1,1,1,1,0,1,0,0,0,0,0,1},
    {1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1},
    {0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,1,1,0,0,0,0,0,0,0,0},
    {1,0,1,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,1,1,0,0,1,0,0,1,0,1,1},
    {1,0,1,0,0,1,0,1,1,1,0,0,1,1,1,0,1,0,1,1,1,0,1,1,1,0,0,0,1},
    {1,1,1,0,0,1,1,1,0,1,0,0,1,1,0,0,1,0,0,0,1,0,1,1,1,0,1,1,0},
    {1,0,1,0,1,1,0,0,0,0,1,0,0,0,0,1,0,0,1,0,1,1,1,1,1,0,0,0,1},
    {1,0,0,0,0,1,1,0,1,0,1,0,1,0,1,0,0,1,1,1,0,0,0,0,0,1,1,0,0},
    {1,1,1,1,0,0,0,1,1,1,0,1,0,0,1,1,1,0,0,1,1,0,1,1,0,0,1,1,1},
    {0,1,1,1,1,1,1,1,0,1,0,1,1,1,0,0,1,1,1,1,1,1,0,1,1,0,1,1,1},
    {0,0,1,0,0,1,0,0,1,0,0,0,0,0,1,1,1,0,0,0,1,0,1,1,1,0,0,1,0},
    {1,1,1,1,1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,0,1,1,0,0,1,1,0,1,0},
    {0,1,0,0,1,0,0,0,0,0,1,1,0,0,1,1,0,0,0,0,1,1,0,1,0,1,1,1,0},
    {1,0,0,0,0,0,1,1,1,0,1,0,0,1,1,1,0,1,0,0,0,0,0,1,0,0,1,0,0},
    {0,0,0,0,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,0,0},
    {0,1,0,1,0,1,1,1,1,1,1,0,0,1,1,0,1,1,1,1,1,1,1,1,1,1,1,0,0},
    {0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,1,0,1,0,1,0,0,0,1,1,1,1,1},
    {1,1,1,1,1,1,1,0,1,0,1,1,0,0,1,0,1,1,0,1,1,0,1,0,1,1,0,1,0},
    {1,0,0,0,0,0,1,0,1,1,0,0,1,1,1,1,0,0,1,0,1,0,0,0,1,1,0,0,1},
    {1,0,1,1,1,0,1,0,0,1,1,0,0,0,0,0,0,1,0,0,1,1,1,1,1,0,1,0,0},
    {1,0,1,1,1,0,1,0,1,0,1,0,0,0,0,1,1,0,0,1,1,1,0,0,1,1,0,0,1},
    {1,0,1,1,1,0,1,0,1,0,1,1,0,0,0,0,0,0,1,0,0,1,0,1,0,0,1,0,1},
    {1,0,0,0,0,0,1,0,0,1,1,0,0,1,0,1,1,0,0,1,1,0,0,1,0,1,0,1,0},
    {1,1,1,1,1,1,1,0,1,0,1,0,1,1,0,0,0,0,1,1,1,0,0,0,0,1,0,1,0},
};

typedef struct {
    fix cam_x;
    fix cam_y;
    fix cam_z;
    fix cam_x_start;
    fix cam_y_start;
    fix cam_z_start;
    fix cam_step_x;
    fix cam_step_y;
    fix cam_step_z;
    int yaw;
    int pitch;
    int loop_frames;
    int loop_frame;
} MenuSceneState;

typedef void (*MenuLoadProgressFn)(int percent);

static MenuSceneState g_menu_scene = {
    .cam_x = I2F(CW / 2) + F(0.5f),
    .cam_y = I2F(20) + F(0.25f),
    .cam_z = I2F(4) + F(0.5f),
    .cam_x_start = I2F(CW / 2) + F(0.5f),
    .cam_y_start = I2F(20) + F(0.25f),
    .cam_z_start = I2F(4) + F(0.5f),
    .cam_step_x = 0,
    .cam_step_y = 0,
    .cam_step_z = F(0.08f),
    .yaw = 0,
    .pitch = LUT_N / 36,
    .loop_frames = 220,
    .loop_frame = 0
};

#include "../data/menu_world_data.inc"
#include "../data/menu_baked_slot.inc"

static inline u16 rgb332_to_rgb555(u8 v){
    int r = (v >> 5) & 0x7;
    int g = (v >> 2) & 0x7;
    int b =  v       & 0x3;
    int R = (r * 31 + 3) / 7;
    int G = (g * 31 + 3) / 7;
    int B = (b * 31 + 1) / 3;
    if (R > 31) R = 31; if (G > 31) G = 31; if (B > 31) B = 31;
    return (u16)(R | (G<<5) | (B<<10));
}

static void menu_build_rgb332_palette(u16* out){
    for(int i=0;i<256;i++) out[i] = rgb332_to_rgb555((u8)i);
    out[251] = RGB5(0,0,0);
}

static void menu_draw_startup_loading(int percent){
    static int last_percent = -1;
    const int bar_w = 88;
    const int bar_h = 5;
    const int x = (SCREEN_W - bar_w) >> 1;
    const int y = SCREEN_H - 20;
    int fill_w;

    if(percent < 0) percent = 0;
    if(percent > 100) percent = 100;
    if(percent == last_percent) return;
    last_percent = percent;

    backbuffer = vram_backbuffer();
    CpuFastSet(image1, (void*)backbuffer, ((SCREEN_W * SCREEN_H) >> 2) | COPY32);
    fill_rect(backbuffer, x - 1, y - 1, bar_w + 2, bar_h + 2, 251);
    fill_rect(backbuffer, x, y, bar_w, bar_h, 0);
    fill_w = (bar_w * percent) / 100;
    if(fill_w > 0) fill_rect(backbuffer, x, y, fill_w, bar_h, 255);
    present_and_flip();
}

static void menu_world_load_baked(void){
    int out = 0;
    for(int i = 0; i < MENU_WORLD_RLE_SIZE; i += 3){
        int run = g_menu_world_rle[i] | (g_menu_world_rle[i + 1] << 8);
        u8 value = g_menu_world_rle[i + 2];
        for(int n = 0; n < run && out < (int)sizeof(world); n++) world[out++] = value;
    }
    while(out < (int)sizeof(world)) world[out++] = 0;
}

static bool menu_snapshot_valid(const u8* slot_data, SaveSlotHeader* out_hdr){
    SaveSlotHeader hdr;
    u32 stored = 0;
    u32 sum;
    u8 tmp[64];
    u32 remaining = DIM_EDIT_AREA_SIZE * DIMENSION_COUNT;
    u32 off = 0;

    memcpy(&hdr, slot_data, sizeof(hdr));
    if(hdr.tag != 0x534C4F54u || (hdr.version != 4 && hdr.version != LEGACY_SAVE_VERSION && hdr.version != SAVE_VERSION)) return false;
    memcpy(&stored, slot_data + SLOT_CHKSUM_OFF, sizeof(stored));
    sum = checksum32(slot_data, SLOT_HDR_PAD);
    while(remaining){
        u32 chunk = (remaining > sizeof(tmp)) ? (u32)sizeof(tmp) : remaining;
        memcpy(tmp, slot_data + SLOT_HDR_PAD + off, chunk);
        sum ^= checksum32(tmp, chunk);
        off += chunk;
        remaining -= chunk;
    }
    if(sum != stored) return false;
    if(out_hdr) *out_hdr = hdr;
    return true;
}

static void menu_set_camera_path(fix start_x, fix start_y, fix start_z, int yaw, int pitch, fix speed, int frames){
    int motion_pitch = pitch;
    int pitch_limit = LUT_N / 18;
    if(motion_pitch > pitch_limit) motion_pitch = pitch_limit;
    if(motion_pitch < -pitch_limit) motion_pitch = -pitch_limit;
    if(motion_pitch < (LUT_N / 96)) motion_pitch = LUT_N / 96;

    fix sy = fsin(yaw);
    fix cy = fcos(yaw);
    fix sp = fsin(motion_pitch);
    fix cp = fcos(motion_pitch);

    g_menu_scene.cam_x_start = start_x;
    g_menu_scene.cam_y_start = start_y;
    g_menu_scene.cam_z_start = start_z;
    g_menu_scene.cam_x = start_x;
    g_menu_scene.cam_y = start_y;
    g_menu_scene.cam_z = start_z;
    g_menu_scene.cam_step_x = fmuli(fmuli(sy, cp), speed);
    g_menu_scene.cam_step_y = fmuli(-sp, speed);
    g_menu_scene.cam_step_z = fmuli(fmuli(cy, cp), speed);
    g_menu_scene.yaw = yaw & (LUT_N - 1);
    g_menu_scene.pitch = motion_pitch;
    g_menu_scene.loop_frames = (frames < 1) ? 1 : frames;
    g_menu_scene.loop_frame = 0;
}

static bool menu_apply_camera_from_snapshot(const u8* slot_data, int dim){
    SaveSlotHeader hdr;
    int menu_off_x = 0;
    int menu_off_z = 0;

    if(!slot_data || !menu_snapshot_valid(slot_data, &hdr)) return false;
    if(dim < 0 || dim >= DIMENSION_COUNT) return false;

    g_dimension = dim;

    if(hdr.dim_state[dim].valid){
        menu_off_x = clamp_world_offset_x(F2I(hdr.dim_state[dim].posx) - (CW / 2));
        menu_off_z = clamp_world_offset_z(F2I(hdr.dim_state[dim].posz) - (CD / 2));
        g_world_offset_x = menu_off_x;
        g_world_offset_z = menu_off_z;

        fix start_x = hdr.dim_state[dim].posx - I2F(menu_off_x);
        fix start_y = hdr.dim_state[dim].posy + F(0.9f);
        fix start_z = hdr.dim_state[dim].posz - I2F(menu_off_z);
        if(start_x < F(1.0f)) start_x = F(1.0f);
        if(start_x > I2F(CW - 2)) start_x = I2F(CW - 2);
        if(start_y < F(2.0f)) start_y = F(2.0f);
        if(start_y > I2F(CH - 3)) start_y = I2F(CH - 3);
        if(start_z < F(1.0f)) start_z = F(1.0f);
        if(start_z > I2F(CD - 2)) start_z = I2F(CD - 2);
        menu_set_camera_path(start_x, start_y, start_z,
                             hdr.dim_state[dim].yaw,
                             hdr.dim_state[dim].pitch,
                             F(0.08f), 180);
        return true;
    }
    return false;
}

static void menu_apply_snapshot_dimension_edits(const u8* slot_data, int dim, u16 count, u32 version){
    const u8* s;
    if(!slot_data || dim < 0 || dim >= DIMENSION_COUNT) return;
    if(count > MAX_EDITS) count = MAX_EDITS;
    s = slot_data + SLOT_HDR_PAD + (dim * DIM_EDIT_AREA_SIZE);
    for(u16 i = 0; i < count; i++){
        u32 o = (u32)i * 4u;
        u32 raw = (u32)s[o + 0]
                | ((u32)s[o + 1] << 8)
                | ((u32)s[o + 2] << 16)
                | ((u32)s[o + 3] << 24);
        u32 e = (version == SAVE_VERSION) ? raw : convert_legacy_edit(raw);
        int gx = edit_x(e);
        int gy = edit_y(e);
        int gz = edit_z(e);
        u8 val = edit_val(e);
        int lx = local_x_from_global(gx);
        int lz = local_z_from_global(gz);
        if(lx >= 0 && lx < CW && lz >= 0 && lz < CD && gy >= 0 && gy < CH){
            world[widx(lx, gy, lz)] = val;
        }
    }
}

static bool menu_find_nether_lava_camera(int* out_x, int* out_y, int* out_z){
    int best_score = -1;
    int best_x = CW / 2;
    int best_z = 8;

    for(int z = 6; z < CD - 26; z++){
        for(int x = 5; x < CW - 5; x++){
            int score = 0;
            bool clear = true;
            if(world[widx(x, 11, z)] != BLK_LAVA + 1) continue;
            for(int y = 12; y <= 18; y++){
                if(world[widx(x, y, z)] != 0){
                    clear = false;
                    break;
                }
            }
            if(!clear) continue;

            for(int dz = 0; dz <= 16; dz++){
                for(int dx = -4; dx <= 4; dx++){
                    int lx = x + dx;
                    int lz = z + dz;
                    if(lx < 1 || lx >= CW - 1 || lz < 1 || lz >= CD - 1) continue;
                    if(world[widx(lx, 11, lz)] == BLK_LAVA + 1) score += 4;
                    if(world[widx(lx, 12, lz)] == 0) score += 1;
                    if(world[widx(lx, 13, lz)] == 0) score += 1;
                }
            }
            if(score > best_score){
                best_score = score;
                best_x = x;
                best_z = z;
            }
        }
    }

    if(best_score < 0) return false;
    if(out_x) *out_x = best_x;
    if(out_y) *out_y = 14;
    if(out_z) *out_z = best_z;
    return true;
}

static bool menu_build_live_nether_scene(const u8* slot_data, MenuLoadProgressFn progress_cb){
    SaveSlotHeader hdr;
    int center_gx = VW / 2;
    int center_gz = VD / 2;
    int cam_x = CW / 2;
    int cam_y = 14;
    int cam_z = 8;

    if(slot_data && menu_snapshot_valid(slot_data, &hdr)){
        g_world_seed = hdr.seed;
        g_world_type = (hdr.world_type <= WORLD_HIGHLANDS) ? (WorldType)hdr.world_type : WORLD_CLASSIC;
        if(hdr.dim_state[1].valid){
            center_gx = F2I(hdr.dim_state[1].posx);
            center_gz = F2I(hdr.dim_state[1].posz);
        }
    } else {
        memset(&hdr, 0, sizeof(hdr));
        g_world_seed = 0x4E455448u;
        g_world_type = WORLD_CLASSIC;
    }

    g_dimension = 1;
    g_world_offset_x = 0;
    g_world_offset_z = 0;
    stream_reset_state();
    stream_center_on_global(center_gx, center_gz);
    gen_world_with_progress_callback(8, 86, progress_cb);
    if(progress_cb) progress_cb(90);
    if(slot_data && hdr.edit_count[1]) menu_apply_snapshot_dimension_edits(slot_data, 1, hdr.edit_count[1], hdr.version);

    g_world_offset_x = clamp_world_offset_x(center_gx - (CW / 2));
    g_world_offset_z = clamp_world_offset_z(center_gz - (CD / 2));
    if(!menu_find_nether_lava_camera(&cam_x, &cam_y, &cam_z)){
        cam_x = CW / 2;
        cam_y = 14;
        cam_z = 8;
    }
    menu_set_camera_path(I2F(cam_x) + F(0.5f), I2F(cam_y) + F(0.5f), I2F(cam_z) + F(0.5f),
                         0, LUT_N / 56, F(0.07f), 320);
    if(progress_cb) progress_cb(100);
    return true;
}

static bool menu_choose_nether_scene(void){
    u32 s = g_menu_rng_state ? g_menu_rng_state : 0xA531D24Bu;
    s ^= s << 13;
    s ^= s >> 17;
    s ^= s << 5;
    if(s == 0) s = 0x6D2B79F5u;
    g_menu_rng_state = s;
    menu_rng_save_state();
    return (s & 1u) != 0;
}

static void build_menu_scene_with_loading(void){
    memset(world, 0, sizeof(world));
    g_world_offset_x = 0;
    g_world_offset_z = 0;
    stream_reset_state();
    g_dimension = 0;
    menu_draw_startup_loading(0);
    if(menu_choose_nether_scene()){
        if(menu_build_live_nether_scene(g_menu_baked_slot, menu_draw_startup_loading)) return;
    }
    menu_draw_startup_loading(20);
    menu_world_load_baked();
    menu_draw_startup_loading(82);
    if(menu_apply_camera_from_snapshot(g_menu_baked_slot, 0)){
        menu_draw_startup_loading(100);
        return;
    }
    menu_set_camera_path(I2F(32) + F(0.5f), I2F(20) + F(0.25f), I2F(4) + F(0.5f), 0, LUT_N / 36, F(0.08f), 220);
    menu_draw_startup_loading(100);
}

static void menu_update_camera(void){
    if(g_menu_scene.loop_frame <= 0){
        g_menu_scene.cam_x = g_menu_scene.cam_x_start;
        g_menu_scene.cam_y = g_menu_scene.cam_y_start;
        g_menu_scene.cam_z = g_menu_scene.cam_z_start;
        g_menu_scene.loop_frame = 1;
    } else {
        g_menu_scene.cam_x += g_menu_scene.cam_step_x;
        g_menu_scene.cam_y += g_menu_scene.cam_step_y;
        g_menu_scene.cam_z += g_menu_scene.cam_step_z;
        g_menu_scene.loop_frame++;
        if(g_menu_scene.loop_frame >= g_menu_scene.loop_frames) g_menu_scene.loop_frame = 0;
    }
    pl.pos.x = g_menu_scene.cam_x;
    pl.pos.y = g_menu_scene.cam_y;
    pl.pos.z = g_menu_scene.cam_z;
    pl.vel.x = pl.vel.y = pl.vel.z = 0;
    pl.yaw = g_menu_scene.yaw;
    pl.pitch = g_menu_scene.pitch;
    pl.onGround = false;
}

static void menu_draw_qr_overlay_bb(void){
    int dst_x = 6;
    int dst_y = SCREEN_H - MENU_QR_H - 6;
    const int code_x = dst_x + MENU_QR_QUIET;
    const int code_y = dst_y + MENU_QR_QUIET;
    fill_rect(backbuffer, dst_x - 2, dst_y - 2, MENU_QR_W + 4, MENU_QR_H + 4, 251);
    fill_rect(backbuffer, dst_x, dst_y, MENU_QR_W, MENU_QR_H, 255);
    for(int y = 0; y < QR_HEIGHT; y++){
        for(int x = 0; x < QR_WIDTH; x++){
            if(qr_code[y][x]) mode4_plot_px(backbuffer, code_x + x, code_y + y, 251);
        }
    }
}

static void draw_text3x5_shadowed_both(const char* text, int x, int y, int scale, u8 color, u8 shadow);
static void draw_text3x5_shadowed_centered_bb(const char* text, int y, int scale, u8 color, u8 shadow);

static void draw_menu_live_overlay(int selected){
    draw_text3x5_shadowed_centered_bb("MINICRAFT 3D", 20, 4, MENU_TEXT_WHITE, 251);
    draw_text3x5_shadowed_centered_bb("START GAME", 94, 2, (selected == 0) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
    draw_text3x5_shadowed_centered_bb("CONTROLS", 116, 2, (selected == 1) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
    menu_draw_qr_overlay_bb();
    draw_text3x5_shadowed_both(MENU_VERSION_TEXT, SCREEN_W - (int)strlen(MENU_VERSION_TEXT) * 4 - 8, SCREEN_H - 14, 1, MENU_TEXT_WHITE, 251);
}

#include "mode4_ui_fast.c"

static EWRAM_DATA char g_toast_msg[32];
static int g_toast_timer = 0;

static void show_toast(const char* msg) {
    strncpy(g_toast_msg, msg, 31); g_toast_msg[31] = '\0'; g_toast_timer = 90;
}

static void render_toast(void) {
    if(g_toast_timer > 0) {
        int len=0; while(g_toast_msg[len]) len++;
        int scale = (g_render_scale > 1) ? 1 : 2;
        int w = len * 4 * scale + 4;
        int h = 7 * scale;
        int x = (g_rw - w) / 2;
        int y = g_rh - 35/g_render_scale;
        fill_rect(backbuffer, x, y, w, h, 253); 
        draw_text3x5(backbuffer, x+2, y+1, g_toast_msg, scale, 255);
        g_toast_timer--;
    }
}

static void progress_screen_begin(ProgressScreenState* state){
    state->saved_scale = g_render_scale;
    set_hardware_scale(1);
    g_render_scale = 1;
    update_render_dims();
}

static void progress_screen_end(const ProgressScreenState* state){
    g_render_scale = state->saved_scale;
    update_render_dims();
    set_hardware_scale(g_render_scale);
}

static void draw_progress_screen(const char* title, const char* status, int percent){
    if(percent < 0) percent = 0;
    if(percent > 100) percent = 100;
    backbuffer = vram_backbuffer();
    memset_page8((volatile u8*)backbuffer, 252);
    draw_text3x5_shadowed_centered_bb(title, 26, 2, 255, 251);
    char buf[16];
    int percent_scale = 4;
    int percent_w;
    int percent_x;
    snprintf(buf, sizeof(buf), "%d", percent);
    percent_w = text3x5_width(buf, percent_scale) - percent_scale;
    if(percent_w < 0) percent_w = 0;
    percent_x = (g_rw - percent_w) >> 1;
    draw_text3x5_shadowed(backbuffer, percent_x, (g_rh - (5 * percent_scale)) >> 1, buf, percent_scale, 254, 251);
    {
        int bar_w = 140;
        int bar_h = 12;
        int bar_x = (g_rw - bar_w) >> 1;
        int bar_y = 104;
        draw_rect(backbuffer, bar_x, bar_y, bar_w, bar_h, 253);
        fill_rect(backbuffer, bar_x + 1, bar_y + 1, bar_w - 2, bar_h - 2, 244);
        {
            int fill_w = ((bar_w - 2) * percent) / 100;
            if(fill_w > 0) fill_rect(backbuffer, bar_x + 1, bar_y + 1, fill_w, bar_h - 2, 254);
        }
    }
    if(status && status[0]){
        draw_text3x5_shadowed_centered_bb(status, 126, 1, 255, 251);
    }
    present_and_flip();
}

static void draw_flame_rect(int anchor_x, int base_y, int x_off, int y_off, int w, int h, bool flip, u8 color){
    if(w <= 0 || h <= 0) return;
    fill_rect(backbuffer, flip ? (anchor_x - x_off - w) : (anchor_x + x_off), base_y - y_off - h, w, h, color);
}

static void draw_flame_cluster(int anchor_x, int base_y, int width, int height, bool flip, int flicker){
    int wobble = (flicker & 1) ? 1 : 0;
    int tongue = height / 3;
    if(tongue < 5) tongue = 5;

    draw_flame_rect(anchor_x, base_y, 0, 0, width, height / 2, flip, 248);
    draw_flame_rect(anchor_x, base_y, width / 10, height / 6, (width * 4) / 5, (height * 2) / 3, flip, 249);
    draw_flame_rect(anchor_x, base_y, width / 3, (height * 2) / 3 - wobble, width / 3, tongue + wobble, flip, 249);
    draw_flame_rect(anchor_x, base_y, (width * 3) / 4, height / 4, width / 3, height / 5, flip, 249);
    draw_flame_rect(anchor_x, base_y, width / 6, height / 10, (width * 3) / 5, height / 4, flip, 254);
    draw_flame_rect(anchor_x, base_y, width / 3, height / 2, width / 5, tongue / 2, flip, 254);
}

static void draw_burn_overlay(void){
    if(g_burn_timer <= 0) return;
    int flicker = (int)(g_frame & 3);
    int cluster_w = g_rw / 4;
    int cluster_h = g_rh / 3;
    int center_w = g_rw / 5;
    int center_h = g_rh / 5;
    int base_y = g_rh + 2 + (flicker & 1);
    if(cluster_w < 18) cluster_w = 18;
    if(cluster_h < 18) cluster_h = 18;
    if(center_w < 14) center_w = 14;
    if(center_h < 12) center_h = 12;

    draw_flame_cluster(0, base_y, cluster_w, cluster_h + ((flicker == 0) ? 2 : 0), false, flicker);
    draw_flame_cluster(g_rw, base_y, cluster_w, cluster_h + ((flicker == 2) ? 2 : 0), true, flicker + 1);
    draw_flame_rect(g_rw / 2, g_rh + 2, center_w / 2, center_h / 3, center_w, center_h, false, 248);
    draw_flame_rect(g_rw / 2, g_rh + 2, center_w / 3, center_h / 2 - (flicker & 1), (center_w * 2) / 3, center_h, false, 249);
    draw_flame_rect(g_rw / 2, g_rh + 2, center_w / 2, center_h, center_w / 3, center_h / 2, false, 254);
}

static void render_textured(u8 selected_block, RayHit hit);

#include "mode4_preview_fast.c"

static void draw_block_icon(int x, int y, int size, u8 blk_id, bool selected){
    u8 border = selected ? 254 : 253;
    fill_rect(backbuffer, x, y, size, size, 252);
    for(int i=0; i<size; i++){
        u16* ptr = (u16*)backbuffer + ((y+i)*(SCREEN_W>>1));
        int lx = x;
        int rx = x + size - 1;
        if(lx & 1) ptr[lx>>1] = (ptr[lx>>1] & 0x00FF) | (border<<8);
        else ptr[lx>>1] = (ptr[lx>>1] & 0xFF00) | border;
        if(rx & 1) ptr[rx>>1] = (ptr[rx>>1] & 0x00FF) | (border<<8);
        else ptr[rx>>1] = (ptr[rx>>1] & 0xFF00) | border;
    }
    for(int i=0; i<size; i++){
        int ty = y;
        int by = y + size - 1;
        u16* ptr_t = (u16*)backbuffer + (ty*(SCREEN_W>>1)) + ((x+i)>>1);
        u16* ptr_b = (u16*)backbuffer + (by*(SCREEN_W>>1)) + ((x+i)>>1);
        if((x+i)&1){ *ptr_t = (*ptr_t & 0x00FF) | (border<<8); *ptr_b = (*ptr_b & 0x00FF) | (border<<8); }
        else { *ptr_t = (*ptr_t & 0xFF00) | border; *ptr_b = (*ptr_b & 0xFF00) | border; }
    }
    int inner = size - 2;
    draw_item_preview(x + 1, y + 1, inner, blk_id);
}

static void show_block_inventory(u8* io_curBlk){
    int saved_scale = g_render_scale;
    set_hardware_scale(1); g_render_scale = 1; update_render_dims();
    int cols = 6;
    int rows = 6;
    int cell = 18;
    int margin_x = (g_rw - cols * cell) / 2;
    int margin_y = 22;
    int total_rows = (BUILDABLE_COUNT + cols - 1) / cols;
    int selected = buildable_index_from_block(*io_curBlk);
    int row_offset = selected / cols;
    bool wait_release = true;
    if(row_offset > rows/2) row_offset -= rows/2; else row_offset = 0;
    if(row_offset > total_rows - rows) row_offset = total_rows - rows;
    if(row_offset < 0) row_offset = 0;

    while(1){
        render_textured(*io_curBlk, (RayHit){0});
        fill_rect(backbuffer, 0, 0, g_rw, 16, 252);
        fill_rect(backbuffer, 0, g_rh - 16, g_rw, 16, 252);
        draw_text3x5_centered_bb("BLOCK SELECTOR", 4, 2, 255);
        for(int r=0; r<rows; r++){
            for(int c=0; c<cols; c++){
                int idx = (row_offset + r) * cols + c;
                if(idx >= BUILDABLE_COUNT) continue;
                int x = margin_x + c * cell;
                int y = margin_y + r * cell;
                u8 blk = block_id_from_buildable(g_buildable_blocks[idx]);
                draw_block_icon(x, y, cell - 2, blk, idx == selected);
            }
        }
        draw_text3x5_centered_bb(g_buildable_names[selected], g_rh - 14, 2, 255);
        present_and_flip();
        vblank(); scanKeys();
        u16 kd = keysDown();
        u16 kh = keysHeld();
        if(wait_release){
            if((kh & (KEY_A | KEY_B | KEY_SELECT | KEY_START | KEY_RIGHT | KEY_LEFT | KEY_UP | KEY_DOWN | KEY_R | KEY_L)) == 0){
                wait_release = false;
            }
            continue;
        }
        if(kd & (KEY_B | KEY_START)) break;
        if(kd & KEY_A){
            u8 chosen = block_id_from_buildable(g_buildable_blocks[selected]);
            *io_curBlk = chosen;
            if(g_hotbar_prev_slot < 0 || g_hotbar_prev_slot >= QUICKBAR_SLOTS) g_hotbar_prev_slot = 0;
            g_quickbar[g_hotbar_prev_slot] = chosen;
            g_hotbar_slot = g_hotbar_prev_slot;
            show_toast(g_buildable_names[selected]);
            break;
        }
        if(kd & KEY_LEFT){
            if(selected % cols) selected--;
        }
        if(kd & KEY_RIGHT){
            if((selected % cols) < cols - 1 && selected + 1 < BUILDABLE_COUNT) selected++;
        }
        if(kd & KEY_UP){
            if(selected - cols >= 0) selected -= cols;
        }
        if(kd & KEY_DOWN){
            if(selected + cols < BUILDABLE_COUNT) selected += cols;
        }
        int sel_row = selected / cols;
        if(sel_row < row_offset) row_offset = sel_row;
        if(sel_row >= row_offset + rows) row_offset = sel_row - rows + 1;
        if(row_offset > total_rows - rows) row_offset = total_rows - rows;
        if(row_offset < 0) row_offset = 0;
    }
    g_render_scale = saved_scale;
    update_render_dims();
    set_hardware_scale(g_render_scale);
}

static void draw_item_slot(int x, int y, int size, InventorySlot slot, bool selected){
    u8 border = selected ? 254 : 253;
    draw_rect(backbuffer, x, y, size, size, border);
    if(slot.item == ITEM_NONE || slot.count == 0) return;
    fill_rect(backbuffer, x + 2, y + 2, size - 4, size - 4, 252);
    draw_item_preview(x + 2, y + 2, size - 4, slot.item);
    char buf[4];
    snprintf(buf, sizeof(buf), "%d", slot.count);
    draw_text3x5(backbuffer, x + size - 8, y + size - 7, buf, 1, 255);
}

static void draw_recipe_item_count(int x, int y, u8 item, u8 count){
    draw_rect(backbuffer, x, y, 20, 20, 253);
    fill_rect(backbuffer, x + 1, y + 1, 18, 18, 252);
    draw_item_preview(x + 2, y + 2, 16, item);
    char buf[4];
    snprintf(buf, sizeof(buf), "%d", count);
    draw_text3x5(backbuffer, x + 12, y + 13, buf, 1, 255);
}

static void show_survival_inventory(u8* io_curBlk){
    int saved_scale = g_render_scale;
    set_hardware_scale(1);
    g_render_scale = 1;
    update_render_dims();
    int page = 0;
    int selected = 0;
    bool wait_release = true;
    while(1){
        backbuffer = vram_backbuffer();
        memset_page8((volatile u8*)backbuffer, 252);
        if(page == 0){
            draw_text3x5_centered_bb("INVENTORY", 10, 2, 255);
            for(int i=0; i<SURVIVAL_INV_SLOTS; i++){
                int col = i % 4;
                int row = i / 4;
                int x = 24 + col * 48;
                int y = 34 + row * 34;
                draw_item_slot(x, y, 28, g_inventory[i], i == selected);
                if(i < HOTBAR_SLOTS) draw_text3x5(backbuffer, x, y - 8, "HB", 1, 254);
            }
            draw_text3x5_centered_bb("A USE/SWAP  L/R PAGE  B EXIT", SCREEN_H - 12, 2, 254);
        } else {
            draw_text3x5_centered_bb("CRAFTING", 10, 3, 255);
            int craftable[BUILDABLE_COUNT];
            int craftable_count = 0;
            for(int i=0; i<recipe_count(); i++){
                if(recipe_is_craftable(&g_recipes[i])) craftable[craftable_count++] = i;
            }
            if(craftable_count <= 0){
                selected = 0;
                draw_text3x5_centered_bb("NOTHING CRAFTABLE", 68, 2, 253);
            } else {
                if(selected >= craftable_count) selected = craftable_count - 1;
                int y = 28;
                for(int row=0; row<craftable_count; row++){
                    const RecipeDef* r = &g_recipes[craftable[row]];
                    u8 border = (row == selected) ? 254 : 253;
                    draw_rect(backbuffer, 12, y - 2, g_rw - 24, 26, border);
                    draw_recipe_item_count(18, y + 1, r->in1, r->count1);
                    if(r->in2 != ITEM_NONE){
                        draw_text3x5(backbuffer, 42, y + 7, "+", 1, 255);
                        draw_recipe_item_count(50, y + 1, r->in2, r->count2);
                    }
                    draw_text3x5(backbuffer, 94, y + 7, "->", 1, 255);
                    draw_recipe_item_count(116, y + 1, r->out, r->out_count);
                    y += 28;
                }
            }
            draw_text3x5_centered_bb("A CRAFT  L/R PAGE  B EXIT", SCREEN_H - 12, 2, 254);
        }
        present_and_flip();
        vblank();
        scanKeys();
        u16 kd = keysDown();
        u16 kh = keysHeld();
        if(wait_release){
            if((kh & (KEY_A | KEY_B | KEY_SELECT | KEY_START | KEY_RIGHT | KEY_LEFT | KEY_UP | KEY_DOWN | KEY_R | KEY_L)) == 0){
                wait_release = false;
            }
            continue;
        }
        if(kd & (KEY_B | KEY_START)) break;
        if(kd & KEY_L){ page = (page + 1) & 1; selected = 0; continue; }
        if(kd & KEY_R){ page = (page + 1) & 1; selected = 0; continue; }
        if(page == 0){
            if(kd & KEY_LEFT){ if((selected % 4) > 0) selected--; }
            if(kd & KEY_RIGHT){ if((selected % 4) < 3 && selected + 1 < SURVIVAL_INV_SLOTS) selected++; }
            if(kd & KEY_UP){ if(selected - 4 >= 0) selected -= 4; }
            if(kd & KEY_DOWN){ if(selected + 4 < SURVIVAL_INV_SLOTS) selected += 4; }
            if(kd & KEY_A){
                if(selected < HOTBAR_SLOTS){
                    g_hotbar_slot = selected;
                    refresh_selected_item(io_curBlk);
                    if(*io_curBlk) show_toast(item_name(*io_curBlk));
                } else {
                    InventorySlot tmp = g_inventory[g_hotbar_slot];
                    g_inventory[g_hotbar_slot] = g_inventory[selected];
                    g_inventory[selected] = tmp;
                    refresh_selected_item(io_curBlk);
                }
            }
        } else {
            int craftable[BUILDABLE_COUNT];
            int craftable_count = 0;
            for(int i=0; i<recipe_count(); i++){
                if(recipe_is_craftable(&g_recipes[i])) craftable[craftable_count++] = i;
            }
            if(kd & KEY_UP){ if(selected > 0) selected--; }
            if(kd & KEY_DOWN){ if(selected + 1 < craftable_count) selected++; }
            if(kd & KEY_A){
                if(craftable_count > 0 && craft_recipe(craftable[selected])){
                    refresh_selected_item(io_curBlk);
                    show_toast(item_name(g_recipes[craftable[selected]].out));
                }
            }
        }
    }
    g_render_scale = saved_scale;
    update_render_dims();
    set_hardware_scale(g_render_scale);
}

