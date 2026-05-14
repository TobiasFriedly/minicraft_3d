/* ===================== RENDERER ============================ */
static const fix FAR_Z = F(40.0f);
static const fix g_near_z = F(0.25f);
enum { FACE_POSX=0, FACE_NEGX=1, FACE_POSY=2, FACE_NEGY=3, FACE_POSZ=4, FACE_NEGZ=5 };

typedef struct Face { 
    u8 x,y,z; 
    u8 dir; 
    u8 blk; 
    u16 next; 
} Face;

#define MAX_FACES 1600
static EWRAM_DATA Face facePool[MAX_FACES];
static int facePoolIdx = 0;

#define BUCKET_COUNT 512
#define FACE_NONE 0xFFFFu
static EWRAM_DATA u16 faceBuckets[BUCKET_COUNT] __attribute__((aligned(4)));

static inline __attribute__((unused)) bool air_or_outside(int x,int y,int z){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return true;
    u8 id = world[widx(x,y,z)];
    return (id == 0 || id == (BLK_PORTAL+1)); 
}

HOT_INLINE ARM_CODE void emit_face_bucketed_fast(int x, int y, int z, u8 blk, u8 dir, fix depth) {
    if(depth <= F(0.01f) || depth > FAR_Z) return;
    if(depth < g_near_z) depth = g_near_z;
    if(facePoolIdx >= g_face_limit) return;
    fix d12 = (depth << 3) + (depth << 2);
    int bucketIdx = F2I(d12);
    if(bucketIdx < 0) bucketIdx = 0;
    if(bucketIdx >= BUCKET_COUNT) bucketIdx = BUCKET_COUNT - 1;

    u16 idx = (u16)facePoolIdx++;
    Face* f = &facePool[idx];
    f->x = (u8)x; f->y = (u8)y; f->z = (u8)z; f->dir = dir; f->blk = blk;
    f->next = faceBuckets[bucketIdx];
    faceBuckets[bucketIdx] = idx;
}

HOT_INLINE ARM_CODE v3 world_to_cam(fix wx, fix wy, fix wz, const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos){
    v3 rel = { wx-cam_pos->x, wy-cam_pos->y, wz-cam_pos->z };
    v3 out;
    out.x = fmuli(rel.x, fx->x) + fmuli(rel.y, fx->y) + fmuli(rel.z, fx->z);
    out.y = fmuli(rel.x, fy->x) + fmuli(rel.y, fy->y) + fmuli(rel.z, fy->z);
    out.z = fmuli(rel.x, fz->x) + fmuli(rel.y, fz->y) + fmuli(rel.z, fz->z);
    return out;
}

HOT_INLINE ARM_CODE bool cam_to_screen(const v3* pc, int* sx, int* sy){
    if(pc->z <= g_near_z) return false;
    fix focal = scaled_focal();
    fix invz = fmuli(focal, fast_invz(pc->z));
    fix px = fmuli(pc->x, invz);
    fix py = fmuli(pc->y, invz);
    if(px < -I2F(SCREEN_W*2) || px > I2F(SCREEN_W*3) || py < -I2F(SCREEN_H*2) || py > I2F(SCREEN_H*3)) return false;
    *sx = (g_rw>>1) + F2I(px);
    *sy = (g_rh>>1) - F2I(py);
    return true;
}

HOT_INLINE ARM_CODE bool cam_to_screen_focal(const v3* pc, fix focal, int* sx, int* sy){
    if(pc->z <= g_near_z) return false;
    fix invz = fmuli(focal, fast_invz(pc->z));
    fix px = fmuli(pc->x, invz);
    fix py = fmuli(pc->y, invz);
    if(px < -I2F(SCREEN_W*2) || px > I2F(SCREEN_W*3) || py < -I2F(SCREEN_H*2) || py > I2F(SCREEN_H*3)) return false;
    *sx = (g_rw>>1) + F2I(px);
    *sy = (g_rh>>1) - F2I(py);
    return true;
}

#if PERF_PROFILE && PERF_HALT_AFTER_WORLDGEN
static ARM_CODE __attribute__((noinline)) void draw_sky_gradient(void) {
#else
static IWRAM_CODE ARM_CODE __attribute__((noinline)) void draw_sky_gradient(void) {
#endif
    int p_max = (LUT_N>>2) - 5;
    int horizon_y = (g_rh/2) - (pl.pitch * (g_rh/2)) / (p_max ? p_max : 1);
    if (horizon_y < 0) horizon_y = 0;
    if (horizon_y > g_rh) horizon_y = g_rh;
    u16* vram = (u16*)backbuffer; 
    
    if(g_sky_mode == 1) {
        int sky_idx = SKY_START_IDX + (SKY_COLORS - 1);
        int ground_idx = VOID_START_IDX + (SKY_COLORS - 1);
        if(sky_idx > 250) sky_idx = 250;
        if(ground_idx > 250) ground_idx = 250;
        u16 sky = (u16)(sky_idx | (sky_idx << 8));
        u32 sky32 = (u32)sky | ((u32)sky << 16);
        u16 ground = (u16)(ground_idx | (ground_idx << 8));
        u32 ground32 = (u32)ground | ((u32)ground << 16);


        for (int y=0; y<g_rh; y++) {
            u32* row = (u32*)(vram + (y * (SCREEN_W >> 1)));
            u32 color = (y < horizon_y) ? sky32 : ground32;
            int count = g_rw / 4; 
            while(count--) *row++ = color;
            if (g_rw & 2) *((u16*)row) = (u16)color;
        }
        return;
    }

    int sky_den = horizon_y ? horizon_y : 1;
    int ground_den = (g_rh - horizon_y) ? (g_rh - horizon_y) : 1;
    int sky_grad = 0, sky_acc = 0;
    int ground_grad = 0, ground_acc = 0;

    for (int y = 0; y < g_rh; y++) {
        u32* row = (u32*)(vram + (y * (SCREEN_W >> 1)));
        u16 val16;
        if (y < horizon_y) {
            int grad = sky_grad;
            if(grad >= SKY_COLORS) grad = SKY_COLORS - 1;
            int color_idx = SKY_START_IDX + (SKY_COLORS - 1 - grad);
            if (color_idx > 250) color_idx = 250;
            val16 = (u16)(color_idx | (color_idx << 8));
            sky_acc += SKY_COLORS;
            while(sky_acc >= sky_den){
                sky_acc -= sky_den;
                sky_grad++;
            }
        } else {
            int grad = ground_grad;
            if(grad >= SKY_COLORS) grad = SKY_COLORS - 1;
            int color_idx = VOID_START_IDX + grad;
            if (color_idx > 250) color_idx = 250;
            val16 = (u16)(color_idx | (color_idx << 8));
            ground_acc += SKY_COLORS;
            while(ground_acc >= ground_den){
                ground_acc -= ground_den;
                ground_grad++;
            }
        }
        u32 val32 = (u32)val16 | ((u32)val16 << 16);
        int count = g_rw >> 2;
        while(count--) *row++ = val32;
        if (g_rw & 2) *((u16*)row) = val16;
    }
}

#include "raster.c"

static void draw_line_clipped(int x0, int y0, int x1, int y1, u8 color) {
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = dx + dy, e2;
    while (1) {
        if(x0 >= 0 && x0 < g_rw && y0 >= 0 && y0 < g_rh) {
            u16* ptr = (u16*)backbuffer + (y0 * (SCREEN_W >> 1)) + (x0 >> 1);
            if (x0 & 1) *ptr = (*ptr & 0x00FF) | (color << 8);
            else        *ptr = (*ptr & 0xFF00) | color;
        }
        if (x0 == x1 && y0 == y1) break;
        e2 = 2 * err;
        if (e2 >= dy) { err += dy; x0 += sx; }
        if (e2 <= dx) { err += dx; y0 += sy; }
    }
}

static inline void mode4_plot_px(volatile u8* dst, int x, int y, u8 c);
static inline void fill_rect(volatile u8* dst,int x,int y,int w,int h,u8 c);
static void draw_text3x5(volatile u8* dst,int x,int y,const char* s,int scale,u8 col);
static void draw_block_texture_preview_to(volatile u8* dst, int x, int y, int size, u8 blk_id);
static void draw_item_preview_to(volatile u8* dst, int x, int y, int size, u8 item);

static const u8 heart_sprite[6] = {0x36, 0x7F, 0x7F, 0x3E, 0x1C, 0x08};
static const u8 bubble_sprite[5] = {0x04, 0x0E, 0x1F, 0x0E, 0x04};

static void draw_status_icon(int x, int y, const u8* rows, int w, int h, int scale, int state, u8 fill_col, u8 empty_col, int flash_mask){
    if(scale == 1){
        for(int ry=0; ry<h; ry++){
            for(int rx=0; rx<w; rx++){
                if(rows[ry] & (1 << (w - 1 - rx))) mode4_plot_px(backbuffer, x + 1 + rx, y + 1 + ry, 244);
            }
        }
        for(int ry=0; ry<h; ry++){
            for(int rx=0; rx<w; rx++){
                if(!(rows[ry] & (1 << (w - 1 - rx)))) continue;
                u8 col = empty_col;
                int half_mask = (rx < ((w + 1) >> 1)) ? 1 : 2;
                if(state >= 2) col = fill_col;
                else if(state == 1 && half_mask == 1) col = fill_col;
                if(flash_mask & half_mask) col = 251;
                mode4_plot_px(backbuffer, x + rx, y + ry, col);
            }
        }
        return;
    }
    for(int ry=0; ry<h; ry++){
        for(int rx=0; rx<w; rx++){
            if(!(rows[ry] & (1 << (w - 1 - rx)))) continue;
            fill_rect(backbuffer, x + scale + rx*scale, y + scale + ry*scale, scale, scale, 244);
        }
    }
    for(int ry=0; ry<h; ry++){
        for(int rx=0; rx<w; rx++){
            if(!(rows[ry] & (1 << (w - 1 - rx)))) continue;
            u8 col = empty_col;
            int half_mask = (rx < ((w + 1) >> 1)) ? 1 : 2;
            if(state >= 2) col = fill_col;
            else if(state == 1 && half_mask == 1) col = fill_col;
            if(flash_mask & half_mask) col = 251;
            fill_rect(backbuffer, x + rx*scale, y + ry*scale, scale, scale, col);
        }
    }
}

static int heart_flash_mask_for_slot(int slot){
    if(g_health_flash_timer <= 0 || g_health_flash_unit < 0) return 0;
    if((g_health_flash_timer & 2) == 0) return 0;
    if((g_health_flash_unit >> 1) != slot) return 0;
    return (g_health_flash_unit & 1) ? 2 : 1;
}

static void draw_survival_status_icons(void){
    int icon_scale = (g_render_scale > 1) ? 1 : 2;
    int heart_w = 7 * icon_scale;
    int heart_h = 6 * icon_scale;
    int bubble_w = 5 * icon_scale;
    int bubble_h = 5 * icon_scale;
    int gap = icon_scale;
    int heart_count = (g_player_max_health + 1) >> 1;
    int bubble_count = g_player_max_air;
    if(heart_count < 1) heart_count = 1;
    if(heart_count > 10) heart_count = 10;
    if(bubble_count < 1) bubble_count = 1;
    if(bubble_count > 10) bubble_count = 10;

    int heart_row_w = heart_count * heart_w + (heart_count - 1) * gap;
    int heart_x = (g_rw - heart_row_w) >> 1;
    int hearts_y = g_rh - (20 / g_render_scale) - heart_h - 3;
    if(g_head_in_water) hearts_y -= bubble_h + 2;
    if(hearts_y < 2) hearts_y = 2;

    for(int i=0; i<heart_count; i++){
        int units = (int)g_player_health - i * 2;
        int state = (units >= 2) ? 2 : ((units == 1) ? 1 : 0);
        draw_status_icon(heart_x + i * (heart_w + gap), hearts_y, heart_sprite, 7, 6, icon_scale, state, 249, 248, heart_flash_mask_for_slot(i));
    }

    if(g_head_in_water){
        int bubble_row_w = bubble_count * bubble_w + (bubble_count - 1) * gap;
        int bubble_x = (g_rw - bubble_row_w) >> 1;
        int bubbles_y = hearts_y - bubble_h - 2;
        if(bubbles_y < 2) bubbles_y = 2;
        for(int i=0; i<bubble_count; i++){
            int state = (i < g_player_air) ? 2 : 0;
            int flash_mask = 0;
            if(g_air_flash_timer > 0 && (g_air_flash_timer & 2) && g_air_flash_unit == i) flash_mask = 3;
            draw_status_icon(bubble_x + i * (bubble_w + gap), bubbles_y, bubble_sprite, 5, 5, icon_scale, state, 247, 246, flash_mask);
        }
    }
}

static bool face_uv_to_screen(int bx, int by, int bz, int face, int u8x, int v8y,
                              const v3* fx, const v3* fy, const v3* fz,
                              const v3* cam_pos, fix focal, int* sx, int* sy){
    const fix eps = F(0.01f);
    fix u = (fix)(u8x << PRECISION) / 8;
    fix v = (fix)(v8y << PRECISION) / 8;
    v3 wp;
    switch(face){
        case FACE_POSX: wp = (v3){ I2F(bx + 1) + eps, I2F(by) + FONE - v, I2F(bz) + u }; break;
        case FACE_NEGX: wp = (v3){ I2F(bx) - eps, I2F(by) + FONE - v, I2F(bz + 1) - u }; break;
        case FACE_POSY: wp = (v3){ I2F(bx) + u, I2F(by + 1) + eps, I2F(bz) + v }; break;
        case FACE_NEGY: wp = (v3){ I2F(bx) + u, I2F(by) - eps, I2F(bz + 1) - v }; break;
        case FACE_POSZ: wp = (v3){ I2F(bx) + u, I2F(by) + FONE - v, I2F(bz + 1) + eps }; break;
        case FACE_NEGZ: wp = (v3){ I2F(bx + 1) - u, I2F(by) + FONE - v, I2F(bz) - eps }; break;
        default: return false;
    }
    v3 pc = world_to_cam(wp.x, wp.y, wp.z, fx, fy, fz, cam_pos);
    return cam_to_screen_focal(&pc, focal, sx, sy);
}

typedef struct {
    u8 stage;
    u8 u0, v0, u1, v1;
} CrackSegment;

static const CrackSegment crack_patterns[4][20] = {
    {
        {0, 4, 4, 3, 2}, {0, 4, 4, 5, 5}, {0, 4, 4, 2, 4}, {0, 4, 4, 6, 3},
        {1, 3, 2, 2, 1}, {1, 5, 5, 7, 6}, {1, 2, 4, 1, 5}, {1, 6, 3, 7, 2},
        {2, 2, 1, 1, 0}, {2, 7, 6, 8, 7}, {2, 1, 5, 0, 6}, {2, 7, 2, 8, 1},
        {3, 3, 5, 2, 7}, {3, 5, 2, 6, 1}, {3, 2, 7, 1, 8}, {3, 6, 1, 7, 0},
        {4, 2, 3, 0, 2}, {4, 6, 4, 8, 5}, {4, 4, 7, 5, 8}, {4, 1, 6, 0, 7}
    },
    {
        {0, 4, 4, 5, 2}, {0, 4, 4, 3, 5}, {0, 4, 4, 6, 4}, {0, 4, 4, 2, 3},
        {1, 5, 2, 6, 1}, {1, 3, 5, 2, 6}, {1, 6, 4, 7, 5}, {1, 2, 3, 1, 2},
        {2, 6, 1, 8, 0}, {2, 2, 6, 1, 8}, {2, 7, 5, 8, 6}, {2, 1, 2, 0, 1},
        {3, 5, 5, 6, 7}, {3, 3, 3, 2, 1}, {3, 6, 2, 7, 1}, {3, 2, 6, 1, 7},
        {4, 6, 7, 7, 8}, {4, 2, 1, 0, 0}, {4, 4, 6, 5, 8}, {4, 7, 3, 8, 4}
    },
    {
        {0, 4, 4, 4, 2}, {0, 4, 4, 2, 5}, {0, 4, 4, 6, 5}, {0, 4, 4, 5, 3},
        {1, 4, 2, 3, 1}, {1, 2, 5, 1, 6}, {1, 6, 5, 7, 6}, {1, 5, 3, 6, 2},
        {2, 3, 1, 2, 0}, {2, 1, 6, 0, 7}, {2, 7, 6, 8, 7}, {2, 6, 2, 7, 1},
        {3, 2, 3, 1, 2}, {3, 5, 5, 6, 7}, {3, 3, 6, 2, 8}, {3, 7, 4, 8, 5},
        {4, 1, 2, 0, 1}, {4, 6, 7, 8, 8}, {4, 4, 6, 5, 8}, {4, 7, 1, 8, 0}
    },
    {
        {0, 4, 4, 3, 3}, {0, 4, 4, 5, 2}, {0, 4, 4, 6, 5}, {0, 4, 4, 2, 4},
        {1, 3, 3, 2, 2}, {1, 5, 2, 6, 1}, {1, 6, 5, 7, 6}, {1, 2, 4, 1, 5},
        {2, 2, 2, 0, 1}, {2, 6, 1, 8, 0}, {2, 7, 6, 8, 7}, {2, 1, 5, 0, 6},
        {3, 3, 5, 2, 7}, {3, 6, 2, 7, 1}, {3, 5, 6, 6, 8}, {3, 2, 3, 1, 2},
        {4, 2, 7, 1, 8}, {4, 7, 3, 8, 4}, {4, 4, 6, 3, 8}, {4, 1, 1, 0, 0}
    }
};

static bool block_face_is_exposed(int x, int y, int z, int face){
    int nx = x, ny = y, nz = z;
    switch(face){
        case FACE_POSX: nx++; break;
        case FACE_NEGX: nx--; break;
        case FACE_POSY: ny++; break;
        case FACE_NEGY: ny--; break;
        case FACE_POSZ: nz++; break;
        case FACE_NEGZ: nz--; break;
        default: return false;
    }
    if(nx < 0 || nx >= CW || ny < 0 || ny >= CH || nz < 0 || nz >= CD) return true;
    u8 neighbor = world[widx(nx, ny, nz)];
    return (neighbor == 0 || neighbor == (BLK_PORTAL + 1));
}

static void draw_block_break_overlay_face(int bx, int by, int bz, int face, int stage,
                                          const v3* fx, const v3* fy, const v3* fz,
                                          const v3* cam_pos, fix focal){
    int gx = global_x_from_local(bx);
    int gz = global_z_from_local(bz);
    u32 pattern_hash = hash_region(0xC2A54A71u, gx, by, gz);
    const CrackSegment* pattern = crack_patterns[pattern_hash & 3u];
    for(unsigned i=0; i<sizeof(crack_patterns[0])/sizeof(crack_patterns[0][0]); i++){
        const CrackSegment* seg = &pattern[i];
        if(seg->stage > stage) continue;
        int sx0, sy0, sx1, sy1;
        if(!face_uv_to_screen(bx, by, bz, face, seg->u0, seg->v0, fx, fy, fz, cam_pos, focal, &sx0, &sy0)) continue;
        if(!face_uv_to_screen(bx, by, bz, face, seg->u1, seg->v1, fx, fy, fz, cam_pos, focal, &sx1, &sy1)) continue;
        draw_line_clipped(sx0, sy0 + 1, sx1, sy1 + 1, 255);
        draw_line_clipped(sx0, sy0, sx1, sy1, 251);
    }
}

static void draw_block_break_overlay_all_faces(int bx, int by, int bz, int stage,
                                               const v3* fx, const v3* fy, const v3* fz,
                                               const v3* cam_pos, fix focal){
    for(int face=0; face<6; face++){
        if(!block_face_is_exposed(bx, by, bz, face)) continue;
        draw_block_break_overlay_face(bx, by, bz, face, stage, fx, fy, fz, cam_pos, focal);
    }
}

static bool item_has_line_of_sight(const v3* cam_pos, const v3* target){
    v3 delta = { target->x - cam_pos->x, target->y - cam_pos->y, target->z - cam_pos->z };
    fix max_axis = fixabs(delta.x);
    if(fixabs(delta.y) > max_axis) max_axis = fixabs(delta.y);
    if(fixabs(delta.z) > max_axis) max_axis = fixabs(delta.z);
    int steps = F2I(max_axis * 5);
    if(steps < 1) steps = 1;
    if(steps > 96) steps = 96;
    fix inv_steps = inv_int(steps);
    v3 step = {
        fmuli(delta.x, inv_steps),
        fmuli(delta.y, inv_steps),
        fmuli(delta.z, inv_steps)
    };
    v3 p = *cam_pos;
    int tx = F2I(target->x), ty = F2I(target->y), tz = F2I(target->z);
    for(int i=0; i<steps; i++){
        p.x += step.x;
        p.y += step.y;
        p.z += step.z;
        int cx = F2I(p.x), cy = F2I(p.y), cz = F2I(p.z);
        if(cx == tx && cy == ty && cz == tz) return true;
        if(solid_at(cx, cy, cz)) return false;
    }
    return true;
}

typedef struct {
    u8 item;
    int sx, sy, size;
    fix depth;
} VisibleDroppedItem;

static u8 break_particle_color_for_block(u8 blk, int variant){
    if(blk == BLK_ORE_IRON || blk == BLK_ORE_GOLD || blk == BLK_ORE_DIAMOND){
        return texture_shaded_color(blk, 3, (u8)(5 + (variant % 3)));
    }
    if(blk == BLK_GRASS) blk = BLK_GRASS_SIDE;
    if(blk == BLK_TRACK) blk = BLK_TRACK_SIDE;
    if(blk == BLK_TNT_LIT) blk = BLK_TNT;
    return shaded_texture_for(blk, 3)[(variant * 37 + 7) & (TEXTURE_PIXELS - 1)];
}

static void spawn_block_break_particles(int x, int y, int z, u8 blk){
    int gx = global_x_from_local(x);
    int gz = global_z_from_local(z);
    for(int i = 0; i < 6; i++){
        int slot = -1;
        int oldest = 0;
        u8 oldest_life = 0;
        for(int j = 0; j < MAX_BREAK_PARTICLES; j++){
            if(!g_break_particles[j].active){
                slot = j;
                break;
            }
            if(j == 0 || g_break_particles[j].life < oldest_life){
                oldest = j;
                oldest_life = g_break_particles[j].life;
            }
        }
        if(slot < 0) slot = oldest;

        u32 seed = hash_region(0x51A9E37Du + (u32)i, gx, y, gz);
        BreakParticle* p = &g_break_particles[slot];
        p->active = true;
        p->life = 30;
        p->color = break_particle_color_for_block(blk, (int)((seed >> 3) & 3u));
        p->pos.x = I2F(x) + F(0.3f) + (fix)((seed & 255u) << 8);
        p->pos.y = I2F(y) + F(0.3f) + (fix)(((seed >> 8) & 127u) << 8);
        p->pos.z = I2F(z) + F(0.3f) + (fix)(((seed >> 15) & 255u) << 8);
        p->vel.x = ((seed & 1u) ? F(0.035f) : -F(0.035f)) + (fix)(((int)((seed >> 1) & 15u) - 7) << 10);
        p->vel.z = ((seed & 2u) ? F(0.03f) : -F(0.03f)) + (fix)(((int)((seed >> 5) & 15u) - 7) << 10);
        p->vel.y = F(0.04f) + (fix)(((seed >> 9) & 31u) << 9);
    }
}

static inline bool block_is_tnt_type(u8 blk){
    return blk == BLK_TNT || blk == BLK_TNT_LIT;
}

static int find_tnt_entity_slot(int x, int y, int z){
    for(int i = 0; i < MAX_TNT_ENTITIES; i++){
        if(!g_tnt_entities[i].active) continue;
        if(g_tnt_entities[i].x == x && g_tnt_entities[i].y == y && g_tnt_entities[i].z == z) return i;
    }
    return -1;
}

static void spawn_explosion_particles(int x, int y, int z){
    for(int i = 0; i < 20; i++){
        int slot = -1;
        int oldest = 0;
        u8 oldest_life = 0;
        for(int j = 0; j < MAX_BREAK_PARTICLES; j++){
            if(!g_break_particles[j].active){
                slot = j;
                break;
            }
            if(j == 0 || g_break_particles[j].life < oldest_life){
                oldest = j;
                oldest_life = g_break_particles[j].life;
            }
        }
        if(slot < 0) slot = oldest;

        u32 seed = hash_region(0x7A4E5400u + (u32)i, global_x_from_local(x), y, global_z_from_local(z));
        BreakParticle* p = &g_break_particles[slot];
        p->active = true;
        p->life = 20 + (u8)(seed & 15u);
        p->color = (i % 3 == 0) ? 255 : ((i & 1) ? 254 : 249);
        p->pos.x = I2F(x) + F(0.2f) + (fix)((seed & 255u) << 7);
        p->pos.y = I2F(y) + F(0.2f) + (fix)(((seed >> 8) & 255u) << 7);
        p->pos.z = I2F(z) + F(0.2f) + (fix)(((seed >> 16) & 255u) << 7);
        p->vel.x = (fix)(((int)(seed & 63u) - 31) << 11);
        p->vel.y = F(0.05f) + (fix)((((seed >> 6) & 63u) + 8) << 10);
        p->vel.z = (fix)(((int)((seed >> 12) & 63u) - 31) << 11);
    }
}

static bool ignite_tnt_block_with_fuse(int x, int y, int z, int fuse){
    int slot;
    int world_id;
    int oldest = 0;
    u8 oldest_fuse = 255;
    if(x < 0 || x >= CW || y < 0 || y >= CH || z < 0 || z >= CD) return false;
    world_id = world[widx(x, y, z)];
    if(world_id != BLK_TNT + 1 && world_id != BLK_TNT_LIT + 1) return false;

    slot = find_tnt_entity_slot(x, y, z);
    if(slot >= 0){
        if(fuse < g_tnt_entities[slot].fuse) g_tnt_entities[slot].fuse = (u8)fuse;
        world[widx(x, y, z)] = BLK_TNT_LIT + 1;
        return true;
    }

    slot = -1;
    for(int i = 0; i < MAX_TNT_ENTITIES; i++){
        if(!g_tnt_entities[i].active){
            slot = i;
            break;
        }
        if(g_tnt_entities[i].fuse < oldest_fuse){
            oldest = i;
            oldest_fuse = g_tnt_entities[i].fuse;
        }
    }
    if(slot < 0) slot = oldest;
    else oldest = slot;

    if(g_tnt_entities[oldest].active){
        int ox = g_tnt_entities[oldest].x;
        int oy = g_tnt_entities[oldest].y;
        int oz = g_tnt_entities[oldest].z;
        if(ox >= 0 && ox < CW && oy >= 0 && oy < CH && oz >= 0 && oz < CD){
            int old_id = widx(ox, oy, oz);
            if(world[old_id] == BLK_TNT_LIT + 1) world[old_id] = BLK_TNT + 1;
        }
    }

    g_tnt_entities[slot].active = true;
    g_tnt_entities[slot].fuse = (u8)clamp_int(fuse, 10, 180);
    g_tnt_entities[slot].x = (s8)x;
    g_tnt_entities[slot].y = (s8)y;
    g_tnt_entities[slot].z = (s8)z;
    world[widx(x, y, z)] = BLK_TNT_LIT + 1;
    return true;
}

static bool tnt_explosion_breaks(u8 blk){
    return blk != BLK_BEDROCK && blk != BLK_OBSIDIAN && blk != BLK_PORTAL &&
           blk != BLK_WATER && blk != BLK_LAVA;
}

static void explode_tnt_block(int x, int y, int z){
    const int radius = 3;
    world_set_and_log(x, y, z, 0);
    spawn_explosion_particles(x, y, z);

    if(g_game_mode == MODE_SURVIVAL){
        fix ex = I2F(x) + F(0.5f);
        fix ey = I2F(y) + F(0.5f);
        fix ez = I2F(z) + F(0.5f);
        int dx = F2I(fixabs(pl.pos.x - ex) + F(0.25f));
        int dy = F2I(fixabs((pl.pos.y + F(0.9f)) - ey) + F(0.25f));
        int dz = F2I(fixabs(pl.pos.z - ez) + F(0.25f));
        int max_axis = dx;
        if(dy > max_axis) max_axis = dy;
        if(dz > max_axis) max_axis = dz;
        if(max_axis <= 1) survival_damage(20);
        else if(max_axis == 2) survival_damage(12);
        else if(max_axis == 3) survival_damage(6);
    }

    for(int dz = -radius; dz <= radius; dz++){
        for(int dy = -radius; dy <= radius; dy++){
            for(int dx = -radius; dx <= radius; dx++){
                int tx = x + dx;
                int ty = y + dy;
                int tz = z + dz;
                int dist2 = dx * dx + dy * dy + dz * dz;
                if(dist2 > (radius * radius + 1)) continue;
                if(tx < 0 || tx >= CW || ty < 0 || ty >= CH || tz < 0 || tz >= CD) continue;
                if(tx == x && ty == y && tz == z) continue;
                if(world[widx(tx, ty, tz)] == 0) continue;

                {
                    u8 blk = (u8)(world[widx(tx, ty, tz)] - 1);
                    if(block_is_tnt_type(blk)){
                        ignite_tnt_block_with_fuse(tx, ty, tz, 18 + dist2 * 4);
                        continue;
                    }
                    if(!tnt_explosion_breaks(blk)) continue;
                    spawn_block_break_particles(tx, ty, tz, blk);
                    world_set_and_log(tx, ty, tz, 0);
                }
            }
        }
    }
}

static void update_tnt_entities(void){
    for(int i = 0; i < MAX_TNT_ENTITIES; i++){
        PrimedTnt* t = &g_tnt_entities[i];
        if(!t->active) continue;
        if(t->x < 0 || t->x >= CW || t->y < 0 || t->y >= CH || t->z < 0 || t->z >= CD){
            t->active = false;
            continue;
        }
        if(t->fuse > 0) t->fuse--;
        if(t->fuse == 0){
            t->active = false;
            explode_tnt_block(t->x, t->y, t->z);
            continue;
        }
        {
            bool flash = (t->fuse < 24) ? (((t->fuse >> 1) & 1u) == 0u) : (((t->fuse / 6) & 1u) == 0u);
            u8 desired = flash ? (BLK_TNT_LIT + 1) : (BLK_TNT + 1);
            int id = widx(t->x, t->y, t->z);
            if(world[id] == 0 || block_is_tnt_type((u8)(world[id] - 1))) world[id] = desired;
        }
    }
}

static void update_break_particles(void){
    const fix gravity = F(0.01f);
    for(int i = 0; i < MAX_BREAK_PARTICLES; i++){
        BreakParticle* p = &g_break_particles[i];
        if(!p->active) continue;
        if(p->life == 0){
            p->active = false;
            continue;
        }
        p->life--;
        p->pos.x += p->vel.x;
        p->pos.y += p->vel.y;
        p->pos.z += p->vel.z;
        p->vel.x = fmuli(p->vel.x, F(0.92f));
        p->vel.z = fmuli(p->vel.z, F(0.92f));
        p->vel.y -= gravity;
        if(p->pos.x < 0 || p->pos.x >= I2F(CW) || p->pos.z < 0 || p->pos.z >= I2F(CD) || p->pos.y < 0 || p->pos.y >= I2F(CH)){
            p->active = false;
        }
    }
}

static void render_break_particles(const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos, fix focal){
    for(int i = 0; i < MAX_BREAK_PARTICLES; i++){
        const BreakParticle* p = &g_break_particles[i];
        if(!p->active) continue;
        v3 pc = world_to_cam(p->pos.x, p->pos.y, p->pos.z, fx, fy, fz, cam_pos);
        if(pc.z <= g_near_z || pc.z > FAR_Z) continue;
        if(fixabs(pc.x) > (pc.z << 1) || fixabs(pc.y) > (pc.z << 1)) continue;
        if(!item_has_line_of_sight(cam_pos, &p->pos)) continue;
        fix invz = fmuli(focal, fast_invz(pc.z));
        fix px = fmuli(pc.x, invz);
        fix py = fmuli(pc.y, invz);
        if(px < -I2F(SCREEN_W*2) || px > I2F(SCREEN_W*3) || py < -I2F(SCREEN_H*2) || py > I2F(SCREEN_H*3)) continue;
        int sx = (g_rw >> 1) + F2I(px);
        int sy = (g_rh >> 1) - F2I(py);
        int size = F2I(invz) >> 2;
        if(size < 1) size = 1;
        if(size > 4) size = 4;
        fill_rect(backbuffer, sx - (size >> 1), sy - (size >> 1), size, size, p->color);
    }
}

static void render_dropped_items(const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos, fix focal){
    VisibleDroppedItem visible[MAX_DROPPED_ITEMS];
    int count = 0;
    bool limit_far = stream_load_shed_active();

    for(int i=0; i<MAX_DROPPED_ITEMS; i++){
        const DroppedItem* d = &g_dropped_items[i];
        if(!d->active) continue;
        if(limit_far){
            int dx = abs(F2I(d->pos.x - pl.pos.x));
            int dy = abs(F2I(d->pos.y - pl.pos.y));
            int dz = abs(F2I(d->pos.z - pl.pos.z));
            if(dx > 12 || dy > 12 || dz > 12) continue;
        }

        v3 wp = d->pos;
        wp.y += F(0.08f) + fmuli(fsin((d->age * 8) & (LUT_N - 1)), F(0.05f));
        v3 pc = world_to_cam(wp.x, wp.y, wp.z, fx, fy, fz, cam_pos);
        if(pc.z <= g_near_z || pc.z > FAR_Z) continue;
        if(fixabs(pc.x) > (pc.z << 1) || fixabs(pc.y) > (pc.z << 1)) continue;
        if(!item_has_line_of_sight(cam_pos, &wp)) continue;

        fix invz = fmuli(focal, fast_invz(pc.z));
        fix px = fmuli(pc.x, invz);
        fix py = fmuli(pc.y, invz);
        if(px < -I2F(SCREEN_W*2) || px > I2F(SCREEN_W*3) || py < -I2F(SCREEN_H*2) || py > I2F(SCREEN_H*3)) continue;

        int sx = (g_rw >> 1) + F2I(px);
        int sy = (g_rh >> 1) - F2I(py);
        int size = F2I(invz);
        size = (size * 2) / 3;
        if(size < 4) size = 4;
        if(size > 16) size = 16;

        visible[count].item = d->item;
        visible[count].sx = sx;
        visible[count].sy = sy;
        visible[count].size = size;
        visible[count].depth = pc.z;
        count++;
    }

    for(int i=1; i<count; i++){
        VisibleDroppedItem cur = visible[i];
        int j = i - 1;
        while(j >= 0 && visible[j].depth < cur.depth){
            visible[j + 1] = visible[j];
            j--;
        }
        visible[j + 1] = cur;
    }

    for(int i=0; i<count; i++){
        const VisibleDroppedItem* v = &visible[i];
        int shadow_w = v->size - (v->size / 3);
        if(shadow_w < 2) shadow_w = 2;
        fill_rect(backbuffer, v->sx - (shadow_w >> 1), v->sy + (v->size >> 2), shadow_w, (v->size > 7) ? 2 : 1, 244);
        draw_item_preview_to(backbuffer, v->sx - (v->size >> 1), v->sy - (v->size >> 1), v->size, v->item);
    }
}

static void draw_hand_block(u8 blk_type, int anim_timer) {

    int size = 50 / g_render_scale; 
    int base_x = g_rw - (20 / g_render_scale);
    int base_y = g_rh + (35 / g_render_scale);
    int anim_offset_y = 0;
    if(anim_timer > 0) {
        fix s = fsin((15-anim_timer) * (LUT_N/30));
        anim_offset_y = F2I(fmuli(s, I2F(15 / g_render_scale)));
    }
    base_y += anim_offset_y;

    v2uv p[7];
    p[0].x = base_x; p[0].y = base_y - size; 
    p[1].x = base_x; p[1].y = base_y - size - (size/2) - (size/4); 
    p[2].x = base_x + size; p[2].y = base_y - size - (size/4);
    p[3].x = base_x - size; p[3].y = base_y - size - (size/4);
    p[4].x = base_x; p[4].y = base_y;
    p[5].x = base_x + size; p[5].y = base_y - (size/4);
    p[6].x = base_x - size; p[6].y = base_y - (size/4);

    fix U0=0, U1=I2F(15), V0=0, V1=I2F(15);
    u8 render_top = block_render_top_type(blk_type);
    u8 render_side = block_render_side_type(blk_type);

    const u8* tex_top = shaded_texture_for(render_top, 3);
    const u8* tex_side_1 = shaded_texture_for(render_side, 1);
    const u8* tex_side_0 = shaded_texture_for(render_side, 0);
    int base_idx_top = block_flat_palette_type(render_top) << 3;
    int base_idx_side = block_flat_palette_type(render_side) << 3;

    v2uv q[4];
    q[0]=p[3]; q[0].u=U0; q[0].v=V1; q[1]=p[1]; q[1].u=U0; q[1].v=V0;
    q[2]=p[2]; q[2].u=U1; q[2].v=V0; q[3]=p[0]; q[3].u=U1; q[3].v=V1;
    draw_quad_affine(q[0], q[1], q[2], q[3], tex_top, base_idx_top, 3); 

    q[0]=p[6]; q[0].u=U0; q[0].v=V1; q[1]=p[3]; q[1].u=U0; q[1].v=V0;
    q[2]=p[0]; q[2].u=U1; q[2].v=V0; q[3]=p[4]; q[3].u=U1; q[3].v=V1;
    draw_quad_affine(q[0], q[1], q[2], q[3], tex_side_1, base_idx_side, 1); 

    q[0]=p[4]; q[0].u=U0; q[0].v=V1; q[1]=p[0]; q[1].u=U0; q[1].v=V0;
    q[2]=p[2]; q[2].u=U1; q[2].v=V0; q[3]=p[5]; q[3].u=U1; q[3].v=V1;
    draw_quad_affine(q[0], q[1], q[2], q[3], tex_side_0, base_idx_side, 0); 
}

