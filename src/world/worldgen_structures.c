#define MAX_COLUMN_RAVINES 12

typedef struct {
    bool active;
    bool guaranteed;
    int center_x;
    int center_z;
    int length;
    int yaw;
    int steps;
    int bottom_y;
    int width_min;
    int width_max;
    int bbox_x0;
    int bbox_x1;
    int bbox_z0;
    int bbox_z1;
    u32 seed;
} RavineDesc;

static EWRAM_DATA RavineDesc g_ravine_work[MAX_COLUMN_RAVINES];
#define RAVINE_REGION_SIZE 96
#define RAVINE_GRID_W ((VW + RAVINE_REGION_SIZE - 1) / RAVINE_REGION_SIZE)
#define RAVINE_GRID_D ((VD + RAVINE_REGION_SIZE - 1) / RAVINE_REGION_SIZE)
static EWRAM_DATA RavineDesc g_ravine_guaranteed_cache[3];
static EWRAM_DATA int g_ravine_guaranteed_count = 0;
static EWRAM_DATA RavineDesc g_ravine_region_cache[RAVINE_GRID_W * RAVINE_GRID_D];
static EWRAM_DATA u8 g_ravine_region_cache_valid[RAVINE_GRID_W * RAVINE_GRID_D];
static EWRAM_DATA bool g_ravine_cache_ready;
static EWRAM_DATA u32 g_ravine_cache_seed;
static EWRAM_DATA WorldType g_ravine_cache_world_type;
static EWRAM_DATA int g_ravine_cache_dimension;
static const u16 inv_small_q12_lut[29] = {
    0, 4096, 2048, 1365, 1024, 819, 682, 585, 512, 455,
    409, 372, 341, 315, 292, 273, 256, 240, 227, 215,
    204, 195, 186, 178, 170, 163, 157, 151, 146
};

typedef struct {
    bool along_x;
    int line;
    int start;
    int end;
    int start_y;
    int end_y;
    u8 support_phase;
    bool tracks;
    bool web_room;
} MineshaftSegment;

#define MAX_MINESHAFT_SEGMENTS 6
typedef struct {
    bool active;
    int hub_x;
    int hub_z;
    int hub_y;
    int segment_count;
    MineshaftSegment segments[MAX_MINESHAFT_SEGMENTS];
} MineshaftDesc;

static inline void carve_column_range(u8* col, int plane_stride, int y0, int y1){
    y0 = clamp_int(y0, 1, CH - 2);
    y1 = clamp_int(y1, 1, CH - 2);
    for(int y = y0; y <= y1; y++){
        int idx = y * plane_stride;
        if(col[idx] == BLK_BEDROCK + 1) continue;
        col[idx] = 0;
    }
}

static inline void set_column_block(u8* col, int plane_stride, int y, u8 block_id){
    if(y < 1 || y >= CH - 1) return;
    col[y * plane_stride] = block_id;
}

static inline int lerp_int_round(int a, int b, int num, int den){
    if(den <= 0) return a;
    if(a == b) return a;
    if(b > a) return a + (((b - a) * num) + (den >> 1)) / den;
    return a - (((a - b) * num) + (den >> 1)) / den;
}

static void finalize_ravine_desc(RavineDesc* rav){
    int steps = rav->steps;
    rav->steps = steps;
    rav->bbox_x0 = VW;
    rav->bbox_x1 = -1;
    rav->bbox_z0 = VD;
    rav->bbox_z1 = -1;

    u32 state = rav->seed;
    int yaw = rav->yaw;
    int half_len = rav->length >> 1;
    int width_span = rav->width_max - rav->width_min;
    int taper_inv_q12 = (steps > 1 && steps <= 29) ? inv_small_q12_lut[steps - 1] : 0;
    fix step_len = (steps > 1) ? fdivi(I2F(rav->length), I2F(steps - 1)) : I2F(rav->length);
    fix posx = I2F(rav->center_x) + F(0.5f) - fmuli(fcos(yaw), I2F(half_len));
    fix posz = I2F(rav->center_z) + F(0.5f) - fmuli(fsin(yaw), I2F(half_len));

    for(int step = 0; step < steps; step++){
        int dist_to_end = step;
        int tail = (steps - 1) - step;
        int taper = (dist_to_end < tail) ? dist_to_end : tail;
        int hr = rav->width_min;
        int bottom_y = rav->bottom_y + rng_next_range(&state, -1, 1);
        if(steps > 1) hr += (width_span * taper * 2 * taper_inv_q12) >> 12;
        (void)bottom_y;

        int cx8 = (int)(posx >> 13);
        int cz8 = (int)(posz >> 13);
        int cx = cx8 >> 3;
        int cz = cz8 >> 3;
        int r = hr + 1;
        if(cx - r < rav->bbox_x0) rav->bbox_x0 = cx - r;
        if(cx + r > rav->bbox_x1) rav->bbox_x1 = cx + r;
        if(cz - r < rav->bbox_z0) rav->bbox_z0 = cz - r;
        if(cz + r > rav->bbox_z1) rav->bbox_z1 = cz + r;

        posx += fmuli(fcos(yaw), step_len);
        posz += fmuli(fsin(yaw), step_len);
        yaw = (yaw + rng_next_range(&state, -10, 10)) & (LUT_N - 1);
    }
}

static void validate_ravine_cache(void){
    if(g_ravine_cache_ready &&
       g_ravine_cache_seed == g_world_seed &&
       g_ravine_cache_world_type == g_world_type &&
       g_ravine_cache_dimension == g_dimension) return;

    g_ravine_cache_ready = true;
    g_ravine_cache_seed = g_world_seed;
    g_ravine_cache_world_type = g_world_type;
    g_ravine_cache_dimension = g_dimension;
    g_ravine_guaranteed_count = 0;
    memset(g_ravine_region_cache_valid, 0, sizeof(g_ravine_region_cache_valid));
}

static bool build_guaranteed_ravine_desc(RavineDesc* out, int variant){
    if(g_dimension != 0 || g_world_type == WORLD_SUPERFLAT) return false;
    u32 state = hash_region(0x52415631u, variant * 97, variant * 29, 0);
    memset(out, 0, sizeof(*out));
    out->active = true;
    out->guaranteed = true;
    out->center_x = rng_next_range(&state, 96, VW - 97);
    out->center_z = rng_next_range(&state, 96, VD - 97);
    out->length = rng_next_range(&state, 112, 164);
    out->yaw = rng_next_range(&state, 0, LUT_N - 1);
    out->steps = rng_next_range(&state, 20, 28);
    out->bottom_y = rng_next_range(&state, 2, 5);
    out->width_min = rng_next_range(&state, 4, 6);
    out->width_max = rng_next_range(&state, 9, (g_world_type == WORLD_HIGHLANDS) ? 13 : 11);
    out->seed = mix32(state ^ 0x6C617661u);
    finalize_ravine_desc(out);
    return true;
}

static bool build_regional_ravine_desc(int region_x, int region_z, RavineDesc* out){
    if(g_dimension != 0 || g_world_type == WORLD_SUPERFLAT) return false;
    u32 state = hash_region(0x52415632u, region_x, region_z, 0);
    int chance = (g_world_type == WORLD_HIGHLANDS) ? 18 : 12;
    if(rng_next_range(&state, 0, 99) >= chance) return false;
    memset(out, 0, sizeof(*out));
    out->active = true;
    out->center_x = clamp_int(region_x * 96 + rng_next_range(&state, 18, 78), 62, VW - 63);
    out->center_z = clamp_int(region_z * 96 + rng_next_range(&state, 18, 78), 62, VD - 63);
    out->length = rng_next_range(&state, 96, (g_world_type == WORLD_HIGHLANDS) ? 156 : 136);
    out->yaw = rng_next_range(&state, 0, LUT_N - 1);
    out->steps = rng_next_range(&state, 18, 26);
    out->bottom_y = rng_next_range(&state, 2, 7);
    out->width_min = rng_next_range(&state, 4, 6);
    out->width_max = rng_next_range(&state, 8, (g_world_type == WORLD_HIGHLANDS) ? 12 : 10);
    out->seed = mix32(state ^ 0x63686173u);
    finalize_ravine_desc(out);
    return true;
}

static void apply_ravine_desc_to_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride, const RavineDesc* rav, bool lava_only){
    if(!rav->active) return;
    if(gx < rav->bbox_x0 || gx > rav->bbox_x1 || gz < rav->bbox_z0 || gz > rav->bbox_z1) return;
    u32 state = rav->seed;
    int yaw = rav->yaw;
    int steps = rav->steps;
    int half_len = rav->length >> 1;
    int width_span = rav->width_max - rav->width_min;
    int taper_inv_q12 = (steps > 1 && steps <= 29) ? inv_small_q12_lut[steps - 1] : 0;
    fix step_len = (steps > 1) ? fdivi(I2F(rav->length), I2F(steps - 1)) : I2F(rav->length);
    fix posx = I2F(rav->center_x) + F(0.5f) - fmuli(fcos(yaw), I2F(half_len));
    fix posz = I2F(rav->center_z) + F(0.5f) - fmuli(fsin(yaw), I2F(half_len));

    for(int step = 0; step < steps; step++){
        int dist_to_end = step;
        int tail = (steps - 1) - step;
        int taper = (dist_to_end < tail) ? dist_to_end : tail;
        int hr = rav->width_min;
        int bottom_y = rav->bottom_y + rng_next_range(&state, -1, 1);
        int top_y = (desc->flags & COL_FLAG_OCEAN) ? 12 : clamp_int((int)desc->surface_y + (rav->guaranteed ? 3 : 1), 10, CH - 2);
        bool allow_surface = (desc->flags & COL_FLAG_OCEAN) == 0 && step > 1 && step + 2 < steps;

        if(steps > 1) hr += (width_span * taper * 2 * taper_inv_q12) >> 12;
        bottom_y = clamp_int(bottom_y, 2, 8);
        if(top_y < bottom_y + 8) top_y = bottom_y + 8;
        if(top_y > CH - 2) top_y = CH - 2;

        if(!lava_only){
            int vr = (top_y - bottom_y) >> 1;
            int cy = bottom_y + vr;
            if(vr < 6) vr = 6;
            carve_ellipsoid_in_column(
                gx, gz, desc, col, plane_stride,
                (int)(posx >> 13), (cy << 3) + 4, (int)(posz >> 13),
                hr, vr, allow_surface
            );
        } else if(step > 1 && step + 2 < steps &&
                  (rav->guaranteed || step == (steps >> 1) || step == ((steps >> 1) + 2))){
            int lava_radius = hr - 1;
            if(lava_radius < 2) lava_radius = 2;
            place_cave_lava_pool_in_column(gx, gz, col, plane_stride, (int)(posx >> 13), ((bottom_y + 1) << 3) + 4, (int)(posz >> 13), lava_radius);
        }

        posx += fmuli(fcos(yaw), step_len);
        posz += fmuli(fsin(yaw), step_len);
        yaw = (yaw + rng_next_range(&state, -10, 10)) & (LUT_N - 1);
    }
}

static int collect_ravines_for_column(int gx, int gz, RavineDesc* out, int max_count){
    validate_ravine_cache();
    int count = 0;
    int guaranteed_count = (g_world_type == WORLD_HIGHLANDS) ? 3 : 2;
    while(g_ravine_guaranteed_count < guaranteed_count){
        if(build_guaranteed_ravine_desc(&g_ravine_guaranteed_cache[g_ravine_guaranteed_count], g_ravine_guaranteed_count)){
            g_ravine_guaranteed_count++;
        } else {
            break;
        }
    }
    for(int i = 0; i < g_ravine_guaranteed_count; i++){
        if(count >= max_count) return count;
        out[count++] = g_ravine_guaranteed_cache[i];
    }

    {
        int region_x = gx / 96;
        int region_z = gz / 96;
        for(int rz = region_z - 1; rz <= region_z + 1; rz++){
            if(rz < 0 || rz > ((VD - 1) / 96)) continue;
            for(int rx = region_x - 1; rx <= region_x + 1; rx++){
                if(rx < 0 || rx > ((VW - 1) / 96)) continue;
                if(count >= max_count) return count;
                int idx = rz * RAVINE_GRID_W + rx;
                if(!g_ravine_region_cache_valid[idx]){
                    memset(&g_ravine_region_cache[idx], 0, sizeof(g_ravine_region_cache[idx]));
                    (void)build_regional_ravine_desc(rx, rz, &g_ravine_region_cache[idx]);
                    g_ravine_region_cache_valid[idx] = 1;
                }
                if(g_ravine_region_cache[idx].active) out[count++] = g_ravine_region_cache[idx];
            }
        }
    }
    return count;
}

static void apply_ravine_list_to_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride,
                                        const RavineDesc* ravines, int ravine_count, bool lava_only){
    for(int i = 0; i < ravine_count; i++){
        apply_ravine_desc_to_column(gx, gz, desc, col, plane_stride, &ravines[i], lava_only);
    }
}

static bool append_mineshaft_segment(MineshaftDesc* shaft, bool along_x, int line, int start, int end, int start_y, int end_y, u32* state, bool web_room){
    if(shaft->segment_count >= MAX_MINESHAFT_SEGMENTS) return false;
    shaft->segments[shaft->segment_count].along_x = along_x;
    shaft->segments[shaft->segment_count].line = line;
    shaft->segments[shaft->segment_count].start = start;
    shaft->segments[shaft->segment_count].end = end;
    shaft->segments[shaft->segment_count].start_y = clamp_int(start_y, 6, 18);
    shaft->segments[shaft->segment_count].end_y = clamp_int(end_y, 6, 18);
    shaft->segments[shaft->segment_count].support_phase = (u8)rng_next_range(state, 0, 4);
    shaft->segments[shaft->segment_count].tracks = rng_next_range(state, 0, 99) < 50;
    shaft->segments[shaft->segment_count].web_room = web_room;
    shaft->segment_count++;
    return true;
}

static bool build_guaranteed_mineshaft_desc(MineshaftDesc* out, int variant){
    if(g_dimension != 0 || g_world_type == WORLD_SUPERFLAT) return false;

    u32 state = hash_region(0x4D534831u, variant * 41, variant * 113, 0);
    int branch_target;
    int dir_used[4] = {0, 0, 0, 0};
    memset(out, 0, sizeof(*out));

    out->active = true;
    out->hub_x = rng_next_range(&state, 72, VW - 73);
    out->hub_z = rng_next_range(&state, 72, VD - 73);
    out->hub_y = rng_next_range(&state, 7, 16);
    branch_target = rng_next_range(&state, 3, 5);

    for(int i = 0; i < branch_target && out->segment_count < 4; i++){
        int dir = rng_next_range(&state, 0, 3);
        int tries = 0;
        while(dir_used[dir] && tries < 8){
            dir = (dir + 1) & 3;
            tries++;
        }
        if(dir_used[dir]) break;
        dir_used[dir] = 1;

        {
            int length = rng_next_range(&state, 18, 34);
            int end_y = clamp_int(out->hub_y + rng_next_range(&state, -2, 2), 6, 18);
            bool web_room = rng_next_range(&state, 0, 99) < 35;
            switch(dir){
                case 0: append_mineshaft_segment(out, true, out->hub_z, out->hub_x + 2, clamp_int(out->hub_x + length, 3, VW - 4), out->hub_y, end_y, &state, web_room); break;
                case 1: append_mineshaft_segment(out, true, out->hub_z, out->hub_x - 2, clamp_int(out->hub_x - length, 3, VW - 4), out->hub_y, end_y, &state, web_room); break;
                case 2: append_mineshaft_segment(out, false, out->hub_x, out->hub_z + 2, clamp_int(out->hub_z + length, 3, VD - 4), out->hub_y, end_y, &state, web_room); break;
                default: append_mineshaft_segment(out, false, out->hub_x, out->hub_z - 2, clamp_int(out->hub_z - length, 3, VD - 4), out->hub_y, end_y, &state, web_room); break;
            }
        }
    }

    while(out->segment_count < branch_target && out->segment_count < MAX_MINESHAFT_SEGMENTS){
        int parent = rng_next_range(&state, 0, out->segment_count - 1);
        MineshaftSegment* p = &out->segments[parent];
        int span = abs(p->end - p->start);
        int mid = (p->start + p->end) >> 1;
        int start_y = lerp_int_round(p->start_y, p->end_y, span >> 1, span ? span : 1);
        int end_y = clamp_int(start_y + rng_next_range(&state, -1, 2), 6, 18);
        int length = rng_next_range(&state, 12, 22);
        bool web_room = rng_next_range(&state, 0, 99) < 45;

        if(p->along_x){
            if(rng_next_range(&state, 0, 1) == 0)
                append_mineshaft_segment(out, false, mid, p->line + 1, clamp_int(p->line + length, 3, VD - 4), start_y, end_y, &state, web_room);
            else
                append_mineshaft_segment(out, false, mid, p->line - 1, clamp_int(p->line - length, 3, VD - 4), start_y, end_y, &state, web_room);
        } else {
            if(rng_next_range(&state, 0, 1) == 0)
                append_mineshaft_segment(out, true, mid, p->line + 1, clamp_int(p->line + length, 3, VW - 4), start_y, end_y, &state, web_room);
            else
                append_mineshaft_segment(out, true, mid, p->line - 1, clamp_int(p->line - length, 3, VW - 4), start_y, end_y, &state, web_room);
        }
    }

    return true;
}

static bool build_mineshaft_desc(int region_x, int region_z, MineshaftDesc* out){
    if(g_dimension != 0 || g_world_type == WORLD_SUPERFLAT) return false;

    u32 state = hash_region(0x4D534846u, region_x, region_z, 0);
    int chance = (g_world_type == WORLD_HIGHLANDS) ? 14 : 9;
    int branch_target;
    int dir_used[4] = {0, 0, 0, 0};
    memset(out, 0, sizeof(*out));
    if(rng_next_range(&state, 0, 99) >= chance) return false;

    out->active = true;
    out->hub_x = clamp_int(region_x * 80 + rng_next_range(&state, 16, 64), 12, VW - 13);
    out->hub_z = clamp_int(region_z * 80 + rng_next_range(&state, 16, 64), 12, VD - 13);
    out->hub_y = rng_next_range(&state, 7, 16);
    branch_target = rng_next_range(&state, 3, 6);

    for(int i = 0; i < branch_target && out->segment_count < 4; i++){
        int dir = rng_next_range(&state, 0, 3);
        int tries = 0;
        while(dir_used[dir] && tries < 8){
            dir = (dir + 1) & 3;
            tries++;
        }
        if(dir_used[dir]) break;
        dir_used[dir] = 1;

        {
            int length = rng_next_range(&state, 18, 34);
            int end_y = clamp_int(out->hub_y + rng_next_range(&state, -2, 2), 6, 18);
            bool web_room = rng_next_range(&state, 0, 99) < 35;
            switch(dir){
                case 0: append_mineshaft_segment(out, true, out->hub_z, out->hub_x + 2, clamp_int(out->hub_x + length, 3, VW - 4), out->hub_y, end_y, &state, web_room); break;
                case 1: append_mineshaft_segment(out, true, out->hub_z, out->hub_x - 2, clamp_int(out->hub_x - length, 3, VW - 4), out->hub_y, end_y, &state, web_room); break;
                case 2: append_mineshaft_segment(out, false, out->hub_x, out->hub_z + 2, clamp_int(out->hub_z + length, 3, VD - 4), out->hub_y, end_y, &state, web_room); break;
                default: append_mineshaft_segment(out, false, out->hub_x, out->hub_z - 2, clamp_int(out->hub_z - length, 3, VD - 4), out->hub_y, end_y, &state, web_room); break;
            }
        }
    }

    while(out->segment_count < branch_target && out->segment_count < MAX_MINESHAFT_SEGMENTS){
        int parent = rng_next_range(&state, 0, out->segment_count - 1);
        MineshaftSegment* p = &out->segments[parent];
        int span = abs(p->end - p->start);
        int mid = (p->start + p->end) >> 1;
        int start_y = lerp_int_round(p->start_y, p->end_y, span >> 1, span ? span : 1);
        int end_y = clamp_int(start_y + rng_next_range(&state, -1, 2), 6, 18);
        int length = rng_next_range(&state, 12, 22);
        bool web_room = rng_next_range(&state, 0, 99) < 45;

        if(p->along_x){
            if(rng_next_range(&state, 0, 1) == 0)
                append_mineshaft_segment(out, false, mid, p->line + 1, clamp_int(p->line + length, 3, VD - 4), start_y, end_y, &state, web_room);
            else
                append_mineshaft_segment(out, false, mid, p->line - 1, clamp_int(p->line - length, 3, VD - 4), start_y, end_y, &state, web_room);
        } else {
            if(rng_next_range(&state, 0, 1) == 0)
                append_mineshaft_segment(out, true, mid, p->line + 1, clamp_int(p->line + length, 3, VW - 4), start_y, end_y, &state, web_room);
            else
                append_mineshaft_segment(out, true, mid, p->line - 1, clamp_int(p->line - length, 3, VW - 4), start_y, end_y, &state, web_room);
        }
    }

    return true;
}

static void apply_mineshaft_room_to_column(int gx, int gz, u8* col, int plane_stride, int room_x, int room_z, int floor_y, bool webs, u32 seed){
    int dx = gx - room_x;
    int dz = gz - room_z;
    if(dx < -2 || dx > 2 || dz < -2 || dz > 2) return;

    carve_column_range(col, plane_stride, floor_y + 1, floor_y + 3);
    set_column_block(col, plane_stride, floor_y, BLK_PLANK + 1);
    if(abs(dx) == 2 && abs(dz) == 2){
        set_column_block(col, plane_stride, floor_y + 1, BLK_WOOD + 1);
        set_column_block(col, plane_stride, floor_y + 2, BLK_WOOD + 1);
        set_column_block(col, plane_stride, floor_y + 3, BLK_WOOD + 1);
    }
    if(webs){
        for(int y = floor_y + 1; y <= floor_y + 2; y++){
            u32 cell = hash_region(seed, room_x + dx + 3, y, room_z + dz + 3);
            if(abs(dx) >= 1 || abs(dz) >= 1){
                if((cell & 7u) == 0u) set_column_block(col, plane_stride, y, BLK_GRAVEL + 1);
                else if((cell & 7u) == 1u) set_column_block(col, plane_stride, y, BLK_MOSSY_COBBLE + 1);
            }
        }
    }
}

static void apply_mineshaft_segment_to_column(int gx, int gz, u8* col, int plane_stride, const MineshaftSegment* seg){
    int coord = seg->along_x ? gx : gz;
    int cross = seg->along_x ? gz : gx;
    int cross_delta = cross - seg->line;
    int lo = (seg->start < seg->end) ? seg->start : seg->end;
    int hi = (seg->start > seg->end) ? seg->start : seg->end;
    int span = hi - lo;
    int along;
    int floor_y;
    bool support;

    if(coord < lo || coord > hi || cross_delta < -2 || cross_delta > 2) return;

    along = coord - lo;
    floor_y = lerp_int_round(seg->start_y, seg->end_y, abs(coord - seg->start), span ? span : 1);
    support = (((along + seg->support_phase) % 5) == 0);

    if(cross_delta >= -1 && cross_delta <= 1){
        u8 floor_block = BLK_PLANK + 1;
        carve_column_range(col, plane_stride, floor_y + 1, floor_y + 3);
        if(seg->tracks && cross_delta == 0 && !support && ((along & 3) == 0)){
            floor_block = (((along + seg->support_phase) & 4) == 0) ? (BLK_GRAVEL + 1) : (BLK_MOSSY_COBBLE + 1);
        }
        set_column_block(col, plane_stride, floor_y, floor_block);
        if(support) set_column_block(col, plane_stride, floor_y + 3, BLK_WOOD + 1);
    } else if(support) {
        set_column_block(col, plane_stride, floor_y, BLK_PLANK + 1);
        set_column_block(col, plane_stride, floor_y + 1, BLK_WOOD + 1);
        set_column_block(col, plane_stride, floor_y + 2, BLK_WOOD + 1);
        set_column_block(col, plane_stride, floor_y + 3, BLK_WOOD + 1);
    }

    if(seg->web_room && (coord == seg->end || coord == seg->end + ((seg->end >= seg->start) ? 2 : -2))){
        int room_x = seg->along_x ? (seg->end + ((seg->end >= seg->start) ? 3 : -3)) : seg->line;
        int room_z = seg->along_x ? seg->line : (seg->end + ((seg->end >= seg->start) ? 3 : -3));
        apply_mineshaft_room_to_column(gx, gz, col, plane_stride, room_x, room_z, seg->end_y, true, 0x57454221u);
    }
}

static void apply_single_mineshaft_to_column(int gx, int gz, u8* col, int plane_stride, const MineshaftDesc* shaft){
    if(!shaft->active) return;
    apply_mineshaft_room_to_column(gx, gz, col, plane_stride, shaft->hub_x, shaft->hub_z, shaft->hub_y, false, 0);
    for(int i = 0; i < shaft->segment_count; i++) apply_mineshaft_segment_to_column(gx, gz, col, plane_stride, &shaft->segments[i]);
}

static void apply_mineshafts_to_column(int gx, int gz, u8* col, int plane_stride){
    MineshaftDesc shaft;
    int guaranteed_count = (g_world_type == WORLD_HIGHLANDS) ? 2 : 1;
    int region_x = gx / 80;
    int region_z = gz / 80;
    for(int i = 0; i < guaranteed_count; i++){
        if(build_guaranteed_mineshaft_desc(&shaft, i)){
            apply_single_mineshaft_to_column(gx, gz, col, plane_stride, &shaft);
        }
    }
    for(int rz = region_z - 1; rz <= region_z + 1; rz++){
        if(rz < 0 || rz > ((VD - 1) / 80)) continue;
        for(int rx = region_x - 1; rx <= region_x + 1; rx++){
            if(rx < 0 || rx > ((VW - 1) / 80)) continue;
            if(build_mineshaft_desc(rx, rz, &shaft)){
                apply_single_mineshaft_to_column(gx, gz, col, plane_stride, &shaft);
            }
        }
    }
}

static void apply_tunnel_region_to_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride, int region_x, int region_z){
    u32 state = hash_region(0xA511E9B3u, region_x, region_z, 0);
    int tunnel_roll = rng_next_range(&state, 0, 99);
    int tunnel_count = (tunnel_roll < 20) ? 0 : ((tunnel_roll < 70) ? 1 : ((tunnel_roll < 95) ? 2 : 3));

    for(int i = 0; i < tunnel_count; i++){
        bool allow_surface = rng_next_range(&state, 0, 99) < 32;
        int start_x = clamp_int(region_x * 16 + rng_next_range(&state, 1, 14), 1, VW - 2);
        int start_z = clamp_int(region_z * 16 + rng_next_range(&state, 1, 14), 1, VD - 2);
        int start_y = allow_surface ? rng_next_range(&state, 14, 25) : rng_next_range(&state, 4, 16);
        int steps = rng_next_range(&state, 10, 18);
        int yaw = rng_next_range(&state, 0, LUT_N - 1);
        int pitch = allow_surface ? rng_next_range(&state, 4, 12) : rng_next_range(&state, -6, 6);
        int hr_base = 2 + rng_next_range(&state, 0, 1);
        fix step_len = allow_surface ? F(1.75f) : F(1.55f);
        fix posx = I2F(start_x) + F(0.5f);
        fix posy = I2F(start_y) + F(0.5f);
        fix posz = I2F(start_z) + F(0.5f);

        for(int step = 0; step < steps; step++){
            int widen = 0;
            if((step % 6) == 2 && rng_next_range(&state, 0, 99) < 55) widen = 1;
            if(allow_surface && step < 4) widen++;
            carve_ellipsoid_in_column(
                gx, gz, desc, col, plane_stride,
                (int)(posx >> 13), (int)(posy >> 13), (int)(posz >> 13),
                hr_base + widen, 2 + (widen > 0), allow_surface
            );

            fix pitch_cos = fcos(pitch);
            fix horiz_step = fmuli(pitch_cos, step_len);
            posx += fmuli(fcos(yaw), horiz_step);
            posz += fmuli(fsin(yaw), horiz_step);
            posy += fmuli(fsin(pitch), step_len);

            yaw = (yaw + rng_next_range(&state, -10, 10)) & (LUT_N - 1);
            pitch += rng_next_range(&state, -3, 3);
            if(allow_surface){
                if(step < (steps >> 1)) pitch += 1;
                if(posy > I2F(24)) pitch -= 4;
            } else {
                if(posy < I2F(5)) pitch += 3;
                if(posy > I2F(16)) pitch -= 3;
            }
            pitch = clamp_int(pitch, -26, 26);
        }
    }
}

static void apply_chamber_region_to_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride, int region_x, int region_z){
    u32 state = hash_region(0xB16B00B5u, region_x, region_z, 0);
    if(rng_next_range(&state, 0, 99) >= 18) return;

    int center_x = clamp_int(region_x * 32 + rng_next_range(&state, 6, 25), 2, VW - 3);
    int center_z = clamp_int(region_z * 32 + rng_next_range(&state, 6, 25), 2, VD - 3);
    int center_y = rng_next_range(&state, 5, 17);
    int hr = rng_next_range(&state, 4, 6);
    int vr = rng_next_range(&state, 3, 4);

    carve_ellipsoid_in_column(
        gx, gz, desc, col, plane_stride,
        center_x * 8 + 4, center_y * 8 + 4, center_z * 8 + 4,
        hr, vr, false
    );

    if(rng_next_range(&state, 0, 99) < 35){
        int ox = rng_next_range(&state, -2, 2);
        int oz = rng_next_range(&state, -2, 2);
        carve_ellipsoid_in_column(
            gx, gz, desc, col, plane_stride,
            (center_x + ox) * 8 + 4, (center_y + rng_next_range(&state, -1, 1)) * 8 + 4, (center_z + oz) * 8 + 4,
            hr - 1, vr, false
        );
    }
}

static void apply_ore_region_to_column(int gx, int gz, u8* col, int plane_stride, int region_x, int region_z, u8 ore_blk, int chance_pct, int max_y, int min_nodes, int max_nodes){
    u32 state = hash_region(0xC0FFEE11u ^ (u32)ore_blk, region_x, region_z, ore_blk);
    if(rng_next_range(&state, 0, 99) >= chance_pct) return;

    int nodes = rng_next_range(&state, min_nodes, max_nodes);
    int yaw = rng_next_range(&state, 0, LUT_N - 1);
    int pitch = rng_next_range(&state, -5, 5);
    fix posx = I2F(clamp_int(region_x * 16 + rng_next_range(&state, 2, 13), 1, VW - 2)) + F(0.5f);
    fix posy = I2F(rng_next_range(&state, 3, max_y)) + F(0.5f);
    fix posz = I2F(clamp_int(region_z * 16 + rng_next_range(&state, 2, 13), 1, VD - 2)) + F(0.5f);

    for(int i = 0; i < nodes; i++){
        int hr = 1 + (rng_next_range(&state, 0, 99) < 35);
        int vr = 1 + (hr > 1);
        place_ore_blob_in_column(
            gx, gz, col, plane_stride,
            (int)(posx >> 13), (int)(posy >> 13), (int)(posz >> 13),
            hr, vr, ore_blk
        );

        fix horiz_step = fmuli(fcos(pitch), F(0.95f));
        posx += fmuli(fcos(yaw), horiz_step);
        posz += fmuli(fsin(yaw), horiz_step);
        posy += fmuli(fsin(pitch), F(0.65f));
        yaw = (yaw + rng_next_range(&state, -14, 14)) & (LUT_N - 1);
        pitch += rng_next_range(&state, -3, 3);
        if(posy < I2F(3)) pitch += 3;
        if(posy > I2F(max_y)) pitch -= 3;
        pitch = clamp_int(pitch, -18, 18);
    }
}

static void carve_overworld_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride){
    int tunnel_rx = gx >> 4;
    int tunnel_rz = gz >> 4;
    int tunnel_max_x = (VW - 1) >> 4;
    int tunnel_max_z = (VD - 1) >> 4;
    for(int rz = tunnel_rz - 1; rz <= tunnel_rz + 1; rz++){
        if(rz < 0 || rz > tunnel_max_z) continue;
        for(int rx = tunnel_rx - 1; rx <= tunnel_rx + 1; rx++){
            if(rx < 0 || rx > tunnel_max_x) continue;
            apply_tunnel_region_to_column(gx, gz, desc, col, plane_stride, rx, rz);
        }
    }

    int chamber_rx = gx >> 5;
    int chamber_rz = gz >> 5;
    int chamber_max_x = (VW - 1) >> 5;
    int chamber_max_z = (VD - 1) >> 5;
    for(int rz = chamber_rz - 1; rz <= chamber_rz + 1; rz++){
        if(rz < 0 || rz > chamber_max_z) continue;
        for(int rx = chamber_rx - 1; rx <= chamber_rx + 1; rx++){
            if(rx < 0 || rx > chamber_max_x) continue;
            apply_chamber_region_to_column(gx, gz, desc, col, plane_stride, rx, rz);
        }
    }
}

static void place_overworld_ores_in_column(int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride){
    (void)desc;
    int ore_rx = gx >> 4;
    int ore_rz = gz >> 4;
    int ore_max_x = (VW - 1) >> 4;
    int ore_max_z = (VD - 1) >> 4;
    for(int rz = ore_rz - 1; rz <= ore_rz + 1; rz++){
        if(rz < 0 || rz > ore_max_z) continue;
        for(int rx = ore_rx - 1; rx <= ore_rx + 1; rx++){
            if(rx < 0 || rx > ore_max_x) continue;
            apply_ore_region_to_column(gx, gz, col, plane_stride, rx, rz, BLK_ORE_IRON, 46, 20, 5, 8);
            apply_ore_region_to_column(gx, gz, col, plane_stride, rx, rz, BLK_ORE_GOLD, 18, 14, 3, 5);
            apply_ore_region_to_column(gx, gz, col, plane_stride, rx, rz, BLK_ORE_DIAMOND, 10, 9, 2, 4);
        }
    }
    place_cave_exposed_iron_in_column(gx, gz, col, plane_stride);
}

static inline bool local_world_in_bounds(int x, int y, int z){
    return x >= 0 && x < CW && y >= 0 && y < CH && z >= 0 && z < CD;
}

static inline void local_world_set(int x, int y, int z, int blk){
    if(!local_world_in_bounds(x, y, z)) return;
    world[widx(x, y, z)] = (blk < 0) ? 0 : (u8)(blk + 1);
}

static inline u8 local_world_get_raw(int x, int y, int z){
    if(!local_world_in_bounds(x, y, z)) return 0;
    return world[widx(x, y, z)];
}

static void local_world_fill_box(int x0, int x1, int y0, int y1, int z0, int z1, int blk){
    if(x0 > x1){ int t = x0; x0 = x1; x1 = t; }
    if(y0 > y1){ int t = y0; y0 = y1; y1 = t; }
    if(z0 > z1){ int t = z0; z0 = z1; z1 = t; }
    if(x1 < 0 || x0 >= CW || y1 < 0 || y0 >= CH || z1 < 0 || z0 >= CD) return;
    x0 = clamp_int(x0, 0, CW - 1);
    x1 = clamp_int(x1, 0, CW - 1);
    y0 = clamp_int(y0, 0, CH - 1);
    y1 = clamp_int(y1, 0, CH - 1);
    z0 = clamp_int(z0, 0, CD - 1);
    z1 = clamp_int(z1, 0, CD - 1);
    for(int y = y0; y <= y1; y++){
        for(int z = z0; z <= z1; z++){
            for(int x = x0; x <= x1; x++) local_world_set(x, y, z, blk);
        }
    }
}

static void local_world_clear_box(int x0, int x1, int y0, int y1, int z0, int z1){
    local_world_fill_box(x0, x1, y0, y1, z0, z1, -1);
}

static void fortress_drop_support(int x, int start_y, int z){
    if(x <= 0 || x >= CW - 1 || z <= 0 || z >= CD - 1) return;
    for(int y = start_y; y > 1; y--){
        u8 raw = local_world_get_raw(x, y, z);
        local_world_set(x, y, z, BLK_NETHER_BRICK);
        if(y != start_y && raw != 0 && raw != BLK_LAVA + 1) break;
    }
}

static void fortress_corridor(bool along_x, int fixed, int start, int end, int y, bool enclosed, int support_phase){
    int a0 = start, a1 = end;
    if(a0 > a1){ int t = a0; a0 = a1; a1 = t; }
    if(along_x){
        local_world_clear_box(a0, a1, y, y + 4, fixed - 2, fixed + 2);
        local_world_fill_box(a0, a1, y, y, fixed - 2, fixed + 2, BLK_NETHER_BRICK);
        if(enclosed){
            local_world_fill_box(a0, a1, y + 1, y + 3, fixed - 2, fixed - 2, BLK_NETHER_BRICK);
            local_world_fill_box(a0, a1, y + 1, y + 3, fixed + 2, fixed + 2, BLK_NETHER_BRICK);
            local_world_fill_box(a0, a1, y + 4, y + 4, fixed - 2, fixed + 2, BLK_NETHER_BRICK);
            for(int x = a0 + 2 + (support_phase & 1); x <= a1 - 2; x += 4){
                local_world_clear_box(x, x, y + 2, y + 2, fixed - 2, fixed - 2);
                local_world_clear_box(x, x, y + 2, y + 2, fixed + 2, fixed + 2);
            }
        } else {
            local_world_fill_box(a0, a1, y + 1, y + 1, fixed - 2, fixed - 2, BLK_NETHER_BRICK);
            local_world_fill_box(a0, a1, y + 1, y + 1, fixed + 2, fixed + 2, BLK_NETHER_BRICK);
        }
    } else {
        local_world_clear_box(fixed - 2, fixed + 2, y, y + 4, a0, a1);
        local_world_fill_box(fixed - 2, fixed + 2, y, y, a0, a1, BLK_NETHER_BRICK);
        if(enclosed){
            local_world_fill_box(fixed - 2, fixed - 2, y + 1, y + 3, a0, a1, BLK_NETHER_BRICK);
            local_world_fill_box(fixed + 2, fixed + 2, y + 1, y + 3, a0, a1, BLK_NETHER_BRICK);
            local_world_fill_box(fixed - 2, fixed + 2, y + 4, y + 4, a0, a1, BLK_NETHER_BRICK);
            for(int z = a0 + 2 + (support_phase & 1); z <= a1 - 2; z += 4){
                local_world_clear_box(fixed - 2, fixed - 2, y + 2, y + 2, z, z);
                local_world_clear_box(fixed + 2, fixed + 2, y + 2, y + 2, z, z);
            }
        } else {
            local_world_fill_box(fixed - 2, fixed - 2, y + 1, y + 1, a0, a1, BLK_NETHER_BRICK);
            local_world_fill_box(fixed + 2, fixed + 2, y + 1, y + 1, a0, a1, BLK_NETHER_BRICK);
        }
    }

    if(along_x){
        fortress_drop_support(a0, y - 1, fixed - 2);
        fortress_drop_support(a0, y - 1, fixed + 2);
        fortress_drop_support(a1, y - 1, fixed - 2);
        fortress_drop_support(a1, y - 1, fixed + 2);
    } else {
        fortress_drop_support(fixed - 2, y - 1, a0);
        fortress_drop_support(fixed + 2, y - 1, a0);
        fortress_drop_support(fixed - 2, y - 1, a1);
        fortress_drop_support(fixed + 2, y - 1, a1);
    }

    int step = enclosed ? 7 : 6;
    int first = a0 + 2 + (support_phase % 3);
    for(int a = first; a < a1; a += step){
        if(along_x){
            fortress_drop_support(a, y - 1, fixed - 2);
            fortress_drop_support(a, y - 1, fixed + 2);
        } else {
            fortress_drop_support(fixed - 2, y - 1, a);
            fortress_drop_support(fixed + 2, y - 1, a);
        }
    }
}

static void fortress_crossroad(int cx, int cz, int y){
    local_world_clear_box(cx - 4, cx + 4, y, y + 4, cz - 4, cz + 4);
    local_world_fill_box(cx - 4, cx + 4, y, y, cz - 4, cz + 4, BLK_NETHER_BRICK);
    local_world_fill_box(cx - 1, cx + 1, y + 1, y + 1, cz - 1, cz + 1, BLK_NETHER_BRICK);

    for(int x = cx - 4; x <= cx + 4; x++){
        if(x < cx - 1 || x > cx + 1){
            local_world_set(x, y + 1, cz - 4, BLK_NETHER_BRICK);
            local_world_set(x, y + 1, cz + 4, BLK_NETHER_BRICK);
        }
    }
    for(int z = cz - 4; z <= cz + 4; z++){
        if(z < cz - 1 || z > cz + 1){
            local_world_set(cx - 4, y + 1, z, BLK_NETHER_BRICK);
            local_world_set(cx + 4, y + 1, z, BLK_NETHER_BRICK);
        }
    }

    fortress_drop_support(cx - 4, y - 1, cz - 4);
    fortress_drop_support(cx + 4, y - 1, cz - 4);
    fortress_drop_support(cx - 4, y - 1, cz + 4);
    fortress_drop_support(cx + 4, y - 1, cz + 4);
}

static void fortress_open_room(int cx, int cz, int y, int radius){
    local_world_clear_box(cx - radius, cx + radius, y, y + 5, cz - radius, cz + radius);
    local_world_fill_box(cx - radius, cx + radius, y, y, cz - radius, cz + radius, BLK_NETHER_BRICK);
    for(int x = cx - radius; x <= cx + radius; x++){
        if(x != cx - 1 && x != cx && x != cx + 1){
            local_world_set(x, y + 1, cz - radius, BLK_NETHER_BRICK);
            local_world_set(x, y + 1, cz + radius, BLK_NETHER_BRICK);
        }
    }
    for(int z = cz - radius; z <= cz + radius; z++){
        if(z != cz - 1 && z != cz && z != cz + 1){
            local_world_set(cx - radius, y + 1, z, BLK_NETHER_BRICK);
            local_world_set(cx + radius, y + 1, z, BLK_NETHER_BRICK);
        }
    }
    fortress_drop_support(cx - radius, y - 1, cz - radius);
    fortress_drop_support(cx + radius, y - 1, cz - radius);
    fortress_drop_support(cx - radius, y - 1, cz + radius);
    fortress_drop_support(cx + radius, y - 1, cz + radius);
}

static void fortress_enclosed_room(int cx, int cz, int y, int half_x, int half_z){
    local_world_clear_box(cx - half_x, cx + half_x, y, y + 4, cz - half_z, cz + half_z);
    local_world_fill_box(cx - half_x, cx + half_x, y, y, cz - half_z, cz + half_z, BLK_NETHER_BRICK);
    local_world_fill_box(cx - half_x, cx + half_x, y + 1, y + 3, cz - half_z, cz - half_z, BLK_NETHER_BRICK);
    local_world_fill_box(cx - half_x, cx + half_x, y + 1, y + 3, cz + half_z, cz + half_z, BLK_NETHER_BRICK);
    local_world_fill_box(cx - half_x, cx - half_x, y + 1, y + 3, cz - half_z, cz + half_z, BLK_NETHER_BRICK);
    local_world_fill_box(cx + half_x, cx + half_x, y + 1, y + 3, cz - half_z, cz + half_z, BLK_NETHER_BRICK);
    local_world_fill_box(cx - half_x, cx + half_x, y + 4, y + 4, cz - half_z, cz + half_z, BLK_NETHER_BRICK);

    for(int x = cx - half_x + 2; x <= cx + half_x - 2; x += 4){
        local_world_clear_box(x, x, y + 2, y + 2, cz - half_z, cz - half_z);
        local_world_clear_box(x, x, y + 2, y + 2, cz + half_z, cz + half_z);
    }

    fortress_drop_support(cx - half_x, y - 1, cz - half_z);
    fortress_drop_support(cx + half_x, y - 1, cz - half_z);
    fortress_drop_support(cx - half_x, y - 1, cz + half_z);
    fortress_drop_support(cx + half_x, y - 1, cz + half_z);
}

static void fortress_stair_run_x(int start_x, int zc, int start_y, int dir, int rises, int* out_end_x, int* out_end_y){
    int cur_x = start_x;
    int cur_y = start_y;
    for(int i = 0; i < rises; i++){
        fortress_corridor(true, zc, cur_x, cur_x + dir, cur_y, false, i);
        cur_x += dir * 2;
        cur_y++;
    }
    fortress_corridor(true, zc, cur_x, cur_x + dir * 2, cur_y, false, rises);
    *out_end_x = cur_x + dir * 2;
    *out_end_y = cur_y;
}

static void place_nether_fortress(int cx, int cz, int base_y, int gx, int gz){
    u32 state = hash_region(0x4E465452u, gx, base_y, gz);
    int west_len = rng_next_range(&state, 10, 16);
    int east_len = rng_next_range(&state, 12, 18);
    int north_len = rng_next_range(&state, 8, 14);
    int south_len = rng_next_range(&state, 10, 15);
    int support_phase = rng_next_range(&state, 0, 3);
    int stair_rises = rng_next_range(&state, 2, 3);
    bool stairs_west = (rng_next_range(&state, 0, 99) < 45);

    fortress_crossroad(cx, cz, base_y);
    fortress_corridor(true, cz, cx - west_len - 4, cx - 5, base_y, false, support_phase);
    fortress_corridor(true, cz, cx + 5, cx + east_len + 4, base_y, false, support_phase + 1);
    fortress_corridor(false, cx, cz - north_len - 4, cz - 5, base_y, false, support_phase + 2);
    fortress_corridor(false, cx, cz + 5, cz + south_len + 4, base_y, false, support_phase + 3);

    {
        int blaze_cz = cz - north_len - 8;
        fortress_open_room(cx, blaze_cz, base_y, 4);
        local_world_fill_box(cx - 1, cx + 1, base_y + 1, base_y + 1, blaze_cz - 1, blaze_cz + 1, BLK_NETHER_BRICK);
        local_world_fill_box(cx, cx, base_y + 2, base_y + 3, blaze_cz, blaze_cz, BLK_BASALT);
        local_world_clear_box(cx - 1, cx + 1, base_y + 1, base_y + 3, blaze_cz + 4, blaze_cz + 4);
    }

    {
        int farm_cz = cz + south_len + 8;
        fortress_enclosed_room(cx, farm_cz, base_y, 4, 4);
        local_world_fill_box(cx - 2, cx + 2, base_y, base_y, farm_cz - 2, farm_cz + 2, BLK_NETHERRACK);
        local_world_fill_box(cx - 1, cx + 1, base_y, base_y, farm_cz - 3, farm_cz + 3, BLK_BASALT);
        local_world_clear_box(cx - 1, cx + 1, base_y + 1, base_y + 3, farm_cz - 4, farm_cz - 4);
    }

    {
        int dir = stairs_west ? -1 : 1;
        int branch_end = stairs_west ? (cx - west_len - 4) : (cx + east_len + 4);
        int stair_end_x, stair_end_y;
        int hall_from, hall_to;
        int hall_center_x;

        fortress_stair_run_x(branch_end + dir, cz, base_y, dir, stair_rises, &stair_end_x, &stair_end_y);
        hall_from = stair_end_x + dir;
        hall_to = hall_from + dir * 7;
        fortress_corridor(true, cz, hall_from, hall_to, stair_end_y, true, support_phase + 1);
        hall_center_x = hall_to + dir * 4;
        fortress_enclosed_room(hall_center_x, cz, stair_end_y, 5, 3);
        if(dir > 0) local_world_clear_box(hall_center_x - 5, hall_center_x - 5, stair_end_y + 1, stair_end_y + 3, cz - 1, cz + 1);
        else         local_world_clear_box(hall_center_x + 5, hall_center_x + 5, stair_end_y + 1, stair_end_y + 3, cz - 1, cz + 1);
    }
}
