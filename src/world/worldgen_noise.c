/* ===================== WORLD GENERATION ==================== */
static inline u32 noise2_u16(int x,int z){
    u32 s = (u32)( (u32)(x + (int)g_world_seed) * 73856093u) ^ (u32)( (u32)(z + (int)(g_world_seed>>1)) * 19349663u);
    s ^= s>>13; s *= 0x27d4eb2d; s ^= s>>15;
    return (s & 0xFFFFu);
}

static fix vnoise_norm(int x, int z, int scalePow2, int seed){
    int mask = (1<<scalePow2) - 1;
    int gx = x >> scalePow2, gz = z >> scalePow2;
    fix fx = (fix)(((u32)(x & mask) << PRECISION) >> scalePow2);
    fix fz = (fix)(((u32)(z & mask) << PRECISION) >> scalePow2);
    u32 n00 = noise2_u16(gx+seed,   gz+seed);
    u32 n10 = noise2_u16(gx+1+seed, gz+seed);
    u32 n01 = noise2_u16(gx+seed,   gz+1+seed);
    u32 n11 = noise2_u16(gx+1+seed, gz+1+seed);
    fix a = (fix)n00;
    fix b = (fix)n10;
    fix c = (fix)n01;
    fix d = (fix)n11;
    return flerp(flerp(a, b, fx), flerp(c, d, fx), fz);
}

enum {
    BIOME_OCEAN = 0,
    BIOME_GRASSLAND,
    BIOME_DESERT,
    BIOME_DARK_FOREST,
    BIOME_JUNGLE
};

static inline int biome_type_at(int x, int z){
    fix temp = vnoise_norm(x, z, 4, 211);
    fix moist = vnoise_norm(x, z, 4, 97);
    if(temp < F(0.18f)) return BIOME_OCEAN;
    if(temp < F(0.62f)) return BIOME_GRASSLAND;
    if(temp < F(0.82f)) return BIOME_DESERT;
    return (moist < F(0.5f)) ? BIOME_DARK_FOREST : BIOME_JUNGLE;
}

static inline u32 mix32(u32 x){
    x ^= x >> 16;
    x *= 0x7feb352du;
    x ^= x >> 15;
    x *= 0x846ca68bu;
    x ^= x >> 16;
    return x;
}

static inline u32 hash_region(u32 salt, int a, int b, int c){
    u32 x = (u32)a * 0x9E3779B9u;
    u32 y = (u32)b * 0x85EBCA6Bu;
    u32 z = (u32)c * 0xC2B2AE35u;
    return mix32(g_world_seed ^ salt ^ x ^ y ^ z);
}

static inline int rng_next_range(u32* state, int min_v, int max_v){
    *state = mix32(*state + 0x9E3779B9u);
    if(max_v <= min_v) return min_v;
    return min_v + (int)(*state % (u32)(max_v - min_v + 1));
}

static inline int clamp_int(int v, int lo, int hi){
    if(v < lo) return lo;
    if(v > hi) return hi;
    return v;
}

static inline bool block_is_overworld_natural(u8 blk){
    return blk == BLK_GRASS || blk == BLK_DIRT || blk == BLK_STONE ||
           blk == BLK_SAND || blk == BLK_GRAVEL || blk == BLK_CLAY;
}

static inline bool block_is_cave_floor_solid(u8 blk){
    return blk == BLK_STONE || blk == BLK_DIRT || blk == BLK_GRAVEL ||
           blk == BLK_CLAY || blk == BLK_SAND || blk == BLK_COBBLE ||
           blk == BLK_BASALT || blk == BLK_ORE_IRON || blk == BLK_ORE_GOLD ||
           blk == BLK_ORE_DIAMOND;
}

static void carve_ellipsoid_in_column(
    int gx, int gz, const ColumnGenDesc* desc, u8* col, int plane_stride,
    int cx8, int cy8, int cz8, int hr, int vr, bool allow_surface
){
    int x8 = gx * 8 + 4;
    int z8 = gz * 8 + 4;
    int dx = x8 - cx8;
    int dz = z8 - cz8;
    int hr8 = hr * 8;
    int vr8 = vr * 8;
    if(dx > hr8 || dx < -hr8 || dz > hr8 || dz < -hr8) return;
    int dist2 = dx * dx + dz * dz;
    if(dist2 > hr8 * hr8) return;

    int y0 = clamp_int((cy8 - vr8 - 4) / 8, 1, CH - 2);
    int y1 = clamp_int((cy8 + vr8 + 4) / 8, 1, CH - 2);
    int hr2 = hr8 * hr8;
    int vr2 = vr8 * vr8;
    int horiz = dist2 * vr2;
    int rhs = hr2 * vr2;
    int surface_y = desc ? desc->surface_y : (CH - 1);

    for(int y = y0; y <= y1; y++){
        if(!allow_surface && y >= surface_y) continue;
        int dy = y * 8 + 4 - cy8;
        int lhs = horiz + dy * dy * hr2;
        if(lhs > rhs) continue;
        u8 id = col[y * plane_stride];
        if(!id) continue;
        u8 blk = (u8)(id - 1);
        if(!block_is_overworld_natural(blk)) continue;
        col[y * plane_stride] = 0;
    }
}

static void place_ore_blob_in_column(
    int gx, int gz, u8* col, int plane_stride,
    int cx8, int cy8, int cz8, int hr, int vr, u8 ore_blk
){
    int x8 = gx * 8 + 4;
    int z8 = gz * 8 + 4;
    int dx = x8 - cx8;
    int dz = z8 - cz8;
    int hr8 = hr * 8;
    int vr8 = vr * 8;
    if(dx > hr8 || dx < -hr8 || dz > hr8 || dz < -hr8) return;
    int dist2 = dx * dx + dz * dz;
    if(dist2 > hr8 * hr8) return;

    int y0 = clamp_int((cy8 - vr8 - 4) / 8, 1, CH - 2);
    int y1 = clamp_int((cy8 + vr8 + 4) / 8, 1, CH - 2);
    int hr2 = hr8 * hr8;
    int vr2 = vr8 * vr8;
    int horiz = dist2 * vr2;
    int rhs = hr2 * vr2;

    for(int y = y0; y <= y1; y++){
        int dy = y * 8 + 4 - cy8;
        int lhs = horiz + dy * dy * hr2;
        if(lhs > rhs) continue;
        if(col[y * plane_stride] == BLK_STONE + 1) col[y * plane_stride] = ore_blk + 1;
    }
}

static void place_cave_lava_pool_in_column(
    int gx, int gz, u8* col, int plane_stride,
    int cx8, int cy8, int cz8, int hr
){
    int x8 = gx * 8 + 4;
    int z8 = gz * 8 + 4;
    int dx = x8 - cx8;
    int dz = z8 - cz8;
    int hr8 = hr * 8;
    if(dx > hr8 || dx < -hr8 || dz > hr8 || dz < -hr8) return;
    int dist2 = dx * dx + dz * dz;
    if(dist2 > hr8 * hr8) return;

    int y = clamp_int((cy8 + 4) / 8, 1, 12);
    int idx = y * plane_stride;
    if(col[idx] == 0){
        if(!col[idx - plane_stride]) return;
        if(!block_is_cave_floor_solid((u8)(col[idx - plane_stride] - 1))) return;
    } else {
        int clear_top = y;
        int inner_limit2 = (hr8 * hr8 * 9) / 25;
        if(dist2 > inner_limit2) return;
        while(clear_top <= y + 2 && clear_top < CH - 1){
            int clear_idx = clear_top * plane_stride;
            if(col[clear_idx] == 0) break;
            if(!block_is_cave_floor_solid((u8)(col[clear_idx] - 1))) return;
            clear_top++;
        }
        if(clear_top == y || clear_top > y + 2 || clear_top >= CH - 1) return;
        if(col[clear_top * plane_stride] != 0) return;
        if(!col[idx - plane_stride]) return;
        if(!block_is_cave_floor_solid((u8)(col[idx - plane_stride] - 1))) return;
        for(int clear_y = y; clear_y < clear_top; clear_y++) col[clear_y * plane_stride] = 0;
    }
    col[idx] = BLK_LAVA + 1;
}

static void place_cave_exposed_iron_in_column(int gx, int gz, u8* col, int plane_stride){
    int placed = 0;
    for(int y = 5; y <= 18 && placed < 2; y++){
        int idx = y * plane_stride;
        if(col[idx] != BLK_STONE + 1) continue;
        if(col[idx - plane_stride] != 0 && col[idx + plane_stride] != 0) continue;
        u32 roll = noise2_u16(gx * 19 + y * 7, gz * 23 + y * 13);
        if((roll % 100u) >= 7u) continue;
        col[idx] = BLK_ORE_IRON + 1;
        placed++;
    }
}

static void place_cave_lava_in_column(int gx, int gz, u8* col, int plane_stride){
    int chamber_rx = gx >> 5;
    int chamber_rz = gz >> 5;
    int chamber_max_x = (VW - 1) >> 5;
    int chamber_max_z = (VD - 1) >> 5;
    for(int rz = chamber_rz - 1; rz <= chamber_rz + 1; rz++){
        if(rz < 0 || rz > chamber_max_z) continue;
        for(int rx = chamber_rx - 1; rx <= chamber_rx + 1; rx++){
            if(rx < 0 || rx > chamber_max_x) continue;

            u32 state = hash_region(0xB16B00B5u, rx, rz, 0);
            if(rng_next_range(&state, 0, 99) >= 46) continue;

            int center_x = clamp_int(rx * 32 + rng_next_range(&state, 6, 25), 2, VW - 3);
            int center_z = clamp_int(rz * 32 + rng_next_range(&state, 6, 25), 2, VD - 3);
            int center_y = rng_next_range(&state, 4, 20);
            int hr = rng_next_range(&state, 5, 9);
            int vr = rng_next_range(&state, 3, 5);
            bool has_branch = rng_next_range(&state, 0, 99) < 55;
            if(has_branch){
                (void)rng_next_range(&state, -2, 2);
                (void)rng_next_range(&state, -2, 2);
                (void)rng_next_range(&state, -1, 1);
            }
            if(rng_next_range(&state, 0, 99) >= 58) continue;

            int lava_y = center_y - vr + 1;
            if(lava_y > 12) continue;
            if(lava_y > 6 && rng_next_range(&state, 0, 99) >= 35) continue;
            place_cave_lava_pool_in_column(gx, gz, col, plane_stride, center_x * 8 + 4, lava_y * 8 + 4, center_z * 8 + 4, hr);
            if(hr >= 7 && rng_next_range(&state, 0, 99) < 60){
                int branch_dx = rng_next_range(&state, -2, 2);
                int branch_dz = rng_next_range(&state, -2, 2);
                place_cave_lava_pool_in_column(
                    gx, gz, col, plane_stride,
                    (center_x + branch_dx) * 8 + 4, lava_y * 8 + 4, (center_z + branch_dz) * 8 + 4,
                    hr - 2
                );
            }
        }
    }
}

