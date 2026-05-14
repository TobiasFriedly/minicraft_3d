static void prepare_column_desc(int gx, int gz, ColumnGenDesc* out){
    memset(out, 0, sizeof(*out));
    if(g_dimension == 1){
        fix floor1 = vnoise_norm(gx, gz, 4, 100);
        fix floor2 = vnoise_norm(gx, gz, 3, 200);
        fix floor3 = vnoise_norm(gx, gz, 5, 511);
        fix roof1 = vnoise_norm(gx, gz, 4, 311);
        fix roof2 = vnoise_norm(gx, gz, 3, 417);
        fix roof3 = vnoise_norm(gx, gz, 5, 619);
        int h = 4 + F2I(fmuli(floor1, F(8)) + fmuli(floor2, F(5)) + fmuli(floor3, F(3)));
        int roof_y = (CH - 8) - F2I(fmuli(roof1, F(8)) + fmuli(roof2, F(6)) + fmuli(roof3, F(4)));
        int min_gap = 12 + ((noise2_u16(gx + 222, gz + 444) >> 7) & 3);
        if(h < 3) h = 3;
        if(h > 19) h = 19;
        if(roof_y < h + min_gap) roof_y = h + min_gap;
        if(roof_y > CH - 5) roof_y = CH - 5;
        out->top_block = BLK_NETHERRACK;
        out->flags = COL_FLAG_NETHER;
        out->surface_y = (u8)(h - 1);
        out->nether_roof_y = (u8)roof_y;
        if(gx >= 12 && gx < VW - 12 && gz >= 12 && gz < VD - 12 &&
           ((gx - 12) & 31) == 0 && ((gz - 12) & 31) == 0 &&
           ((noise2_u16(gx + 400, gz + 900) & 255) < 112)){
            out->deco_heavy |= DECO_HEAVY_FORTRESS;
        }
        return;
    }

    if(g_world_type == WORLD_SUPERFLAT){
        out->surface_y = 10;
        out->top_block = BLK_GRASS;
        out->flags = COL_FLAG_SUPERFLAT;
        return;
    }

    const bool highlands = (g_world_type == WORLD_HIGHLANDS);
    int biome = biome_type_at(gx, gz);
    bool is_ocean = (biome == BIOME_OCEAN);
    bool is_desert = (biome == BIOME_DESERT);
    fix n1 = vnoise_norm(gx, gz, highlands ? 5 : 4, highlands ? 13 : 11);
    fix n2 = vnoise_norm(gx, gz, 3, highlands ? 41 : 37);
    fix n3 = vnoise_norm(gx, gz, 2, highlands ? 83 : 73);
    int h = 10 + F2I(fmuli(n1, highlands ? F(16) : F(12))
                   + fmuli(n2, highlands ? F(8) : F(6))
                   + fmuli(n3, highlands ? F(6) : F(3)));
    if(is_ocean) h = (highlands ? 5 : 6) + F2I(fmuli(fmuli(n1, highlands ? F(16) : F(12))
                                                   + fmuli(n2, highlands ? F(8) : F(6))
                                                   + fmuli(n3, highlands ? F(6) : F(3)),
                                                   highlands ? F(0.35f) : F(0.4f)));
    if(h < 6) h = 6;
    if(h > CH-2) h = CH-2;

    out->surface_y = (u8)h;
    out->biome = (u8)biome;
    if(is_desert) out->top_block = BLK_SAND;
    else if(is_ocean) out->top_block = (noise2_u16(gx+17, gz+29) & 1) ? BLK_SAND : BLK_CLAY;
    else out->top_block = BLK_GRASS;

    if(h <= 15 && h >= 13 && !is_ocean) out->flags |= COL_FLAG_SAND_BEACH;
    if(is_ocean) out->flags |= COL_FLAG_OCEAN;
    if(highlands) out->flags |= COL_FLAG_HIGHLANDS;
    if((out->flags & COL_FLAG_SAND_BEACH) && !(out->flags & COL_FLAG_OCEAN)) out->top_block = BLK_SAND;
    if(highlands && h > 24 && !is_ocean) out->top_block = BLK_STONE;

    if(biome == BIOME_DARK_FOREST && (noise2_u16(gx+321, gz+888) & 31) == 0) out->deco_light |= DECO_LIGHT_MOSS;
    if(is_desert && h >= 15 && !(out->flags & COL_FLAG_SAND_BEACH) &&
       (noise2_u16(gx+501, gz+901) & 255) < (u32)(highlands ? 12 : 10)) out->deco_light |= DECO_LIGHT_CACTUS;
    if((biome == BIOME_GRASSLAND || biome == BIOME_DARK_FOREST || biome == BIOME_JUNGLE) &&
       h >= 15 && !(out->flags & COL_FLAG_SAND_BEACH) && !is_ocean){
        int chance = (biome == BIOME_JUNGLE) ? (highlands ? 5 : 7)
                   : (biome == BIOME_DARK_FOREST) ? (highlands ? 9 : 14)
                   : (highlands ? 20 : 24);
        if((noise2_u16(gx+101, gz+777) & 1023) < (u32)chance) out->deco_heavy |= DECO_HEAVY_TREE;
    }
    if(highlands && out->top_block == BLK_STONE && h >= 16 && h < CH-3 &&
       (noise2_u16(gx+333, gz+999) & 4095) < 2) out->deco_heavy |= DECO_HEAVY_LAVA;
}

static void write_base_column(int x, int z, const ColumnGenDesc* desc){
    const int plane_stride = CW * CD;
    u8* col = &world[phys_z_from_local(z) * CW + phys_x_from_local(x)];
    for(int y=0; y<CH; y++, col += plane_stride) *col = 0;
    col = &world[phys_z_from_local(z) * CW + phys_x_from_local(x)];

    if(desc->flags & COL_FLAG_NETHER){
        int gx = global_x_from_local(x);
        int gz = global_z_from_local(z);
        int h = desc->surface_y + 1;
        int roof_y = desc->nether_roof_y;
        int lava_top = 12;
        int stal_len = 0;
        int stal_blk = BLK_NETHERRACK;
        col[0] = BLK_BEDROCK+1;
        col[(CH-1) * plane_stride] = BLK_BEDROCK+1;
        if((noise2_u16(gx + 611, gz + 877) & 255) < 54){
            stal_len = 2 + ((noise2_u16(gx + 177, gz + 333) >> 8) & 7);
            if(roof_y - stal_len < h + 6) stal_len = clamp_int(roof_y - (h + 6), 0, 10);
            if((noise2_u16(gx + 921, gz + 71) & 63) < 18) stal_blk = BLK_BASALT;
        }
        for(int y=1; y<CH-1; y++){
            if(y < h) col[y * plane_stride] = BLK_NETHERRACK+1;
            else if(y < lava_top) col[y * plane_stride] = BLK_LAVA+1;
            else if(y >= roof_y) col[y * plane_stride] = BLK_NETHERRACK+1;
            else if(stal_len > 0 && y >= roof_y - stal_len) col[y * plane_stride] = (u8)(stal_blk + 1);
        }
        return;
    }

    if(desc->flags & COL_FLAG_SUPERFLAT){
        const int flatHeight = 10;
        col[0] = BLK_BEDROCK+1;
        for(int y=1; y<flatHeight-3; y++) col[y * plane_stride] = BLK_STONE+1;
        for(int y=flatHeight-3; y<flatHeight; y++) col[y * plane_stride] = BLK_DIRT+1;
        col[flatHeight * plane_stride] = BLK_GRASS+1;
        return;
    }

    const int seaLevel = 14;
    int h = desc->surface_y;
    u8 fillBlock = BLK_DIRT;
    if(desc->biome == BIOME_DESERT || (desc->flags & COL_FLAG_SAND_BEACH)) fillBlock = BLK_SAND;
    else if(desc->flags & COL_FLAG_OCEAN) fillBlock = BLK_CLAY;
    if((desc->flags & COL_FLAG_HIGHLANDS) && desc->top_block == BLK_STONE) fillBlock = BLK_STONE;

    col[0] = BLK_BEDROCK+1;
    for(int y=1; y<CH; y++){
        if      (y < h-4)                col[y * plane_stride] = BLK_STONE+1;
        else if (y < h)                  col[y * plane_stride] = fillBlock+1;
        else if (y == h)                 col[y * plane_stride] = desc->top_block+1;
        else if (y <= seaLevel && y > h) col[y * plane_stride] = BLK_WATER+1;
    }
}

static void apply_overworld_column_features(int x, int z, const ColumnGenDesc* desc){
    if(desc->flags & (COL_FLAG_NETHER | COL_FLAG_SUPERFLAT)) return;
    const int plane_stride = CW * CD;
    u8* col = &world[phys_z_from_local(z) * CW + phys_x_from_local(x)];
    int gx = global_x_from_local(x);
    int gz = global_z_from_local(z);
    int ravine_count = collect_ravines_for_column(gx, gz, g_ravine_work, MAX_COLUMN_RAVINES);
    carve_overworld_column(gx, gz, desc, col, plane_stride);
    apply_ravine_list_to_column(gx, gz, desc, col, plane_stride, g_ravine_work, ravine_count, false);
    place_cave_lava_in_column(gx, gz, col, plane_stride);
    apply_ravine_list_to_column(gx, gz, desc, col, plane_stride, g_ravine_work, ravine_count, true);
    place_overworld_ores_in_column(gx, gz, desc, col, plane_stride);
    apply_mineshafts_to_column(gx, gz, col, plane_stride);
}

static void decorate_light_column_from_desc(int x, int z, const ColumnGenDesc* desc){
    int gx = global_x_from_local(x);
    int gz = global_z_from_local(z);
    if(desc->flags & COL_FLAG_SUPERFLAT) return;

    const int seaLevel = 14;
    int h = desc->surface_y;
    bool sandBeach = (desc->flags & COL_FLAG_SAND_BEACH) != 0;
    bool is_desert = (desc->biome == BIOME_DESERT);

    if(desc->deco_light & DECO_LIGHT_MOSS){
        world[widx(x,h,z)] = BLK_MOSSY_COBBLE+1;
    }

    if((desc->deco_light & DECO_LIGHT_CACTUS) && is_desert && h >= seaLevel + 1 && !sandBeach){
        int ch = 2 + (noise2_u16(gx+77, gz+19) & 1);
        for(int i=0; i<ch && (h+1+i) < CH-1; i++) world[widx(x, h+1+i, z)] = BLK_CACTUS+1;
    }
}

static void decorate_heavy_column_from_desc(int x, int z, const ColumnGenDesc* desc){
    int gx = global_x_from_local(x);
    int gz = global_z_from_local(z);

    if(desc->flags & COL_FLAG_NETHER){
        if(desc->deco_heavy & DECO_HEAVY_FORTRESS){
            int base_y = 14 + (noise2_u16(gx + 77, gz + 33) & 3);
            place_nether_fortress(x, z, base_y, gx, gz);
        }
        return;
    }

    if(desc->flags & COL_FLAG_SUPERFLAT) return;

    int h = desc->surface_y;
    bool highlands = (desc->flags & COL_FLAG_HIGHLANDS) != 0;
    bool is_ocean = (desc->flags & COL_FLAG_OCEAN) != 0;
    bool sandBeach = (desc->flags & COL_FLAG_SAND_BEACH) != 0;
    bool is_grass = (desc->biome == BIOME_GRASSLAND);
    bool is_dark_forest = (desc->biome == BIOME_DARK_FOREST);
    bool is_jungle = (desc->biome == BIOME_JUNGLE);

    if((desc->deco_heavy & DECO_HEAVY_TREE) && (is_grass || is_dark_forest || is_jungle) && !sandBeach && !is_ocean){
        u8 tree_wood = BLK_WOOD;
        u8 tree_leaf = BLK_LEAF;
        if(is_dark_forest){ tree_wood = BLK_WOOD_DARK; tree_leaf = BLK_LEAF_DARK; }
        else if(is_jungle){ tree_wood = BLK_WOOD_LIGHT; tree_leaf = BLK_LEAF_LIGHT; }
        int ty = h + 1;
        int th = (is_jungle ? 6 : 4) + (noise2_u16(gx+55, gz+33) & 1);
        for(int i=0; i<th && ty+i < CH; i++) world[widx(x, ty+i, z)] = tree_wood+1;
        int top = ty + th;
        for(int dy=-2; dy<=2; dy++) for(int dx=-2; dx<=2; dx++) for(int dz=-2; dz<=2; dz++){
            if(abs(dx)+abs(dz) > 3) continue;
            int lx = x + dx;
            int ly = top + dy;
            int lz = z + dz;
            if(lx>=0 && lx<CW && ly>=0 && ly<CH && lz>=0 && lz<CD){
                if(world[widx(lx,ly,lz)] == 0) world[widx(lx,ly,lz)] = tree_leaf+1;
            }
        }
    }

    if((desc->deco_heavy & DECO_HEAVY_LAVA) && highlands && desc->top_block == BLK_STONE && h >= 16 && h < CH-3){
        for(int dz=-2; dz<=2; dz++){
            for(int dx=-2; dx<=2; dx++){
                int lx = x + dx;
                int lz = z + dz;
                if(lx <= 1 || lx >= CW-2 || lz <= 1 || lz >= CD-2) continue;
                if(abs(dx) <= 1 && abs(dz) <= 1) world[widx(lx, h, lz)] = BLK_LAVA+1;
                else world[widx(lx, h, lz)] = BLK_STONE+1;
            }
        }
    }
}

static bool stream_rect_near_player(StreamRect rect, int dist){
    int px = F2I(pl.pos.x);
    int pz = F2I(pl.pos.z);
    int dx = 0, dz = 0;
    if(px < rect.x0) dx = rect.x0 - px;
    else if(px >= rect.x1) dx = px - (rect.x1 - 1);
    if(pz < rect.z0) dz = rect.z0 - pz;
    else if(pz >= rect.z1) dz = pz - (rect.z1 - 1);
    return dx <= dist && dz <= dist;
}

typedef struct {
    int start;
    int end;
} WrappedSpan;

static int split_wrapped_span(int origin, int size, int start, int end, WrappedSpan out[2]){
    int len = end - start;
    int p0 = origin + start;
    if(p0 >= size) p0 -= size;
    if(len <= 0) return 0;
    if(p0 + len <= size){
        out[0].start = p0;
        out[0].end = p0 + len;
        return 1;
    }
    out[0].start = p0;
    out[0].end = size;
    out[1].start = 0;
    out[1].end = len - (size - p0);
    return 2;
}

static void clear_strip_region(const StreamRect* r){
    WrappedSpan zspans[2], xspans[2];
    int zcount = split_wrapped_span(g_world_ring_z, CD, r->z0, r->z1, zspans);
    int xcount = split_wrapped_span(g_world_ring_x, CW, r->x0, r->x1, xspans);
    for(int y=0; y<CH; y++){
        int ybase = y * CD * CW;
        for(int zi=0; zi<zcount; zi++){
            for(int z=zspans[zi].start; z<zspans[zi].end; z++){
                int base = ybase + z * CW;
                for(int xi=0; xi<xcount; xi++){
                    memset(&world[base + xspans[xi].start], 0, (size_t)(xspans[xi].end - xspans[xi].start));
                }
            }
        }
    }
}

static int prepare_slice_desc(StreamRect rect, int line, bool x_major){
    int idx = 0;
    if(x_major){
        for(int z=rect.z0; z<rect.z1; z++){
            prepare_column_desc(global_x_from_local(line), global_z_from_local(z), &g_stream_desc[idx++]);
        }
    } else {
        for(int x=rect.x0; x<rect.x1; x++){
            prepare_column_desc(global_x_from_local(x), global_z_from_local(line), &g_stream_desc[idx++]);
        }
    }
    return idx;
}

static void apply_slice_base(StreamRect rect, int line, bool x_major, int count){
    int idx = 0;
    if(x_major){
        for(int z=rect.z0; z<rect.z1 && idx<count; z++) write_base_column(line, z, &g_stream_desc[idx++]);
    } else {
        for(int x=rect.x0; x<rect.x1 && idx<count; x++) write_base_column(x, line, &g_stream_desc[idx++]);
    }
}

static void apply_slice_light(StreamRect rect, int line, bool x_major, int count){
    int idx = 0;
    if(x_major){
        for(int z=rect.z0; z<rect.z1 && idx<count; z++) decorate_light_column_from_desc(line, z, &g_stream_desc[idx++]);
    } else {
        for(int x=rect.x0; x<rect.x1 && idx<count; x++) decorate_light_column_from_desc(x, line, &g_stream_desc[idx++]);
    }
}

static void apply_slice_features(StreamRect rect, int line, bool x_major, int count){
    int idx = 0;
    if(x_major){
        for(int z=rect.z0; z<rect.z1 && idx<count; z++) apply_overworld_column_features(line, z, &g_stream_desc[idx++]);
    } else {
        for(int x=rect.x0; x<rect.x1 && idx<count; x++) apply_overworld_column_features(x, line, &g_stream_desc[idx++]);
    }
}

static void apply_slice_heavy(StreamRect rect, int line, bool x_major, int count){
    int idx = 0;
    if(x_major){
        for(int z=rect.z0; z<rect.z1 && idx<count; z++) decorate_heavy_column_from_desc(line, z, &g_stream_desc[idx++]);
    } else {
        for(int x=rect.x0; x<rect.x1 && idx<count; x++) decorate_heavy_column_from_desc(x, line, &g_stream_desc[idx++]);
    }
}

static StreamRect stream_rect_padded(const StreamRect* r, int padding){
    StreamRect out = {
        r->x0 - padding,
        r->x1 + padding,
        r->z0 - padding,
        r->z1 + padding
    };
    if(out.x0 < 0) out.x0 = 0;
    if(out.z0 < 0) out.z0 = 0;
    if(out.x1 > CW) out.x1 = CW;
    if(out.z1 > CD) out.z1 = CD;
    return out;
}

static bool stream_process_lines(StreamRect rect, int* cursor, StreamPhase phase, u16 budget_ticks){
    bool x_major = ((rect.x1 - rect.x0) < CW);
    int start = x_major ? rect.x0 : rect.z0;
    int end = x_major ? rect.x1 : rect.z1;
    u16 t0 = REG_TM3CNT_L;
    if(*cursor < start) *cursor = start;
    while(*cursor < end){
        int count = prepare_slice_desc(rect, *cursor, x_major);
        if(phase == STREAM_BASE) apply_slice_base(rect, *cursor, x_major, count);
        else if(phase == STREAM_DECOR_LIGHT) apply_slice_light(rect, *cursor, x_major, count);
        else apply_slice_heavy(rect, *cursor, x_major, count);
        (*cursor)++;
        if((u16)(REG_TM3CNT_L - t0) >= budget_ticks) break;
    }
    return *cursor >= end;
}

static bool stream_process_feature_columns(StreamRect rect, int* line_cursor, int* col_cursor, u16 budget_ticks){
    bool x_major = ((rect.x1 - rect.x0) < CW);
    int line_start = x_major ? rect.x0 : rect.z0;
    int line_end = x_major ? rect.x1 : rect.z1;
    int col_start = x_major ? rect.z0 : rect.x0;
    int col_end = x_major ? rect.z1 : rect.x1;
    u16 t0 = REG_TM3CNT_L;

    if(*line_cursor < line_start){
        *line_cursor = line_start;
        *col_cursor = col_start;
    }

    while(*line_cursor < line_end){
        if(*col_cursor < col_start) *col_cursor = col_start;
        while(*col_cursor < col_end){
            int x = x_major ? *line_cursor : *col_cursor;
            int z = x_major ? *col_cursor : *line_cursor;
            ColumnGenDesc desc;
            prepare_column_desc(global_x_from_local(x), global_z_from_local(z), &desc);
            apply_overworld_column_features(x, z, &desc);
            (*col_cursor)++;
            if((u16)(REG_TM3CNT_L - t0) >= budget_ticks) return false;
        }
        (*line_cursor)++;
        *col_cursor = col_start;
    }

    return true;
}

static bool stream_feature_phase_step(void){
    while(g_stream_state.feature_rect_index < g_stream_state.rect_count){
        StreamRect rect = stream_rect_padded(&g_stream_state.rects[g_stream_state.feature_rect_index], STREAM_BASE_PADDING);
        if(stream_process_feature_columns(rect, &g_stream_state.feature_line_cursor, &g_stream_state.feature_col_cursor, STREAM_BUDGET_FEATURE_TICKS)){
            g_stream_state.feature_rect_index++;
            g_stream_state.feature_line_cursor = 0;
            g_stream_state.feature_col_cursor = 0;
        } else {
            return false;
        }
    }
    return true;
}

#if PERF_PROFILE
static void perf_gen_reset(void){
    g_prof_gen_total_ticks = 0;
    g_prof_gen_terrain_ticks = 0;
    g_prof_gen_detail_ticks = 0;
    g_prof_gen_light_ticks = 0;
    g_prof_gen_decor_ticks = 0;
    g_prof_gen_detail_max_line_ticks = 0;
    g_prof_gen_detail_columns = 0;
}
#endif

static void regenerate_region(int x0, int x1, int z0, int z1){
    if(x0 < 0) x0 = 0;
    if(z0 < 0) z0 = 0;
    if(x1 > CW) x1 = CW;
    if(z1 > CD) z1 = CD;
    if(x0 >= x1 || z0 >= z1) return;
    StreamRect rect = { x0, x1, z0, z1 };
    bool x_major = ((rect.x1 - rect.x0) < CW);
    int line_start = x_major ? rect.x0 : rect.z0;
    int line_end = x_major ? rect.x1 : rect.z1;
    for(int line=line_start; line<line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_base(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_terrain_ticks += prof_elapsed(pt);
#endif
    }
    for(int line=line_start; line<line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_features(rect, line, x_major, count);
#if PERF_PROFILE
        u32 dt = prof_elapsed(pt);
        g_prof_gen_detail_ticks += dt;
        g_prof_gen_detail_columns += (u32)count;
        prof_record_max(&g_prof_gen_detail_max_line_ticks, dt);
#endif
    }
    for(int line=line_start; line<line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_light(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_light_ticks += prof_elapsed(pt);
#endif
    }
    for(int line=line_start; line<line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_heavy(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_decor_ticks += prof_elapsed(pt);
#endif
    }
}

static void gen_world(void){
#if PERF_PROFILE
    perf_gen_reset();
#endif
    memset(world, 0, sizeof(world));
    regenerate_region(0, CW, 0, CD);
#if PERF_PROFILE
    g_prof_gen_total_ticks = g_prof_gen_terrain_ticks + g_prof_gen_detail_ticks +
                             g_prof_gen_light_ticks + g_prof_gen_decor_ticks;
    g_prof_gen_runs++;
    perf_debug_trap();
#endif
}

static void gen_world_with_progress_callback(int start_percent, int end_percent, void (*progress_cb)(int)){
    StreamRect rect = { 0, CW, 0, CD };
    bool x_major = ((rect.x1 - rect.x0) < CW);
    int line_start = x_major ? rect.x0 : rect.z0;
    int line_end = x_major ? rect.x1 : rect.z1;
    int total_lines = line_end - line_start;
    int total_steps = total_lines * 4;
    int completed_steps = 0;
    int span = end_percent - start_percent;

#if PERF_PROFILE
    perf_gen_reset();
#endif
    memset(world, 0, sizeof(world));
    if(progress_cb) progress_cb(start_percent);
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_base(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_terrain_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        if(progress_cb) progress_cb(start_percent + (completed_steps * span) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_features(rect, line, x_major, count);
#if PERF_PROFILE
        u32 dt = prof_elapsed(pt);
        g_prof_gen_detail_ticks += dt;
        g_prof_gen_detail_columns += (u32)count;
        prof_record_max(&g_prof_gen_detail_max_line_ticks, dt);
#endif
        completed_steps++;
        if(progress_cb) progress_cb(start_percent + (completed_steps * span) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_light(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_light_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        if(progress_cb) progress_cb(start_percent + (completed_steps * span) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_heavy(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_decor_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        if(progress_cb) progress_cb(start_percent + (completed_steps * span) / (total_steps ? total_steps : 1));
    }
#if PERF_PROFILE
    g_prof_gen_total_ticks = g_prof_gen_terrain_ticks + g_prof_gen_detail_ticks +
                             g_prof_gen_light_ticks + g_prof_gen_decor_ticks;
    g_prof_gen_runs++;
    perf_debug_trap();
#endif
}

static void gen_world_with_progress_screen(const char* title){
    ProgressScreenState screen;
    StreamRect rect = { 0, CW, 0, CD };
    bool x_major = ((rect.x1 - rect.x0) < CW);
    int line_start = x_major ? rect.x0 : rect.z0;
    int line_end = x_major ? rect.x1 : rect.z1;
    int total_lines = line_end - line_start;
    int total_steps = total_lines * 4;
    int completed_steps = 0;
    progress_screen_begin(&screen);
#if PERF_PROFILE
    perf_gen_reset();
#endif
    memset(world, 0, sizeof(world));
    draw_progress_screen(title, "TERRAIN", 0);
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_base(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_terrain_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        draw_progress_screen(title, "TERRAIN", (completed_steps * 100) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_features(rect, line, x_major, count);
#if PERF_PROFILE
        u32 dt = prof_elapsed(pt);
        g_prof_gen_detail_ticks += dt;
        g_prof_gen_detail_columns += (u32)count;
        prof_record_max(&g_prof_gen_detail_max_line_ticks, dt);
#endif
        completed_steps++;
        draw_progress_screen(title, "DETAIL", (completed_steps * 100) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_light(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_light_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        draw_progress_screen(title, "LIGHT", (completed_steps * 100) / (total_steps ? total_steps : 1));
    }
    for(int line = line_start; line < line_end; line++){
#if PERF_PROFILE
        u16 pt = prof_now();
#endif
        int count = prepare_slice_desc(rect, line, x_major);
        apply_slice_heavy(rect, line, x_major, count);
#if PERF_PROFILE
        g_prof_gen_decor_ticks += prof_elapsed(pt);
#endif
        completed_steps++;
        draw_progress_screen(title, "DECOR", (completed_steps * 100) / (total_steps ? total_steps : 1));
    }
#if PERF_PROFILE
    g_prof_gen_total_ticks = g_prof_gen_terrain_ticks + g_prof_gen_detail_ticks +
                             g_prof_gen_light_ticks + g_prof_gen_decor_ticks;
    g_prof_gen_runs++;
    perf_debug_trap();
#endif
    progress_screen_end(&screen);
}

static int clamp_world_offset_x(int off){
    if(off < 0) off = 0;
    if(off > VW - CW) off = VW - CW;
    return off;
}

static int clamp_world_offset_z(int off){
    if(off < 0) off = 0;
    if(off > VD - CD) off = VD - CD;
    return off;
}

static void stream_center_on_global(int gx, int gz){
    g_world_offset_x = clamp_world_offset_x(gx - (CW / 2));
    g_world_offset_z = clamp_world_offset_z(gz - (CD / 2));
}

static void stream_reset_state(void){
    memset(&g_stream_state, 0, sizeof(g_stream_state));
    g_stream_state.phase = STREAM_IDLE;
    g_world_ring_x = 0;
    g_world_ring_z = 0;
}

static void shift_world_window(int dx, int dz){
    g_world_ring_x = wrap_world_x(g_world_ring_x + dx);
    g_world_ring_z = wrap_world_z(g_world_ring_z + dz);
}

static void stream_add_rect(int x0, int x1, int z0, int z1){
    if(x0 < 0) x0 = 0;
    if(z0 < 0) z0 = 0;
    if(x1 > CW) x1 = CW;
    if(z1 > CD) z1 = CD;
    if(x0 >= x1 || z0 >= z1) return;
    if(g_stream_state.rect_count >= 2) return;
    g_stream_state.rects[g_stream_state.rect_count].x0 = x0;
    g_stream_state.rects[g_stream_state.rect_count].x1 = x1;
    g_stream_state.rects[g_stream_state.rect_count].z0 = z0;
    g_stream_state.rects[g_stream_state.rect_count].z1 = z1;
    g_stream_state.rect_count++;
}

static void stream_start(int new_off_x, int new_off_z){
    new_off_x = clamp_world_offset_x(new_off_x);
    new_off_z = clamp_world_offset_z(new_off_z);
    if(new_off_x == g_world_offset_x && new_off_z == g_world_offset_z) return;
    g_stream_state.active = true;
    g_stream_state.phase = STREAM_SHIFT;
    g_stream_state.old_off_x = g_world_offset_x;
    g_stream_state.old_off_z = g_world_offset_z;
    g_stream_state.new_off_x = new_off_x;
    g_stream_state.new_off_z = new_off_z;
    g_stream_state.prefetch_off_x = new_off_x;
    g_stream_state.prefetch_off_z = new_off_z;
    g_stream_state.shift_x = new_off_x - g_world_offset_x;
    g_stream_state.shift_z = new_off_z - g_world_offset_z;
    g_stream_state.rect_count = 0;
    g_stream_state.base_rect_index = 0;
    g_stream_state.base_cursor = 0;
    g_stream_state.feature_rect_index = 0;
    g_stream_state.feature_line_cursor = 0;
    g_stream_state.feature_col_cursor = 0;
    g_stream_state.light_rect_index = 0;
    g_stream_state.light_cursor = 0;
    g_stream_state.heavy_rect_index = 0;
    g_stream_state.heavy_cursor = 0;
    g_stream_state.edit_cursor = 0;
}

static void stream_queue_or_start(int new_off_x, int new_off_z){
    new_off_x = clamp_world_offset_x(new_off_x);
    new_off_z = clamp_world_offset_z(new_off_z);
    if(g_stream_state.active){
        g_stream_state.has_pending = true;
        g_stream_state.pending_shift_x = new_off_x;
        g_stream_state.pending_shift_z = new_off_z;
    } else {
        stream_start(new_off_x, new_off_z);
    }
}

static bool stream_rect_contains_global(const StreamRect* r, int gx, int gz){
    int lx = local_x_from_global(gx);
    int lz = local_z_from_global(gz);
    return lx >= r->x0 && lx < r->x1 && lz >= r->z0 && lz < r->z1;
}

static bool stream_phase_step(int* rect_index, int* cursor, StreamPhase phase, int padding, u16 budget_ticks){
    while(*rect_index < g_stream_state.rect_count){
        StreamRect rect = stream_rect_padded(&g_stream_state.rects[*rect_index], padding);
        if(phase == STREAM_DECOR_HEAVY && stream_rect_near_player(rect, STREAM_HEAVY_NEAR_DIST)){
            return false;
        }
        if(stream_process_lines(rect, cursor, phase, budget_ticks)){
            (*rect_index)++;
            *cursor = 0;
        } else {
            return false;
        }
    }
    return true;
}

static inline bool stream_load_shed_active(void){
    return g_stream_state.active && (g_stream_state.phase == STREAM_SHIFT ||
                                     g_stream_state.phase == STREAM_BASE ||
                                     g_stream_state.phase == STREAM_TERRAIN_FEATURES ||
                                     g_stream_state.phase == STREAM_DECOR_LIGHT);
}

static void stream_step(void){
    if(!g_stream_state.active){
        if(g_stream_state.has_pending){
            int pending_x = g_stream_state.pending_shift_x;
            int pending_z = g_stream_state.pending_shift_z;
            g_stream_state.has_pending = false;
            stream_start(pending_x, pending_z);
        }
        return;
    }

    if(g_stream_state.phase == STREAM_DECOR_HEAVY && g_stream_state.has_pending){
        g_stream_state.active = false;
        g_stream_state.phase = STREAM_IDLE;
        return;
    }

    switch(g_stream_state.phase){
        case STREAM_SHIFT: {
            int dx = g_stream_state.shift_x;
            int dz = g_stream_state.shift_z;
            g_world_offset_x = g_stream_state.new_off_x;
            g_world_offset_z = g_stream_state.new_off_z;

            if(abs(dx) >= CW || abs(dz) >= CD){
                g_world_ring_x = 0;
                g_world_ring_z = 0;
                gen_world();
                apply_dimension_edits(g_dimension);
                g_stream_state.phase = STREAM_IDLE;
                g_stream_state.active = false;
                break;
            }

            shift_world_window(dx, dz);
            if(dz > 0) stream_add_rect(0, CW, CD - dz, CD);
            else if(dz < 0) stream_add_rect(0, CW, 0, -dz);
            if(dx > 0) stream_add_rect(CW - dx, CW, 0, CD);
            else if(dx < 0) stream_add_rect(0, -dx, 0, CD);

            pl.pos.x -= I2F(dx);
            pl.pos.z -= I2F(dz);
            for(int i=0; i<MAX_DROPPED_ITEMS; i++){
                if(!g_dropped_items[i].active) continue;
                g_dropped_items[i].pos.x -= I2F(dx);
                g_dropped_items[i].pos.z -= I2F(dz);
                if(g_dropped_items[i].pos.x < 0 || g_dropped_items[i].pos.x >= I2F(CW) ||
                   g_dropped_items[i].pos.z < 0 || g_dropped_items[i].pos.z >= I2F(CD)){
                    g_dropped_items[i].active = false;
                }
            }
            for(int i=0; i<MAX_BREAK_PARTICLES; i++){
                if(!g_break_particles[i].active) continue;
                g_break_particles[i].pos.x -= I2F(dx);
                g_break_particles[i].pos.z -= I2F(dz);
                if(g_break_particles[i].pos.x < 0 || g_break_particles[i].pos.x >= I2F(CW) ||
                   g_break_particles[i].pos.z < 0 || g_break_particles[i].pos.z >= I2F(CD)){
                    g_break_particles[i].active = false;
                }
            }
            for(int i=0; i<MAX_TNT_ENTITIES; i++){
                if(!g_tnt_entities[i].active) continue;
                g_tnt_entities[i].x -= (s8)dx;
                g_tnt_entities[i].z -= (s8)dz;
                if(g_tnt_entities[i].x < 0 || g_tnt_entities[i].x >= CW ||
                   g_tnt_entities[i].z < 0 || g_tnt_entities[i].z >= CD){
                    g_tnt_entities[i].active = false;
                }
            }
            if(g_mining_x >= 0){
                g_mining_x -= dx;
                g_mining_z -= dz;
                if(g_mining_x < 0 || g_mining_x >= CW || g_mining_z < 0 || g_mining_z >= CD){
                    g_mining_x = g_mining_y = g_mining_z = -1;
                    g_mining_ticks = 0;
                }
            }
            for(int i=0; i<g_stream_state.rect_count; i++) clear_strip_region(&g_stream_state.rects[i]);
            g_stream_state.phase = STREAM_BASE;
            g_stream_state.base_rect_index = 0;
            g_stream_state.base_cursor = 0;
            g_stream_state.feature_rect_index = 0;
            g_stream_state.feature_line_cursor = 0;
            g_stream_state.feature_col_cursor = 0;
            g_stream_state.light_rect_index = 0;
            g_stream_state.light_cursor = 0;
            g_stream_state.heavy_rect_index = 0;
            g_stream_state.heavy_cursor = 0;
        } break;

        case STREAM_BASE:
            if(stream_phase_step(&g_stream_state.base_rect_index, &g_stream_state.base_cursor, STREAM_BASE, STREAM_BASE_PADDING, STREAM_BUDGET_BASE_TICKS)){
                g_stream_state.phase = STREAM_TERRAIN_FEATURES;
            }
            break;

        case STREAM_TERRAIN_FEATURES:
            if(stream_feature_phase_step()){
                g_stream_state.phase = STREAM_DECOR_LIGHT;
            }
            break;

        case STREAM_DECOR_LIGHT:
            if(stream_phase_step(&g_stream_state.light_rect_index, &g_stream_state.light_cursor, STREAM_DECOR_LIGHT, STREAM_DECOR_PADDING, STREAM_BUDGET_LIGHT_TICKS)){
                g_stream_state.phase = STREAM_EDITS;
                g_stream_state.edit_cursor = 0;
            }
            break;

        case STREAM_EDITS: {
            u16 count = g_edit_count[g_dimension];
            u16 t0 = REG_TM3CNT_L;
            if(count > MAX_EDITS) count = MAX_EDITS;
            while(g_stream_state.edit_cursor < count){
                int i = g_stream_state.edit_cursor++;
                int gx = edit_x(g_edit_log[g_dimension][i]);
                int gz = edit_z(g_edit_log[g_dimension][i]);
                int y = edit_y(g_edit_log[g_dimension][i]);
                u8 val = edit_val(g_edit_log[g_dimension][i]);
                for(int r=0; r<g_stream_state.rect_count; r++){
                    StreamRect rect = stream_rect_padded(&g_stream_state.rects[r], STREAM_DECOR_PADDING);
                    if(stream_rect_contains_global(&rect, gx, gz)){
                        int lx = local_x_from_global(gx);
                        int lz = local_z_from_global(gz);
                        if(lx >= 0 && lx < CW && lz >= 0 && lz < CD && y >= 0 && y < CH){
                            world[widx(lx,y,lz)] = val;
                        }
                        break;
                    }
                }
                if((u16)(REG_TM3CNT_L - t0) >= STREAM_BUDGET_EDIT_TICKS) break;
            }
            if(g_stream_state.edit_cursor >= count){
                g_stream_state.phase = STREAM_DECOR_HEAVY;
            }
        } break;

        case STREAM_DECOR_HEAVY:
            if(stream_phase_step(&g_stream_state.heavy_rect_index, &g_stream_state.heavy_cursor, STREAM_DECOR_HEAVY, STREAM_DECOR_PADDING, STREAM_BUDGET_HEAVY_TICKS)){
                g_stream_state.phase = STREAM_IDLE;
                g_stream_state.active = false;
            }
            break;

        case STREAM_IDLE:
            g_stream_state.active = false;
            break;
    }
}

static void stream_maybe_recenter(void){
    int local_x = F2I(pl.pos.x);
    int local_z = F2I(pl.pos.z);
    int new_off_x = g_world_offset_x;
    int new_off_z = g_world_offset_z;
    int right_trigger = CW - 1 - STREAM_TRIGGER_MARGIN;
    int right_settle = CW - 1 - STREAM_SETTLE_POS;
    int right_prefetch = CW - 1 - STREAM_PREFETCH_MARGIN;
    int bottom_trigger = CD - 1 - STREAM_TRIGGER_MARGIN;
    int bottom_settle = CD - 1 - STREAM_SETTLE_POS;
    int bottom_prefetch = CD - 1 - STREAM_PREFETCH_MARGIN;

    if(local_x < STREAM_TRIGGER_MARGIN && g_world_offset_x > 0) new_off_x -= (STREAM_SETTLE_POS - local_x);
    else if(local_x < STREAM_PREFETCH_MARGIN && pl.vel.x < 0 && g_world_offset_x > 0) new_off_x -= (STREAM_PREFETCH_MARGIN - local_x);
    else if(local_x > right_trigger && g_world_offset_x < (VW - CW)) new_off_x += (local_x - right_settle);
    else if(local_x > right_prefetch && pl.vel.x > 0 && g_world_offset_x < (VW - CW)) new_off_x += (local_x - right_prefetch);

    if(local_z < STREAM_TRIGGER_MARGIN && g_world_offset_z > 0) new_off_z -= (STREAM_SETTLE_POS - local_z);
    else if(local_z < STREAM_PREFETCH_MARGIN && pl.vel.z < 0 && g_world_offset_z > 0) new_off_z -= (STREAM_PREFETCH_MARGIN - local_z);
    else if(local_z > bottom_trigger && g_world_offset_z < (VD - CD)) new_off_z += (local_z - bottom_settle);
    else if(local_z > bottom_prefetch && pl.vel.z > 0 && g_world_offset_z < (VD - CD)) new_off_z += (local_z - bottom_prefetch);

    if(new_off_x < g_world_offset_x - STREAM_MAX_SHIFT) new_off_x = g_world_offset_x - STREAM_MAX_SHIFT;
    if(new_off_x > g_world_offset_x + STREAM_MAX_SHIFT) new_off_x = g_world_offset_x + STREAM_MAX_SHIFT;
    if(new_off_z < g_world_offset_z - STREAM_MAX_SHIFT) new_off_z = g_world_offset_z - STREAM_MAX_SHIFT;
    if(new_off_z > g_world_offset_z + STREAM_MAX_SHIFT) new_off_z = g_world_offset_z + STREAM_MAX_SHIFT;
    if(new_off_x != g_world_offset_x || new_off_z != g_world_offset_z){
        stream_queue_or_start(new_off_x, new_off_z);
    }
}

