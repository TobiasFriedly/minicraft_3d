static IWRAM_CODE ARM_CODE void render_textured(u8 selected_block, RayHit hit){
#if PERF_PROFILE
    u16 prof_render_start = prof_now();
    u16 prof_section_start = prof_render_start;
#endif
    int saved_face_limit = g_face_limit;
    if(g_vis_dist < 2) g_vis_dist = 2;
    if(g_vis_dist > 20) g_vis_dist = 20;
    if(g_face_limit < 200) g_face_limit = 200;
    if(g_face_limit > MAX_FACES) g_face_limit = MAX_FACES;
    if(g_render_scale < 1) g_render_scale = 1;
    if(g_render_scale > 4) g_render_scale = 4;
    if(stream_load_shed_active() && g_face_limit > 1200) g_face_limit = 1200;
    

    update_render_dims();
    apply_hardware_scale(g_render_scale);
    
    draw_sky_gradient();
#if PERF_PROFILE
    g_prof_render_sky_ticks = prof_elapsed(prof_section_start);
    prof_section_start = prof_now();
#endif
    v3 fx, fy, fz;
    if(!cam_basis(&fx, &fy, &fz)) {
        g_face_limit = saved_face_limit;
        return;
    }
    v3 cam_pos = { pl.pos.x, pl.pos.y + F(1.5f), pl.pos.z };
    fix focal = scaled_focal();
    
    facePoolIdx = 0;
    {
        u32 fill = 0xFFFFFFFFu;
        CpuFastSet(&fill, faceBuckets, (BUCKET_COUNT/2) | COPY32 | FILL);
    }
    
    int cx = F2I(pl.pos.x), cy = F2I(pl.pos.y + F(0.5f)), cz = F2I(pl.pos.z);
    int xmin = cx - g_vis_dist; if(xmin < 0) xmin = 0;
    int xmax = cx + g_vis_dist; if(xmax >= CW) xmax = CW-1;
    int ymin = cy - g_vis_dist; if(ymin < 0) ymin = 0;
    int ymax = cy + g_vis_dist; if(ymax >= CH) ymax = CH-1;
    int zmin = cz - g_vis_dist; if(zmin < 0) zmin = 0;
    int zmax = cz + g_vis_dist; if(zmax >= CD) zmax = CD-1;

    const int r = g_vis_dist;
    const int r2 = r * r;
    const u8 portal = (u8)(BLK_PORTAL + 1);
    const bool hide_water = g_in_water;
    const fix half = F(0.5f);
    const fix eps = F(0.01f);
    const fix half_fz_x = fz.x >> 1;
    const fix half_fz_y = fz.y >> 1;
    const fix half_fz_z = fz.z >> 1;
    const fix inc_depth_x = fz.x;
    const int frustum_margin_q8 = (int)(g_frustum_margin >> 8);

    fix ry = (I2F(ymin) + half) - cam_pos.y;
    for(int y=ymin; y<=ymax && facePoolIdx < g_face_limit; y++, ry += FONE){
        int dy = y - cy;
        int dy2 = dy * dy;
        fix y_fz = fmuli(ry, fz.y);
        fix y_fx = fmuli(ry, fx.y);
        fix y_fy = fmuli(ry, fy.y);
        bool front_posy = (ry + half) < 0;
        bool front_negy = (-ry + half) < 0;

        fix rz = (I2F(zmin) + half) - cam_pos.z;
	        for(int z=zmin; z<=zmax && facePoolIdx < g_face_limit; z++, rz += FONE){
	            int dz = z - cz;
	            int rem = r2 - dy2 - dz*dz;
	            if(rem < 0) continue;
	            fix z_fz = fmuli(rz, fz.z);
	            fix z_fx = fmuli(rz, fx.z);
	            fix z_fy = fmuli(rz, fy.z);
	            bool front_posz = (rz + half) < 0;
	            bool front_negz = (-rz + half) < 0;

            fix base_depth_yz = y_fz + z_fz;
            fix base_latx_yz  = y_fx + z_fx;
            fix base_laty_yz  = y_fy + z_fy;

	            int dxm = dxmax_lut[rem];
	            int xs = cx - dxm; if(xs < xmin) xs = xmin;
	            int xe = cx + dxm; if(xe > xmax) xe = xmax;

	            fix rx = (I2F(xs) + half) - cam_pos.x;
	            fix depth = base_depth_yz + fmuli(rx, fz.x);
	            {
	                fix depth_end = depth + inc_depth_x * (xe - xs);
	                fix depth_min = (depth < depth_end) ? depth : depth_end;
	                fix depth_max = (depth > depth_end) ? depth : depth_end;
	                if(depth_max <= eps || depth_min > FAR_Z) continue;
	            }
	            int phys_z = phys_z_from_local(z);
	            const u8* row = world_row_ptr_phys_const(y, phys_z);
	            const u8* row_posy = (y < (CH - 1)) ? world_row_ptr_phys_const(y + 1, phys_z) : NULL;
	            const u8* row_negy = (y > 0) ? world_row_ptr_phys_const(y - 1, phys_z) : NULL;
	            const u8* row_posz = (z < (CD - 1)) ? world_row_ptr_phys_const(y, wrap_world_z(phys_z + 1)) : NULL;
	            const u8* row_negz = (z > 0) ? world_row_ptr_phys_const(y, wrap_world_z(phys_z - 1)) : NULL;
	            int x = xs;
	            int phys_x = phys_x_from_local(xs);
            int remaining = xe - xs + 1;
            while(remaining > 0 && facePoolIdx < g_face_limit){
                int chunk = CW - phys_x;
                if(chunk > remaining) chunk = remaining;
	                for(int i = 0; i < chunk && facePoolIdx < g_face_limit; i++, x++, rx += FONE, depth += inc_depth_x){
                    int px = phys_x + i;
	                    u8 id = row[px];
	                    if(!id) continue;
	                    if(depth <= eps || depth > FAR_Z) continue;
	                    u8 blk = (u8)(id - 1);
	                    bool front_posx = (rx + half) < 0;
	                    bool front_negx = (-rx + half) < 0;
	                    u8 emit_mask = 0;

	                    if(hide_water && blk == BLK_WATER) {
	                        if(front_posy) {
	                            if(y == (CH-1)) emit_mask |= (u8)(1u << FACE_POSY);
	                            else {
	                                u8 n = row_posy[px];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_POSY);
	                            }
	                        }
	                    } else {
	                        if(front_posx) {
	                            if(x == (CW-1)) emit_mask |= (u8)(1u << FACE_POSX);
	                            else {
	                                u8 n = row[(px + 1 < CW) ? (px + 1) : 0];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_POSX);
	                            }
	                        }
	                        if(front_negx) {
	                            if(x == 0) emit_mask |= (u8)(1u << FACE_NEGX);
	                            else {
	                                u8 n = row[(px > 0) ? (px - 1) : (CW - 1)];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_NEGX);
	                            }
	                        }
	                        if(front_posy) {
	                            if(y == (CH-1)) emit_mask |= (u8)(1u << FACE_POSY);
	                            else {
	                                u8 n = row_posy[px];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_POSY);
	                            }
	                        }
	                        if(front_negy) {
	                            if(y == 0) emit_mask |= (u8)(1u << FACE_NEGY);
	                            else {
	                                u8 n = row_negy[px];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_NEGY);
	                            }
	                        }
	                        if(front_posz) {
	                            if(z == (CD-1)) emit_mask |= (u8)(1u << FACE_POSZ);
	                            else {
	                                u8 n = row_posz[px];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_POSZ);
	                            }
	                        }
	                        if(front_negz) {
	                            if(z == 0) emit_mask |= (u8)(1u << FACE_NEGZ);
	                            else {
	                                u8 n = row_negz[px];
	                                if(n == 0 || n == portal) emit_mask |= (u8)(1u << FACE_NEGZ);
	                            }
	                        }
	                    }
	                    if(!emit_mask) continue;

	                    fix depth_c = (depth < g_near_z) ? g_near_z : depth;
	                    fix latx = base_latx_yz + fmuli(rx, fx.x);
	                    fix laty = base_laty_yz + fmuli(rx, fy.x);
	                    fix half_dim = (depth_c >> 1) + (depth_c >> 3);
	                    fix threshold = (fix)((half_dim >> 8) * frustum_margin_q8) + F(0.8f);
	                    if(fixabs(latx) > threshold || fixabs(laty) > threshold) continue;

	                    if(emit_mask & (1u << FACE_POSX)) emit_face_bucketed_fast(x, y, z, blk, FACE_POSX, depth + half_fz_x);
	                    if(emit_mask & (1u << FACE_NEGX)) emit_face_bucketed_fast(x, y, z, blk, FACE_NEGX, depth - half_fz_x);
	                    if(emit_mask & (1u << FACE_POSY)) emit_face_bucketed_fast(x, y, z, blk, FACE_POSY, depth + half_fz_y);
	                    if(emit_mask & (1u << FACE_NEGY)) emit_face_bucketed_fast(x, y, z, blk, FACE_NEGY, depth - half_fz_y);
	                    if(emit_mask & (1u << FACE_POSZ)) emit_face_bucketed_fast(x, y, z, blk, FACE_POSZ, depth + half_fz_z);
	                    if(emit_mask & (1u << FACE_NEGZ)) emit_face_bucketed_fast(x, y, z, blk, FACE_NEGZ, depth - half_fz_z);
	                }
                remaining -= chunk;
                phys_x = 0;
            }
        }
    }

#if PERF_PROFILE
    g_prof_render_collect_ticks = prof_elapsed(prof_section_start);
    prof_section_start = prof_now();
#endif
    
    int draw_bucket_start = (g_vis_dist + 2) * 12;
    if(draw_bucket_start >= BUCKET_COUNT) draw_bucket_start = BUCKET_COUNT - 1;
    for(int i=draw_bucket_start; i>=0; i--) {
        u16 fi = faceBuckets[i];
        while(fi != FACE_NONE) {
            Face* f = &facePool[fi];
            draw_block_face_textured(f, &fx, &fy, &fz, &cam_pos, focal);
            fi = f->next;
        }
    }

#if PERF_PROFILE
    g_prof_render_draw_ticks = prof_elapsed(prof_section_start);
    prof_section_start = prof_now();
#endif

    render_break_particles(&fx, &fy, &fz, &cam_pos, focal);
    render_dropped_items(&fx, &fy, &fz, &cam_pos, focal);

    if(g_game_mode == MODE_SURVIVAL && hit.hit &&
       hit.x == g_mining_x && hit.y == g_mining_y && hit.z == g_mining_z &&
       g_mining_ticks > 0){
        u8 held_item = selected_block;
        u8 blk = world[widx(hit.x, hit.y, hit.z)] ? (u8)(world[widx(hit.x, hit.y, hit.z)] - 1) : 0;
        int total = survival_break_ticks(blk, held_item);
        int stage = (g_mining_ticks * 5) / (total ? total : 1);
        if(stage > 4) stage = 4;
        draw_block_break_overlay_all_faces(hit.x, hit.y, hit.z, stage, &fx, &fy, &fz, &cam_pos, focal);
    }
    
    if(g_show_hud && g_show_contour) draw_selection_outline(hit, &fx, &fy, &fz, &cam_pos);
    g_face_limit = saved_face_limit;

#if PERF_PROFILE
    g_prof_render_post_ticks = prof_elapsed(prof_section_start);
    prof_section_start = prof_now();
#endif
    
   
    if(g_show_hud) {
        if(g_burn_timer > 0) draw_burn_overlay();
        if(g_show_hand && selected_block){
            if(item_is_block_item(selected_block)){
                draw_hand_block((u8)(selected_block - 1), g_hand_anim_timer);
            } else {
                draw_hand_item_held(selected_block, g_hand_anim_timer);
            }
        }
        u8 cross_idx = 253;
        int cxp = g_rw/2, cyp = g_rh/2;
        int cross_scale = (g_render_scale > 1) ? 1 : 2; 
        int cross_len = cross_scale*4 + 1;
        fill_rect(backbuffer, cxp - cross_scale*2, cyp, cross_len, 1, cross_idx);
        fill_rect(backbuffer, cxp, cyp - cross_scale*2, 1, cross_len, cross_idx);

        if(g_show_hotbar) draw_block_bar(g_hotbar_slot);
        if(g_game_mode == MODE_SURVIVAL){
            draw_survival_status_icons();
        }
        render_toast();
    }

    if(g_show_fps) {
        fill_rect(backbuffer, 2, 2, 48/g_render_scale, 12/g_render_scale, 253);
        draw_text3x5(backbuffer, 4/g_render_scale, 4/g_render_scale, g_fps_text, 1, 255);
    }
#if PERF_PROFILE
    g_prof_render_hud_ticks = prof_elapsed(prof_section_start);
    g_prof_render_ticks = prof_elapsed(prof_render_start);
#if PERF_OVERLAY
    perf_draw_overlay();
#endif
#endif
}

