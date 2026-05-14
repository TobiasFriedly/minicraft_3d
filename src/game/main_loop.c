#if PERF_PROFILE && PERF_HALT_AFTER_WORLDGEN
static void perf_worldgen_hold_screen(void){
    while(1){
        scanKeys();
        if(keysDown() & KEY_START) break;
        memset_page8(backbuffer, 0);
        draw_text3x5(backbuffer, 8, 10, "WORLDGEN PROFILE", 2, 255);
        char buf[32];
        snprintf(buf, sizeof(buf), "TOTAL:%lu", (unsigned long)g_prof_gen_total_ticks);
        draw_text3x5(backbuffer, 8, 34, buf, 1, 255);
        snprintf(buf, sizeof(buf), "TERR:%lu", (unsigned long)g_prof_gen_terrain_ticks);
        draw_text3x5(backbuffer, 8, 44, buf, 1, 255);
        snprintf(buf, sizeof(buf), "DETAIL:%lu", (unsigned long)g_prof_gen_detail_ticks);
        draw_text3x5(backbuffer, 8, 54, buf, 1, 255);
        snprintf(buf, sizeof(buf), "LIGHT:%lu", (unsigned long)g_prof_gen_light_ticks);
        draw_text3x5(backbuffer, 8, 64, buf, 1, 255);
        snprintf(buf, sizeof(buf), "DECOR:%lu", (unsigned long)g_prof_gen_decor_ticks);
        draw_text3x5(backbuffer, 8, 74, buf, 1, 255);
        snprintf(buf, sizeof(buf), "MAX DETAIL:%lu", (unsigned long)g_prof_gen_detail_max_line_ticks);
        draw_text3x5(backbuffer, 8, 84, buf, 1, 255);
        draw_text3x5(backbuffer, 8, 104, "START CONTINUE", 1, 254);
        present_and_flip();
    }
}
#endif

static ARM_CODE void mainloop(MenuResult menu_res){
    u8 curBlk = BLK_GRASS + 1;
    bool loaded_from_slot = false;
    clear_dropped_items();
    g_creative_flying = false;
    g_spectator_mode = false;
    g_spectator_saved_hud_valid = false;
    g_jump_tap_timer = 0;
    stream_reset_state();
    if(menu_res.start_from_load){
        if(load_world_from_slot(menu_res.load_slot, &curBlk)) loaded_from_slot = true;
        else {
            stream_reset_state();
            stream_center_on_global(VW/2, VD/2);
            gen_world();
            editlog_clear_all();
        }
    } else {
        g_game_mode = menu_res.game_mode;
        g_allow_mode_switch = menu_res.allow_mode_switch;
        g_dimension = 0;
        editlog_clear_all();
        memset(g_dim_state, 0, sizeof(g_dim_state));
        stream_reset_state();
        stream_center_on_global(VW/2, VD/2);
        gen_world_with_progress_screen("GENERATING WORLD");
#if PERF_PROFILE && PERF_HALT_AFTER_WORLDGEN
        perf_worldgen_hold_screen();
#endif
    }
    init_quickbar();
    if(g_game_mode == MODE_CREATIVE){
        g_quickbar[0] = curBlk;
        g_hotbar_slot = 0;
        g_hotbar_prev_slot = 0;
    } else if(!loaded_from_slot){
        survival_init_inventory();
        survival_reset_stats();
        curBlk = ITEM_NONE;
    }

    if(!loaded_from_slot){
        int spawn_x = CW/2, spawn_z = CD/2, spawn_y = CH - 1;
        if(g_world_type == WORLD_SUPERFLAT){ spawn_y = 11; } 
        else if(!find_overworld_spawn(&spawn_x, &spawn_y, &spawn_z)) {
            for(int y = CH-1; y >= 0; y--){
                u8 block_id = world[widx(spawn_x, y, spawn_z)];
                if(block_id){
                    u8 t = block_id - 1;
                    if(t != BLK_WATER && t != BLK_LEAF && t != BLK_LEAF_DARK && t != BLK_LEAF_LIGHT && t != BLK_LAVA){ spawn_y = y + 1; break; }
                }
            }
            if(spawn_y < 10) spawn_y = 25;
        }
        pl.pos.x = I2F(spawn_x); pl.pos.y = I2F(spawn_y); pl.pos.z = I2F(spawn_z);
        pl.yaw = LUT_N/2; pl.pitch = 0; pl.vel.x = pl.vel.y = pl.vel.z = 0; pl.onGround = false;
        store_current_dimension_state();
    }

    u32 frame = 0; u8 action_cooldown = 0;
    u8 frameskip_pending = 0;
    backbuffer = vram_backbuffer();
    
    // Ensure hardware state is set for game
    update_render_dims();
    
    while(1){
        u16 frame_work_start = frame_timer_now();
        bool render_this_frame = (frameskip_pending == 0);
        if(!render_this_frame) frameskip_pending--;
#if PERF_PROFILE
        u16 prof_frame_start = frame_work_start;
        u16 prof_logic_start = prof_frame_start;
#endif
        g_frame = frame;
        u32 vb = g_vblank_count;
        u32 dvb = vb - g_fps_last_vblank;
        if(dvb >= 60) {
            int fps = (int)((g_fps_frame_count * 60) / (dvb ? dvb : 1));
            fps_set(fps);
            g_fps_frame_count = 0;
            g_fps_last_vblank = vb;
        }
        if(g_hand_anim_timer > 0) g_hand_anim_timer--;
        if(g_jump_tap_timer > 0) g_jump_tap_timer--;
        if(g_health_flash_timer > 0) g_health_flash_timer--;
        if(g_air_flash_timer > 0) g_air_flash_timer--;
        if(g_burn_timer > 0) g_burn_timer--;

        scanKeys();
        u16 raw_held = keysHeld();
        input_update_actions(raw_held);

        if(g_action_state[ACT_SPRINT].pressed) {
            g_is_sprinting = !g_is_sprinting;
            show_toast(g_is_sprinting ? "SPRINT: ON" : "SPRINT: OFF");
        }

        if(g_game_mode == MODE_CREATIVE && !g_spectator_mode && g_action_state[ACT_JUMP_FLY_UP].pressed){
            if(g_creative_flying){
                if(g_jump_tap_timer > 0){
                    set_creative_flying(false);
                    g_jump_tap_timer = 0;
                } else {
                    g_jump_tap_timer = 14;
                }
            } else {
                if(!pl.onGround && g_jump_tap_timer > 0){
                    set_creative_flying(true);
                    g_jump_tap_timer = 0;
                } else {
                    g_jump_tap_timer = 14;
                }
            }
        }

        if(g_action_state[ACT_PAUSE].pressed){
            frameskip_pending = 0;
            PauseResult pr = show_pause_menu(&curBlk);
            // Ensure hardware state is restored after pause
            update_render_dims();
            input_consume_current_buttons();
            
            if(pr == PM_TO_MAINMENU){
                return_to_main_menu(&curBlk, true);
            }
            input_consume_current_buttons();
            frame++; continue;
        }

        if(action_cooldown) action_cooldown--;
        handle_input();
        
        if(g_action_state[ACT_RESET_GRAPHICS].pressed && !action_cooldown &&
           !(g_game_mode == MODE_CREATIVE && (g_creative_flying || g_spectator_mode))) {
            g_vis_dist=9; g_fov_val=120; g_focal=F(130.0f); g_frustum_margin=F(1.2f);
            g_show_hud=true; g_show_contour=true;
            g_show_hand=true; g_show_hotbar=true;
            g_render_scale=2; g_face_limit=MAX_FACES; g_sky_mode=0; g_show_fps=false;
            g_falling_blocks = false; g_auto_frameskip = false;
            g_use_16x16_textures = true; g_texture_pack_enabled = true;
            generate_textures();
            show_toast("SETTINGS RESET"); action_cooldown = 20;
            update_render_dims();
        }

        bool needs_target_hit = render_this_frame ||
            g_action_state[ACT_BREAK].held ||
            g_action_state[ACT_PLACE_USE].held;
        RayHit hit = needs_target_hit ? get_target_block() : (RayHit){false,0,0,0,0};

        if(g_action_state[ACT_INVENTORY].pressed && action_cooldown == 0){
            frameskip_pending = 0;
            if(g_game_mode == MODE_CREATIVE) show_block_inventory(&curBlk);
            else show_survival_inventory(&curBlk);
            input_consume_current_buttons();
            action_cooldown = 12;
        }

        if(action_cooldown == 0 && (g_action_state[ACT_HOTBAR_LEFT].pressed || g_action_state[ACT_HOTBAR_RIGHT].pressed)){
            int delta = g_action_state[ACT_HOTBAR_LEFT].pressed ? -1 : 1;
            int slots = (g_game_mode == MODE_CREATIVE) ? HOTBAR_SLOTS : HOTBAR_SLOTS;
            g_hotbar_slot = (g_hotbar_slot + slots + delta) % slots;
            if(g_game_mode == MODE_CREATIVE){
                if(g_hotbar_slot == QUICKBAR_SLOTS) show_toast("ALL BLOCKS");
                else {
                    curBlk = g_quickbar[g_hotbar_slot];
                    show_toast(item_name(curBlk));
                }
            } else {
                refresh_selected_item(&curBlk);
                show_toast(curBlk ? item_name(curBlk) : "EMPTY");
            }
            action_cooldown = 10;
        }

        if(g_game_mode == MODE_SURVIVAL && (!g_action_state[ACT_BREAK].held || !hit.hit ||
           hit.x != g_mining_x || hit.y != g_mining_y || hit.z != g_mining_z)){
            g_mining_ticks = 0;
            if(!g_action_state[ACT_BREAK].held || !hit.hit){
                g_mining_x = g_mining_y = g_mining_z = -1;
            }
        }

        if(!g_spectator_mode && g_action_state[ACT_BREAK].held && action_cooldown == 0){
            if(g_game_mode == MODE_CREATIVE){
                if(g_hotbar_slot == QUICKBAR_SLOTS){
                    show_block_inventory(&curBlk);
                    input_consume_current_buttons();
                    action_cooldown = 12;
                } else if(hit.hit) {
                    if (curBlk == ITEM_FLINT_STEEL) use_flint_and_steel_at(hit.x, hit.y, hit.z);
                    else {
                        u8 blk = (u8)(world[widx(hit.x, hit.y, hit.z)] - 1);
                        spawn_block_break_particles(hit.x, hit.y, hit.z, blk);
                        world_set_and_log(hit.x, hit.y, hit.z, 0);
                    }
                    action_cooldown = 8; g_hand_anim_timer = 15;
                }
            } else if(hit.hit) {
                u8 blk = world[widx(hit.x, hit.y, hit.z)] - 1;
                if(!survival_can_break(blk, curBlk)){
                    action_cooldown = 8;
                    g_mining_ticks = 0;
                    continue;
                }
                if(g_mining_x != hit.x || g_mining_y != hit.y || g_mining_z != hit.z){
                    g_mining_x = hit.x; g_mining_y = hit.y; g_mining_z = hit.z; g_mining_ticks = 0;
                }
                g_mining_ticks++;
                if(g_mining_ticks >= survival_break_ticks(blk, curBlk)){
                    if(survival_can_harvest(blk, curBlk)){
                        u8 drop = survival_drop_item(blk);
                        if(drop) spawn_dropped_item(hit.x, hit.y, hit.z, drop);
                    }
                    spawn_block_break_particles(hit.x, hit.y, hit.z, blk);
                    world_set_and_log(hit.x, hit.y, hit.z, 0);
                    g_mining_ticks = 0;
                    g_hand_anim_timer = 15;
                    action_cooldown = 6;
                }
            }
        }

        if(!g_spectator_mode && g_action_state[ACT_PLACE_USE].held && action_cooldown == 0){
            if(g_game_mode == MODE_CREATIVE && g_hotbar_slot == QUICKBAR_SLOTS){
                show_block_inventory(&curBlk);
                input_consume_current_buttons();
                action_cooldown = 12;
            } else if (hit.hit) {
                int px=hit.x, py=hit.y, pz=hit.z;
                switch(hit.face) {
                    case FACE_POSX: px++; break; case FACE_NEGX: px--; break;
                    case FACE_POSY: py++; break; case FACE_NEGY: py--; break;
                    case FACE_POSZ: pz++; break; case FACE_NEGZ: pz--; break;
                }
                if(px>=0 && px<CW && py>=0 && py<CH && pz>=0 && pz<CD) {
                    int pl_x = F2I(pl.pos.x), pl_y = F2I(pl.pos.y), pl_z = F2I(pl.pos.z);
                    if(!((px==pl_x && pz==pl_z) && (py==pl_y || py==pl_y+1))) {
                        if(g_game_mode == MODE_CREATIVE){
                            if(curBlk == ITEM_FLINT_STEEL) use_flint_and_steel_at(hit.x, hit.y, hit.z);
                            else if(item_is_placeable(curBlk)) world_set_and_log(px,py,pz, curBlk);
                            action_cooldown = 8; g_hand_anim_timer = 15;
                        } else if(curBlk == ITEM_FLINT_STEEL){
                            use_flint_and_steel_at(hit.x, hit.y, hit.z);
                            action_cooldown = 8; g_hand_anim_timer = 15;
                        } else if(item_is_placeable(curBlk) && g_inventory[g_hotbar_slot].count){
                            world_set_and_log(px,py,pz, curBlk);
                            if(g_inventory[g_hotbar_slot].count > 0) g_inventory[g_hotbar_slot].count--;
                            if(g_inventory[g_hotbar_slot].count == 0){
                                g_inventory[g_hotbar_slot].item = ITEM_NONE;
                                inventory_compact();
                            }
                            refresh_selected_item(&curBlk);
                            action_cooldown = 8; g_hand_anim_timer = 15;
                        }
                    }
                }
            }
        }

        update_falling_blocks();
        update_tnt_entities();
        physics_step(curBlk);
        update_break_particles();
        stream_maybe_recenter();
        stream_step();
        if(g_game_mode == MODE_SURVIVAL && g_player_health == 0){
            frameskip_pending = 0;
            DeathMenuChoice death_choice = show_death_screen(curBlk, hit);
            if(death_choice == DEATH_MAIN_MENU){
                return_to_main_menu(&curBlk, false);
            } else {
                respawn_player(RESPAWN_VISUAL_RESPAWN);
                input_consume_current_buttons();
            }
            frame++;
            continue;
        }
        update_dropped_items(&curBlk);
        store_current_dimension_state();
#if PERF_PROFILE
        g_prof_logic_ticks = prof_elapsed(prof_logic_start);
#endif
        if(render_this_frame) {
            render_textured(curBlk, hit);
#if PERF_PROFILE
            {
                u32 ticks = prof_elapsed(prof_frame_start);
                g_prof_frame_work_ticks = ticks;
                g_prof_frame_work_cycles = ticks * PROF_CYCLES_PER_TICK;
                frameskip_pending = auto_frameskip_for_work_ticks(ticks);
                g_prof_frameskip_pending = frameskip_pending;
            }
#else
            frameskip_pending = auto_frameskip_for_work_ticks(frame_timer_elapsed(frame_work_start));
#endif
            present_and_flip();
            g_fps_frame_count++;
        } else {
            u32 logic_only_ticks = frame_timer_elapsed(frame_work_start);
            g_frameskip_logic_est_ticks = ((g_frameskip_logic_est_ticks * 3u) + logic_only_ticks + 2u) >> 2;
#if PERF_PROFILE
            g_prof_frameskip_logic_ticks = logic_only_ticks;
            g_prof_frameskip_pending = frameskip_pending;
#endif
        }
        frame++;
    }
}

int main(void) {
    irqInit();
    irqSet(IRQ_VBLANK, on_vblank);
    irqEnable(IRQ_VBLANK);
    REG_TM3CNT_H = 0;
    REG_TM3CNT_L = 0;
    REG_TM3CNT_H = TIMER_START | 1;
    REG_DISPCNT = MODE_4 | BG2_ENABLE | BACKBUFFER;
    set_hardware_scale(1);
    keep_save_id();
    controls_load_config();
    menu_rng_load_state();
    build_luts();
    buildPalette();
    generate_textures();
    MenuResult menu_res = {false, -1, MODE_CREATIVE, true};
#if PERF_AUTOSTART
    g_world_seed = PERF_WORLD_SEED;
    g_world_type = (WorldType)PERF_WORLD_TYPE;
    g_game_mode = MODE_CREATIVE;
    g_allow_mode_switch = true;
    g_render_scale = PERF_RENDER_SCALE;
    g_vis_dist = PERF_VIS_DIST;
    g_face_limit = PERF_FACE_LIMIT;
    g_show_fps = false;
    g_show_hud = true;
    g_show_hand = true;
    g_show_hotbar = true;
    g_show_contour = true;
    menu_res.start_from_load = false;
    menu_res.load_slot = -1;
    menu_res.game_mode = MODE_CREATIVE;
    menu_res.allow_mode_switch = true;
#else
    show_menu(&menu_res);
    input_consume_current_buttons();
#endif
    buildPalette();
    generate_textures();
    mainloop(menu_res);
}
