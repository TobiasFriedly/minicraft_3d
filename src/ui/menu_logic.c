/* ===================== MENU LOGIC ========================== */
static int controls_gap_after(int idx){
    return (idx == 3 || idx == 7 || idx == 12) ? 2 : 0;
}

static void draw_text3x5_shadowed_right_both(const char* text, int right_x, int y, int scale, u8 color, u8 shadow){
    int text_w = text3x5_width(text, scale);
    if(text_w <= 0) return;
    draw_text3x5_shadowed_both(text, right_x - text_w, y, scale, color, shadow);
}

static void show_controls(void){
    set_hardware_scale(1); g_render_scale = 1; update_render_dims();
    u16 pal[256]; menu_build_rgb332_palette(pal);
    for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];

    int sel = 0;
    int scroll = 0;
    int last_sel = -1;
    int last_scroll = -1;
    bool capturing = false;
    bool last_capturing = false;
    bool capture_wait_release = false;
    bool capture_started = false;
    u16 capture_mask = 0;
    const int visible_rows = 10;
    const int list_y = 38;
    const int footer_y = SCREEN_H - 12;

    while(1){
        if(sel < scroll) scroll = sel;
        if(sel >= scroll + visible_rows) scroll = sel - visible_rows + 1;

        if(sel != last_sel || scroll != last_scroll || capturing != last_capturing){
            memset_page8(VRAM_PAGE0, DIRT_COLOR);
            memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_shadowed_centered_both("CONTROLS", 12, 2, MENU_TEXT_WHITE, 251);
            draw_text3x5_shadowed_both("PRESET", 10, 25, 1, MENU_TEXT_LABEL, 251);
            draw_text3x5_shadowed_right_both(input_preset_name(), g_rw - 10, 25, 1, MENU_TEXT_SELECTED, 251);

            int y = list_y;
            for(int i=scroll; i<INPUT_ACTION_COUNT && i<scroll + visible_rows; i++){
                char chord[24];
                format_chord(chord, sizeof(chord), &g_action_bindings[i]);
                if(i == sel) draw_text3x5_shadowed_both(">", 4, y, 1, MENU_TEXT_SELECTED, 251);
                draw_text3x5_shadowed_both(g_input_action_names[i], 12, y, 1, (i == sel) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
                draw_text3x5_shadowed_right_both(chord, g_rw - 10, y, 1, (i == sel) ? MENU_TEXT_SELECTED : MENU_TEXT_LABEL, 251);
                y += 9 + controls_gap_after(i);
            }
            if(capturing) draw_text3x5_shadowed_centered_both("PRESS NEW COMBO", footer_y, 1, MENU_TEXT_SELECTED, 251);
            else draw_text3x5_shadowed_centered_both("A EDIT  L/R PRESET  B BACK", footer_y, 1, MENU_TEXT_LABEL, 251);
            last_sel = sel;
            last_scroll = scroll;
            last_capturing = capturing;
        }

        vblank();
        scanKeys();
        u16 raw_down = keysDown();
        u16 raw_held = keysHeld();
        u16 button_mask = raw_held & (KEY_A | KEY_B | KEY_SELECT | KEY_START | KEY_RIGHT | KEY_LEFT | KEY_UP | KEY_DOWN | KEY_R | KEY_L);

        if(capturing){
            if(capture_wait_release){
                if(button_mask == 0) capture_wait_release = false;
                continue;
            }
            if(!capture_started){
                if(button_mask != 0){
                    capture_started = true;
                    capture_mask = button_mask;
                }
            } else {
                capture_mask |= button_mask;
                if(button_mask == 0){
                    InputChord chord;
                    input_chord_from_mask(&chord, capture_mask);
                    if(chord.count >= 1 && chord.count <= 3){
                        input_assign_action_chord((InputAction)sel, &chord);
                        controls_save_config();
                    }
                    capturing = false;
                    capture_wait_release = false;
                    capture_started = false;
                    capture_mask = 0;
                    last_sel = -1;
                }
            }
            continue;
        }

        if(raw_down & KEY_B) break;
        if(raw_down & KEY_UP){ sel--; if(sel < 0) sel = INPUT_ACTION_COUNT - 1; last_sel = -1; }
        if(raw_down & KEY_DOWN){ sel++; if(sel >= INPUT_ACTION_COUNT) sel = 0; last_sel = -1; }
        if(raw_down & KEY_L){
            int preset_idx = input_active_preset();
            int preset_count = (int)(sizeof(g_control_presets) / sizeof(g_control_presets[0]));
            if(preset_idx < 0) preset_idx = 0;
            preset_idx = (preset_idx + preset_count - 1) % preset_count;
            input_apply_preset(preset_idx);
            controls_save_config();
            last_sel = -1;
        }
        if(raw_down & KEY_R){
            int preset_idx = input_active_preset();
            int preset_count = (int)(sizeof(g_control_presets) / sizeof(g_control_presets[0]));
            if(preset_idx < 0) preset_idx = 0;
            preset_idx = (preset_idx + 1) % preset_count;
            input_apply_preset(preset_idx);
            controls_save_config();
            last_sel = -1;
        }
        if(raw_down & KEY_A){
            capturing = true;
            capture_wait_release = true;
            capture_started = false;
            capture_mask = 0;
            last_sel = -1;
        }
    }
}

static u16 palette_to_grayscale(u16 color){
    int r = color & 31;
    int g = (color >> 5) & 31;
    int b = (color >> 10) & 31;
    int gray = (r * 6 + g * 18 + b * 7) / 31;
    if(gray < 0) gray = 0;
    if(gray > 31) gray = 31;
    return RGB5(gray, gray, gray);
}

static void apply_death_palette(void){
    for(int i=0; i<256; i++) PAL_BG_MEM[i] = palette_to_grayscale(PAL_BG_MEM[i]);
    PAL_BG_MEM[255] = RGB5(31,31,31);
    PAL_BG_MEM[254] = RGB5(31,31,0);
    PAL_BG_MEM[253] = RGB5(18,18,18);
    PAL_BG_MEM[252] = RGB5(8,8,8);
    PAL_BG_MEM[251] = RGB5(0,0,0);
    PAL_BG_MEM[250] = RGB5(20,20,20);
    PAL_BG_MEM[249] = RGB5(31,6,8);
}

static DeathMenuChoice show_death_screen(u8 curBlk, RayHit hit){
    bool saved_hud = g_show_hud;
    bool wait_release = true;
    int selected = 0;
    g_show_hud = false;
    apply_death_palette();
    while(1){
        render_textured(curBlk, hit);
        int title_scale = (g_render_scale > 1) ? 2 : 3;
        int body_scale = (g_render_scale > 1) ? 1 : 2;
        int title_y = g_rh / 5;
        int first_option_y = (g_rh / 2) + 8;
        draw_text3x5_shadowed_centered_bb("YOU DIED", title_y, title_scale, 249, 251);
        draw_text3x5_shadowed_centered_bb("RESPAWN", first_option_y, body_scale, selected == 0 ? 254 : 255, 251);
        draw_text3x5_shadowed_centered_bb("MAIN MENU", first_option_y + (body_scale * 8), body_scale, selected == 1 ? 254 : 255, 251);
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
        if(kd & KEY_UP) selected = 0;
        if(kd & KEY_DOWN) selected = 1;
        if(kd & (KEY_A | KEY_START)){
            buildPalette();
            g_show_hud = saved_hud;
            return (selected == 0) ? DEATH_RESPAWN : DEATH_MAIN_MENU;
        }
    }
    buildPalette();
    g_show_hud = saved_hud;
    return DEATH_RESPAWN;
}

static WorldType show_world_setup(GameMode* out_mode, bool* out_allow_switch){
    set_hardware_scale(1); g_render_scale = 1; update_render_dims();
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int row = 0, last_row = -1;
    int mode = MODE_CREATIVE;
    int wtype = WORLD_CLASSIC;
    bool allow_switch = true;
    while(1){
        if(row != last_row){
            memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_shadowed_centered_both("CREATE WORLD", 14, 2, MENU_TEXT_WHITE, 251);
            draw_text3x5_shadowed_centered_both("MODE", 40, 1, row == 0 ? MENU_TEXT_SELECTED : MENU_TEXT_LABEL, 251);
            draw_text3x5_shadowed_centered_both(mode == MODE_SURVIVAL ? "SURVIVAL" : "CREATIVE", 50, 2, row == 0 ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("WORLD", 74, 1, row == 1 ? MENU_TEXT_SELECTED : MENU_TEXT_LABEL, 251);
            draw_text3x5_shadowed_centered_both(wtype == WORLD_CLASSIC ? "CLASSIC" : (wtype == WORLD_SUPERFLAT ? "SUPERFLAT" : "HIGHLANDS"), 84, 2, row == 1 ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("MODE SWITCH", 108, 1, row == 2 ? MENU_TEXT_SELECTED : MENU_TEXT_LABEL, 251);
            draw_text3x5_shadowed_centered_both(allow_switch ? "ALLOWED" : "LOCKED", 118, 2, row == 2 ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("LEFT/RIGHT CHANGE  A CONFIRM", SCREEN_H - 10, 1, MENU_TEXT_LABEL, 251);
            last_row = row;
        }
        vblank(); scanKeys(); u16 kd = keysDown();
        if(kd & KEY_UP) { row--; if(row < 0) row = 2; last_row = -1; }
        if(kd & KEY_DOWN) { row++; if(row > 2) row = 0; last_row = -1; }
        if(kd & KEY_LEFT){
            if(row == 0) mode = (mode == MODE_SURVIVAL) ? MODE_CREATIVE : MODE_SURVIVAL;
            else if(row == 1) wtype = (wtype + 2) % 3;
            else allow_switch = !allow_switch;
            last_row = -1;
        }
        if(kd & KEY_RIGHT){
            if(row == 0) mode = (mode == MODE_SURVIVAL) ? MODE_CREATIVE : MODE_SURVIVAL;
            else if(row == 1) wtype = (wtype + 1) % 3;
            else allow_switch = !allow_switch;
            last_row = -1;
        }
        if(kd & (KEY_A | KEY_START)){
            if(out_mode) *out_mode = (GameMode)mode;
            if(out_allow_switch) *out_allow_switch = allow_switch;
            return (WorldType)wtype;
        }
    }
}

static void seed_disp_append(char* disp, size_t cap, const char* token){
    if(!disp || cap == 0 || !token || token[0] == '\0') return;
    if(strcmp(disp, "NONE") == 0) disp[0] = '\0';
    size_t len = strlen(disp);
    if(len != 0 && len + 1 < cap){ disp[len++] = ' '; disp[len] = '\0'; }
    if(len < cap) strncat(disp, token, cap - len - 1);
    enum { MAX_DISPLAY_CHARS = 28 };
    len = strlen(disp);
    if(len > MAX_DISPLAY_CHARS){
        size_t cut = len - MAX_DISPLAY_CHARS;
        while(cut < len && disp[cut] != ' ') cut++;
        if(cut < len) cut++;
        memmove(disp, disp + cut, len - cut + 1);
    }
}

static u32 select_seed(void){
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int selected = 0, last_selected = -1;
    while(1){
        if(selected != last_selected){
            memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_shadowed_centered_both("SEED TYPE", 28, 2, MENU_TEXT_WHITE, 251);
            draw_text3x5_shadowed_centered_both("RANDOM", 74, 2, (selected == 0) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("CUSTOM", 98, 2, (selected == 1) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED, 251);
            last_selected = selected;
        }
        vblank(); scanKeys(); u16 kd = keysDown();
        if(kd & (KEY_A | KEY_START)){
            if(selected == 0){
                u32 seed = (u32)REG_VCOUNT ^ ((u32)REG_TM0CNT_L << 1);
                return (seed == 0) ? 0xA53F1D77u : seed;
            } else {
                u32 seed = 0;
                char disp[64];
                strncpy(disp, "NONE", sizeof(disp));
                disp[sizeof(disp) - 1] = '\0';
                int cnt = 0, last = -1;
                while(1){
                    if(cnt != last){
                        memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
                        draw_text3x5_shadowed_centered_both("INPUT SEED", 18, 2, MENU_TEXT_WHITE, 251);
                        draw_text3x5_shadowed_centered_both("PRESS BUTTONS", 46, 1, MENU_TEXT_LABEL, 251);
                        draw_text3x5_shadowed_centered_both(disp, 70, 1, MENU_TEXT_SELECTED, 251);
                        draw_text3x5_shadowed_centered_both("START CONFIRM", 110, 1, MENU_TEXT_LABEL, 251);
                        last = cnt;
                    }
                    vblank(); scanKeys(); u16 kd2 = keysDown();
                    if(kd2 & KEY_START) break;
                    u16 input = kd2 & (u16)~KEY_START;
                    if(cnt < 20 && input){
                        if(input & KEY_L)      seed_disp_append(disp, sizeof(disp), "L");
                        if(input & KEY_R)      seed_disp_append(disp, sizeof(disp), "R");
                        if(input & KEY_A)      seed_disp_append(disp, sizeof(disp), "A");
                        if(input & KEY_B)      seed_disp_append(disp, sizeof(disp), "B");
                        if(input & KEY_SELECT) seed_disp_append(disp, sizeof(disp), "SEL");
                        if(input & KEY_UP)     seed_disp_append(disp, sizeof(disp), "DU");
                        if(input & KEY_DOWN)   seed_disp_append(disp, sizeof(disp), "DD");
                        if(input & KEY_LEFT)   seed_disp_append(disp, sizeof(disp), "DL");
                        if(input & KEY_RIGHT)  seed_disp_append(disp, sizeof(disp), "DR");
                        u16 x = input; x ^= x >> 8; seed = seed * 1664525u + (u32)x + 1013904223u;
                        cnt++;
                    }
                }
                return (seed == 0) ? 0xA53F1D77u : seed;
            }
        } else {
            if(kd & KEY_UP) selected = 0;
            if(kd & KEY_DOWN) selected = 1;
        }
    }
}

static MenuResult choose_new_or_load(void){
    MenuResult r = {false, -1, MODE_CREATIVE, true};
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int sel = 0, last=-1;
    while(1){
        if(sel!=last){
            memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_shadowed_centered_both("START GAME", 18, 2, MENU_TEXT_WHITE, 251);
            draw_text3x5_shadowed_centered_both("NEW WORLD",  64, 2, (sel==0)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("LOAD WORLD", 92, 2, (sel==1)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED, 251);
            draw_text3x5_shadowed_centered_both("A SELECT   B BACK", SCREEN_H-10, 1, MENU_TEXT_LABEL, 251);
            last=sel;
        }
        vblank(); scanKeys(); u16 kd=keysDown();
        if(kd & KEY_UP)   sel=0; if(kd & KEY_DOWN) sel=1;
        if(kd & KEY_B){ r.start_from_load=false; r.load_slot=-1; return r; }
        if(kd & (KEY_A|KEY_START)){
            if(sel==0){ r.start_from_load=false; r.load_slot=-1; return r; }
            else{
                int slotSel=0, last2=-1;
                while(1){
                    bool pack_in_slot2 = texture_pack_sram_present();
                    SaveSlotHeader h0,h1; bool v0 = slot_is_valid(0,&h0); bool v1 = (!pack_in_slot2 && slot_is_valid(1,&h1));
                    if(slotSel!=last2){
                        memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
                        draw_text3x5_shadowed_centered_both("LOAD WORLD", 18, 2, MENU_TEXT_WHITE, 251);
                        char s0[32], s1[32];
                        if(v0) snprintf(s0, sizeof(s0), "SLOT 1  %s", h0.game_mode == MODE_SURVIVAL ? "SURV" : "CREA");
                        else snprintf(s0,sizeof(s0),"SLOT 1  EMPTY");
                        if(pack_in_slot2) snprintf(s1, sizeof(s1), "SLOT 2  TEX PACK");
                        else if(v1) snprintf(s1, sizeof(s1), "SLOT 2  %s", h1.game_mode == MODE_SURVIVAL ? "SURV" : "CREA");
                        else snprintf(s1,sizeof(s1),"SLOT 2  EMPTY");
                        draw_text3x5_shadowed_centered_both(s0, 66, 2, (slotSel==0)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED, 251);
                        draw_text3x5_shadowed_centered_both(s1,92, 2, (slotSel==1)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED, 251);
                        draw_text3x5_shadowed_centered_both("A LOAD   B BACK", SCREEN_H-10, 1, MENU_TEXT_LABEL, 251);
                        last2=slotSel;
                    }
                    vblank(); scanKeys(); u16 kd2=keysDown();
                    if(kd2 & KEY_UP) slotSel=0; if(kd2 & KEY_DOWN) slotSel=1;
                    if(kd2 & KEY_B) break;
                    if(kd2 & (KEY_A|KEY_START)){
                        if(slotSel==0 && slot_is_valid(0,NULL)){ r.start_from_load=true; r.load_slot=0; return r; }
                        if(slotSel==1 && !pack_in_slot2 && slot_is_valid(1,NULL)){ r.start_from_load=true; r.load_slot=1; return r; }
                    }
                }
                last=-1;
            }
        }
    }
}

static void show_menu(MenuResult* out_result){
    int saved_scale = g_render_scale;
    bool saved_show_hud = g_show_hud;
    bool saved_show_contour = g_show_contour;
    bool saved_show_hand = g_show_hand;
    bool saved_show_hotbar = g_show_hotbar;
    bool saved_show_fps = g_show_fps;
    bool saved_in_water = g_in_water;
    bool saved_head_in_water = g_head_in_water;
    int saved_burn_timer = g_burn_timer;
    u16 basePal[256];
    set_hardware_scale(1);
    g_render_scale = 1;
    update_render_dims();
    menu_build_rgb332_palette(basePal);
    for(int i = 0; i < 256; i++) PAL_BG_MEM[i] = 0;
    CpuFastSet(image1, (void*)VRAM_PAGE0, ((SCREEN_W*SCREEN_H)>>2) | COPY32);
    CpuFastSet(image1, (void*)VRAM_PAGE1, ((SCREEN_W*SCREEN_H)>>2) | COPY32);
    for(int s = 0; s <= 30; s++){
        if(30 <= 0) break;
        for(int i = 0; i < 256; i++){
            u16 c = basePal[i];
            int r = (c & 31) * s / 30;
            int g = ((c >> 5) & 31) * s / 30;
            int b = ((c >> 10) & 31) * s / 30;
            PAL_BG_MEM[i] = (u16)(r | (g<<5) | (b<<10));
        }
        vblank();
    }
    for(int t = 0; t < 20; t++) vblank();
    clear_dropped_items();
    build_menu_scene_with_loading();
    buildPalette();
    backbuffer = vram_backbuffer();

    g_show_hud = false;
    g_show_contour = false;
    g_show_hand = false;
    g_show_hotbar = false;
    g_show_fps = false;
    g_in_water = false;
    g_head_in_water = false;
    g_burn_timer = 0;

    int selected = 0;
    while(1){
        menu_update_camera();
        render_textured(0, (RayHit){0});
        draw_menu_live_overlay(selected);
        present_and_flip();

        scanKeys(); u16 kd = keysDown();
        if(kd & KEY_UP) selected = 0; if(kd & KEY_DOWN) selected = 1;
        if(kd & (KEY_A | KEY_START)){
            if(selected == 0){
                MenuResult r = choose_new_or_load();
                if(!r.start_from_load){
                    GameMode mode = MODE_CREATIVE;
                    bool allow_switch = true;
                    WorldType wtype = show_world_setup(&mode, &allow_switch);
                    g_world_type = wtype;
                    g_game_mode = mode;
                    g_allow_mode_switch = allow_switch;
                    if(wtype == WORLD_SUPERFLAT){ g_world_seed = 0; }
                    else { g_world_seed = select_seed(); }
                    g_dimension = 0;
                    out_result->start_from_load = false;
                    out_result->load_slot = -1;
                    out_result->game_mode = mode;
                    out_result->allow_mode_switch = allow_switch;
                    break;
                }else{
                    out_result->start_from_load = true;
                    out_result->load_slot = r.load_slot;
                    out_result->game_mode = r.game_mode;
                    out_result->allow_mode_switch = r.allow_mode_switch;
                    break;
                }
            } else {
                show_controls();
                buildPalette();
                input_consume_current_buttons();
            }
        }
    }

    g_show_hud = saved_show_hud;
    g_show_contour = saved_show_contour;
    g_show_hand = saved_show_hand;
    g_show_hotbar = saved_show_hotbar;
    g_show_fps = saved_show_fps;
    g_in_water = saved_in_water;
    g_head_in_water = saved_head_in_water;
    g_burn_timer = saved_burn_timer;
    g_render_scale = saved_scale;
    update_render_dims();
    set_hardware_scale(g_render_scale);
}

static void return_to_main_menu(u8* io_curBlk, bool store_state){
    if(store_state) store_current_dimension_state();
    if(g_spectator_mode) set_spectator_mode(false, &g_show_hud, false);

    MenuResult r2 = {false, -1, MODE_SURVIVAL, true};
    r2.game_mode = MODE_CREATIVE;
    show_menu(&r2);
    g_dimension = 0;
    buildPalette();

    bool loaded = false;
    if(r2.start_from_load && load_world_from_slot(r2.load_slot, io_curBlk)) loaded = true;

    clear_dropped_items();
    stream_reset_state();
    g_is_sprinting = false;

    if(!loaded){
        g_game_mode = r2.game_mode;
        g_allow_mode_switch = r2.allow_mode_switch;
        g_creative_flying = false;
        g_spectator_mode = false;
        g_spectator_saved_hud_valid = false;
        g_jump_tap_timer = 0;
        memset(g_dim_state, 0, sizeof(g_dim_state));
        editlog_clear_all();
        stream_center_on_global(VW/2, VD/2);
        respawn_player(RESPAWN_VISUAL_GENERATE);
        *io_curBlk = (g_game_mode == MODE_CREATIVE) ? (BLK_GRASS + 1) : ITEM_NONE;
        if(g_game_mode == MODE_SURVIVAL) survival_init_inventory();
    }

    init_quickbar();
    if(g_game_mode == MODE_CREATIVE){
        if(!g_spectator_mode) g_creative_flying = false;
        g_quickbar[0] = *io_curBlk;
        g_hotbar_slot = 0;
        g_hotbar_prev_slot = 0;
    } else {
        g_creative_flying = false;
        refresh_selected_item(io_curBlk);
    }
    input_consume_current_buttons();
}

static void render_textured(u8 selected_block, RayHit hit);

static void show_graphics_settings(u8 curBlk) {
    const u8 C_WHITE=255, C_HILIGHT=254, C_DIM=253;
    int page = 0; // 0..2
    int sel = 0;
    bool saved_hud_state = g_show_hud; g_show_hud = false;
    while(1) {
        render_textured(curBlk, (RayHit){0});
        int title_scale = (g_render_scale > 1) ? 2 : 3;
        int body_scale = (g_render_scale > 1) ? 1 : 2;
        int title_y = 12 / g_render_scale;
        int page_y = 30 / g_render_scale;
        int list_y0 = 52 / g_render_scale;
        int step_y = 14 / g_render_scale;
        int footer_y = g_rh - 14 / g_render_scale;
        int min_step = (5 * body_scale) + body_scale + 1;
        if(step_y < min_step) step_y = min_step;

        draw_text3x5_shadowed_centered_bb("GRAPHICS SETTINGS", title_y, title_scale, C_WHITE, 251);
        char pagebuf[16];
        snprintf(pagebuf, sizeof(pagebuf), "PAGE %d/3", page + 1);
        draw_text3x5_shadowed_centered_bb(pagebuf, page_y, body_scale, C_DIM, 251);
        char buf[32];

        int y = list_y0;
        if(page == 0) {
            const int N = 5;
            if(sel >= N) sel = N - 1;

            snprintf(buf, 32, "DIST: %d", g_vis_dist);
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==0 ? C_HILIGHT : C_DIM, 251); y += step_y;

            // Updated label for Hardware Scaling
            snprintf(buf, 32, "SCALE: %dX", g_render_scale);
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==1 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "FOV: %d", g_fov_val);
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==2 ? C_HILIGHT : C_DIM, 251); y += step_y;

            int cull_val = F2I(fmuli(g_frustum_margin, F(10)));
            snprintf(buf, 32, "CULL: %d.%d", cull_val/10, cull_val%10);
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==3 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "MAX FACES: %d", g_face_limit);
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==4 ? C_HILIGHT : C_DIM, 251);
        } else if(page == 1) {
            const int N = 7;
            if(sel >= N) sel = N - 1;

            snprintf(buf, 32, "FPS: %s", g_show_fps ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==0 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "HUD: %s", saved_hud_state ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==1 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "HAND: %s", g_show_hand ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==2 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "HOTBAR: %s", g_show_hotbar ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==3 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "CONTOUR: %s", g_show_contour ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==4 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "TEXTURES: %s", g_textures_enabled ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==5 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "FALLING: %s", g_falling_blocks ? "ON" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==6 ? C_HILIGHT : C_DIM, 251);
        } else {
            const int N = (g_game_mode == MODE_CREATIVE) ? 5 : 4;
            if(sel >= N) sel = N - 1;

            snprintf(buf, 32, "SKY: %s", (g_sky_mode == 1) ? "FLAT" : "GRAD");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==0 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "TEX SIZE: %s", g_use_16x16_textures ? "16X" : "8X");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==1 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "PACK: %s", texture_pack_status_label());
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==2 ? C_HILIGHT : C_DIM, 251); y += step_y;

            snprintf(buf, 32, "FRAMESKIP: %s", g_auto_frameskip ? "AUTO" : "OFF");
            draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==3 ? C_HILIGHT : C_DIM, 251);
            if(g_game_mode == MODE_CREATIVE){
                y += step_y;
                snprintf(buf, 32, "SPECTATOR: %s", g_spectator_mode ? "ON" : "OFF");
                draw_text3x5_shadowed_centered_bb(buf, y, body_scale, sel==4 ? C_HILIGHT : C_DIM, 251);
            }
        }

        draw_text3x5_shadowed_centered_bb("L/R PAGE  DPAD ADJUST  B BACK", footer_y, body_scale, C_HILIGHT, 251);
        present_and_flip();
        scanKeys(); u16 kd = keysDown(); u16 kh = keysHeld();
        if(kd & KEY_L) { page = (page + 2) % 3; sel = 0; }
        if(kd & KEY_R) { page = (page + 1) % 3; sel = 0; }
        int N = (page == 0) ? 5 : (page == 1 ? 7 : ((g_game_mode == MODE_CREATIVE) ? 5 : 4));
        if(kd & KEY_UP) { sel--; if(sel<0) sel=N-1; }
        if(kd & KEY_DOWN) { sel++; if(sel>=N) sel=0; }
        if(kd & KEY_B) break;
        bool left = (kd & KEY_LEFT) || (kh & KEY_LEFT && g_frame % 5 == 0);
        bool right = (kd & KEY_RIGHT) || (kh & KEY_RIGHT && g_frame % 5 == 0);
        if(left || right) {
            int dir = right ? 1 : -1;
            if(page == 0) {
                switch(sel) {
                    case 0:
                        g_vis_dist += dir;
                        if(g_vis_dist < 2) g_vis_dist = 2;
                        if(g_vis_dist > 20) g_vis_dist = 20;
                        break;
                    case 1:
                        g_render_scale += dir;
                        if(g_render_scale < 1) g_render_scale = 1;
                        if(g_render_scale > 4) g_render_scale = 4;
                        if(g_render_scale != 2 && g_fov_val > 150) g_fov_val = 150;
                        update_render_dims();
                        break;
                    case 2:
                        g_fov_val += dir * 2;
                        if(g_fov_val < 30) g_fov_val = 30;
                        if(g_render_scale == 2){
                            if(g_fov_val > 170) g_fov_val = 170;
                        } else {
                            if(g_fov_val > 150) g_fov_val = 150;
                        }
                        g_focal = F(220 - g_fov_val);
                        break;
                    case 3:
                        g_frustum_margin += F(dir * 0.1f);
                        if(g_frustum_margin < F(0.5f)) g_frustum_margin = F(0.5f);
                        if(g_frustum_margin > F(3.0f)) g_frustum_margin = F(3.0f);
                        break;
                    case 4:
                        g_face_limit += dir * 100;
                        if(g_face_limit < 200) g_face_limit = 200;
                        if(g_face_limit > MAX_FACES) g_face_limit = MAX_FACES;
                        break;
                }
            } else if(page == 1) {
                switch(sel) {
                    case 0:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_show_fps = !g_show_fps;
                        break;
                    case 1:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) saved_hud_state = !saved_hud_state;
                        break;
                    case 2:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_show_hand = !g_show_hand;
                        break;
                    case 3:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_show_hotbar = !g_show_hotbar;
                        break;
                    case 4:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_show_contour = !g_show_contour;
                        break;
                    case 5:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_textures_enabled = !g_textures_enabled;
                        break;
                    case 6:
                        if(kd & (KEY_LEFT|KEY_RIGHT)) g_falling_blocks = !g_falling_blocks;
                        break;
                }
            } else {
                if(sel == 0){
                    if(kd & (KEY_LEFT|KEY_RIGHT)) g_sky_mode = (g_sky_mode == 1) ? 0 : 1;
                } else if(sel == 1 && (kd & (KEY_LEFT|KEY_RIGHT))){
                    g_use_16x16_textures = !g_use_16x16_textures;
                    generate_textures();
                } else if(sel == 2 && (kd & (KEY_LEFT|KEY_RIGHT))){
                    if(g_sram_texture_pack_present){
                        g_texture_pack_enabled = !g_texture_pack_enabled;
                        generate_textures();
                    }
                } else if(sel == 3 && (kd & (KEY_LEFT|KEY_RIGHT))){
                    g_auto_frameskip = !g_auto_frameskip;
                } else if(sel == 4 && g_game_mode == MODE_CREATIVE && (kd & (KEY_LEFT|KEY_RIGHT))){
                    set_spectator_mode(!g_spectator_mode, &saved_hud_state, true);
                }
            }
        }
        g_frame++;
    }
    g_show_hud = saved_hud_state;
}

static PauseResult show_pause_menu(u8* io_curBlk){
    // Force scale 1 for clear menu text
    set_hardware_scale(1); 
    int saved_scale = g_render_scale;
    g_render_scale = 1; update_render_dims();

    const u8 C_BG=252, C_WHITE=255, C_HILIGHT=254, C_DIM=253;
    const int N = 6;
    int sel = 0, last = -1;
    bool wait_for_start_release = true;
    while(1){
        if(sel != last){
            memset_page8(VRAM_PAGE0, C_BG); memset_page8(VRAM_PAGE1, C_BG);
            draw_text3x5_shadowed_centered_both("PAUSED", 16, 2, C_WHITE, 251);
            int y = 42;
            for(int i=0;i<N;i++){
                char line[32];
                if(i == 0) strncpy(line, "RESUME", sizeof(line));
                else if(i == 1) strncpy(line, "GRAPHICS", sizeof(line));
                else if(i == 2) strncpy(line, "SAVE SLOT 1", sizeof(line));
                else if(i == 3) strncpy(line, texture_pack_sram_present() ? "PACK IN SLOT 2" : "SAVE SLOT 2", sizeof(line));
                else if(i == 4) snprintf(line, sizeof(line), "MODE: %s", g_game_mode == MODE_SURVIVAL ? "SURVIVAL" : "CREATIVE");
                else strncpy(line, "MAIN MENU", sizeof(line));
                line[sizeof(line) - 1] = '\0';
                u8 c = (i==sel)?C_WHITE:C_DIM;
                if(i == 4 && !g_allow_mode_switch) c = (i==sel) ? C_HILIGHT : C_DIM;
                draw_text3x5_shadowed_centered_both(line, y, 2, c, 251);
                y += 16;
            }
            draw_text3x5_shadowed_centered_both("A SELECT  LEFT/RIGHT MODE  START RESUME", SCREEN_H-10, 1, C_HILIGHT, 251);
            last = sel;
        }
        vblank(); scanKeys(); u16 kd = keysDown(); u16 kh = keysHeld();
        if(wait_for_start_release){
            if((kh & KEY_START) == 0) wait_for_start_release = false;
            kd &= (u16)~KEY_START;
        }
        if(kd & KEY_UP){ sel--; if(sel<0) sel=N-1; } if(kd & KEY_DOWN){ sel++; if(sel>=N) sel=0; }
        if(kd & KEY_START) break;
        if(sel == 4 && (kd & (KEY_LEFT|KEY_RIGHT|KEY_A))){
            if(!g_allow_mode_switch){
                show_toast("MODE SWITCH LOCKED");
            } else {
                g_game_mode = (g_game_mode == MODE_SURVIVAL) ? MODE_CREATIVE : MODE_SURVIVAL;
                if(g_game_mode != MODE_CREATIVE) set_spectator_mode(false, &g_show_hud, false);
                refresh_selected_item(io_curBlk);
                show_toast(g_game_mode == MODE_SURVIVAL ? "MODE: SURVIVAL" : "MODE: CREATIVE");
            }
            last = -1;
            continue;
        }
        if(kd & KEY_A){
            if(sel == 0){ break; }
            else if (sel == 1) { 
                // Restore logic scaling for the preview inside settings, but settings loop handles display
                g_render_scale = saved_scale; update_render_dims();
                show_graphics_settings(*io_curBlk); 
                saved_scale = g_render_scale; // capture changes
                g_render_scale = 1; update_render_dims(); set_hardware_scale(1);
                last = -1; 
            }
            else if(sel == 2 || sel == 3){
                int slot = (sel==2)?0:1;
                if(slot == 1 && texture_pack_sram_present()){
                    draw_text3x5_centered_both("PACK USES SLOT 2", SCREEN_H-36, 2, C_HILIGHT);
                } else {
                    store_current_dimension_state();
                    bool ok = save_world_and_verify(slot, *io_curBlk);
                    if(ok) draw_text3x5_centered_both(slot==0?"SAVED SLOT 1":"SAVED SLOT 2", SCREEN_H-36, 2, C_HILIGHT);
                    else   draw_text3x5_centered_both("SAVE FAILED", SCREEN_H-36, 2, C_HILIGHT);
                }
                for(int t=0;t<60;t++) vblank();
                last = -1; 
            } else if(sel == 5){ 
                g_render_scale = saved_scale; update_render_dims();
                return PM_TO_MAINMENU; 
            }
        }
    }
    // Restore game scaling
    g_render_scale = saved_scale; update_render_dims();
    return PM_RESUME;
}

