/* ===================== SETTINGS & GLOBALS ================== */
static int g_vis_dist = 9;
static EWRAM_DATA fix g_focal    = F(130.0f);
static EWRAM_DATA int g_fov_val  = 120; 
static EWRAM_DATA fix g_frustum_margin = F(1.2f);
static bool g_show_hud = true;
static bool g_show_contour = true;
static bool g_show_hand = true;
static bool g_show_hotbar = true;
static bool g_show_fps = false;
static bool g_in_water = false;
static bool g_head_in_water = false;
static bool g_falling_blocks = false;
static bool g_auto_frameskip = false;
static bool g_creative_flying = false;
static bool g_spectator_mode = false;
static bool g_spectator_saved_hud = true;
static bool g_spectator_saved_hud_valid = false;
static int g_jump_tap_timer = 0;

// g_render_scale now controls Hardware Scaling.
// 1 = 240x160, 2 = 120x80, 3 = 80x53, 4 = 60x40
static EWRAM_DATA int g_render_scale = 2; 
static EWRAM_DATA int g_face_limit = 1600; 
static int g_sky_mode = 0; 
static bool g_textures_enabled = true;
static EWRAM_DATA u8 g_quickbar[QUICKBAR_SLOTS];
static int g_hotbar_slot = 0;
static int g_hotbar_prev_slot = 0;
static GameMode g_game_mode = MODE_CREATIVE;
static bool g_allow_mode_switch = true;
static EWRAM_DATA InputChord g_action_bindings[INPUT_ACTION_COUNT];
static EWRAM_DATA ActionState g_action_state[INPUT_ACTION_COUNT];
static EWRAM_DATA u16 g_prev_raw_buttons = 0;
static EWRAM_DATA InventorySlot g_inventory[SURVIVAL_INV_SLOTS];
static u8 g_player_health = 20;
static u8 g_player_max_health = 20;
static u8 g_player_air = 10;
static u8 g_player_max_air = 10;
static int g_mining_ticks = 0;
static int g_mining_x = -1, g_mining_y = -1, g_mining_z = -1;
static int g_health_flash_timer = 0;
static int g_health_flash_unit = -1;
static int g_air_flash_timer = 0;
static int g_air_flash_unit = -1;
static int g_burn_timer = 0;

#define MAX_DROPPED_ITEMS 20
typedef struct {
    bool active;
    u8 item;
    u16 age;
    v3 pos;
    v3 vel;
} DroppedItem;
static EWRAM_DATA DroppedItem g_dropped_items[MAX_DROPPED_ITEMS];

#define MAX_BREAK_PARTICLES 32
typedef struct {
    bool active;
    u8 color;
    u8 life;
    v3 pos;
    v3 vel;
} BreakParticle;
static EWRAM_DATA BreakParticle g_break_particles[MAX_BREAK_PARTICLES];

#define MAX_TNT_ENTITIES 12
typedef struct {
    bool active;
    u8 fuse;
    s8 x;
    s8 y;
    s8 z;
} PrimedTnt;
static EWRAM_DATA PrimedTnt g_tnt_entities[MAX_TNT_ENTITIES];

static const u16 g_input_button_masks[INPUT_BTN_COUNT] = {
    KEY_A, KEY_B, KEY_SELECT, KEY_START, KEY_RIGHT,
    KEY_LEFT, KEY_UP, KEY_DOWN, KEY_R, KEY_L
};

static const char* g_input_button_short_names[INPUT_BTN_COUNT] = {
    "A", "B", "SEL", "STA", "RT",
    "LT", "UP", "DN", "R", "L"
};

static const u8 g_input_button_display_order[INPUT_BTN_COUNT] = {
    INPUT_BTN_SELECT, INPUT_BTN_START, INPUT_BTN_L, INPUT_BTN_R, INPUT_BTN_UP,
    INPUT_BTN_DOWN, INPUT_BTN_LEFT, INPUT_BTN_RIGHT, INPUT_BTN_A, INPUT_BTN_B
};

static const char* g_input_action_names[INPUT_ACTION_COUNT] = {
    "FORWARD",
    "BACK",
    "LEFT",
    "RIGHT",
    "TURN L",
    "TURN R",
    "LOOK UP",
    "LOOK DOWN",
    "JUMP / UP",
    "FLY DOWN",
    "BREAK",
    "PLACE / USE",
    "SPRINT",
    "INVENTORY",
    "HOTBAR L",
    "HOTBAR R",
    "PAUSE",
    "RESET"
};

static const ControlPreset g_control_presets[] = {
    {
        "CLASSIC",
        {
            INPUT_CHORD1(INPUT_BTN_UP),
            INPUT_CHORD1(INPUT_BTN_DOWN),
            INPUT_CHORD1(INPUT_BTN_LEFT),
            INPUT_CHORD1(INPUT_BTN_RIGHT),
            INPUT_CHORD1(INPUT_BTN_L),
            INPUT_CHORD1(INPUT_BTN_R),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_L),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_R),
            INPUT_CHORD1(INPUT_BTN_B),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_B),
            INPUT_CHORD1(INPUT_BTN_A),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_A),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_UP),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_LEFT),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_RIGHT),
            INPUT_CHORD1(INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_DOWN)
        }
    },
    {
        "SHOULDER",
        {
            INPUT_CHORD1(INPUT_BTN_UP),
            INPUT_CHORD1(INPUT_BTN_DOWN),
            INPUT_CHORD1(INPUT_BTN_LEFT),
            INPUT_CHORD1(INPUT_BTN_RIGHT),
            INPUT_CHORD3(INPUT_BTN_L, INPUT_BTN_R, INPUT_BTN_LEFT),
            INPUT_CHORD3(INPUT_BTN_L, INPUT_BTN_R, INPUT_BTN_RIGHT),
            INPUT_CHORD3(INPUT_BTN_L, INPUT_BTN_R, INPUT_BTN_UP),
            INPUT_CHORD3(INPUT_BTN_L, INPUT_BTN_R, INPUT_BTN_DOWN),
            INPUT_CHORD1(INPUT_BTN_B),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_B),
            INPUT_CHORD1(INPUT_BTN_A),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_A),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_UP),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_LEFT),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_RIGHT),
            INPUT_CHORD1(INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_DOWN)
        }
    },
    {
        "ALT A/B",
        {
            INPUT_CHORD1(INPUT_BTN_UP),
            INPUT_CHORD1(INPUT_BTN_DOWN),
            INPUT_CHORD1(INPUT_BTN_LEFT),
            INPUT_CHORD1(INPUT_BTN_RIGHT),
            INPUT_CHORD1(INPUT_BTN_L),
            INPUT_CHORD1(INPUT_BTN_R),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_L),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_R),
            INPUT_CHORD1(INPUT_BTN_A),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_A),
            INPUT_CHORD1(INPUT_BTN_B),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_B),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_UP),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_LEFT),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_RIGHT),
            INPUT_CHORD1(INPUT_BTN_START),
            INPUT_CHORD2(INPUT_BTN_SELECT, INPUT_BTN_DOWN)
        }
    },
};

static void init_quickbar(void){
    g_quickbar[0] = BLK_GRASS + 1;
    g_quickbar[1] = BLK_DIRT + 1;
    g_quickbar[2] = BLK_STONE + 1;
    g_quickbar[3] = BLK_WOOD + 1;
    g_quickbar[4] = BLK_WATER + 1;
    g_hotbar_slot = 0;
    g_hotbar_prev_slot = 0;
}

static bool input_chords_equal(const InputChord* a, const InputChord* b){
    if(a->count != b->count) return false;
    for(int i=0; i<a->count; i++) if(a->buttons[i] != b->buttons[i]) return false;
    return true;
}

static bool input_chord_matches_mask(const InputChord* chord, u16 held_mask){
    if(chord->count == 0 || chord->count > 3) return false;
    for(int i=0; i<chord->count; i++){
        if(!(held_mask & g_input_button_masks[chord->buttons[i]])) return false;
    }
    return true;
}

static bool input_chord_strict_subset(const InputChord* a, const InputChord* b){
    if(a->count == 0 || a->count >= b->count) return false;
    for(int i=0; i<a->count; i++){
        bool found = false;
        for(int j=0; j<b->count; j++){
            if(a->buttons[i] == b->buttons[j]){
                found = true;
                break;
            }
        }
        if(!found) return false;
    }
    return true;
}

static void input_chord_from_mask(InputChord* out, u16 mask){
    int total = 0;
    out->count = 0;
    for(int i=0; i<3; i++) out->buttons[i] = 0;
    for(int i=0; i<INPUT_BTN_COUNT; i++){
        u8 btn = g_input_button_display_order[i];
        if(!(mask & g_input_button_masks[btn])) continue;
        if(total < 3) out->buttons[total] = btn;
        total++;
    }
    out->count = (u8)total;
}

static void format_chord(char* out, size_t cap, const InputChord* chord){
    out[0] = '\0';
    if(chord->count == 0){
        snprintf(out, cap, "---");
        return;
    }
    for(int i=0; i<chord->count; i++){
        if(i != 0) strncat(out, "+", cap - strlen(out) - 1);
        strncat(out, g_input_button_short_names[chord->buttons[i]], cap - strlen(out) - 1);
    }
}

static void input_reset_action_state(void){
    memset(g_action_state, 0, sizeof(g_action_state));
    g_prev_raw_buttons = 0;
}

static void input_apply_bindings(const InputChord* bindings){
    memcpy(g_action_bindings, bindings, sizeof(g_action_bindings));
    input_reset_action_state();
}

static void input_apply_preset(int preset_idx){
    int preset_count = (int)(sizeof(g_control_presets) / sizeof(g_control_presets[0]));
    if(preset_idx < 0 || preset_idx >= preset_count) return;
    input_apply_bindings(g_control_presets[preset_idx].bindings);
}

static int input_active_preset(void){
    int preset_count = (int)(sizeof(g_control_presets) / sizeof(g_control_presets[0]));
    for(int i=0; i<preset_count; i++){
        if(memcmp(g_action_bindings, g_control_presets[i].bindings, sizeof(g_action_bindings)) == 0) return i;
    }
    return -1;
}

static const char* input_preset_name(void){
    int idx = input_active_preset();
    return (idx >= 0) ? g_control_presets[idx].name : "CUSTOM";
}

static void input_assign_action_chord(InputAction action, const InputChord* chord){
    if(action >= INPUT_ACTION_COUNT || chord->count == 0 || chord->count > 3) return;
    for(int i=0; i<INPUT_ACTION_COUNT; i++){
        if(i == action) continue;
        if(input_chords_equal(&g_action_bindings[i], chord)){
            InputChord tmp = g_action_bindings[action];
            g_action_bindings[action] = *chord;
            g_action_bindings[i] = tmp;
            input_reset_action_state();
            return;
        }
    }
    g_action_bindings[action] = *chord;
    input_reset_action_state();
}

static void input_update_actions(u16 raw_held){
    bool matched_now[INPUT_ACTION_COUNT];
    bool matched_prev[INPUT_ACTION_COUNT];
    bool suppressed_now[INPUT_ACTION_COUNT];
    bool suppressed_prev[INPUT_ACTION_COUNT];

    for(int i=0; i<INPUT_ACTION_COUNT; i++){
        matched_now[i] = input_chord_matches_mask(&g_action_bindings[i], raw_held);
        matched_prev[i] = input_chord_matches_mask(&g_action_bindings[i], g_prev_raw_buttons);
        suppressed_now[i] = false;
        suppressed_prev[i] = false;
    }

    for(int i=0; i<INPUT_ACTION_COUNT; i++){
        for(int j=0; j<INPUT_ACTION_COUNT; j++){
            if(i == j) continue;
            if(matched_now[i] && matched_now[j] && input_chord_strict_subset(&g_action_bindings[i], &g_action_bindings[j])) suppressed_now[i] = true;
            if(matched_prev[i] && matched_prev[j] && input_chord_strict_subset(&g_action_bindings[i], &g_action_bindings[j])) suppressed_prev[i] = true;
        }
    }

    for(int i=0; i<INPUT_ACTION_COUNT; i++){
        g_action_state[i].held = matched_now[i] && !suppressed_now[i];
        g_action_state[i].pressed = g_action_state[i].held && !(matched_prev[i] && !suppressed_prev[i]);
    }
    g_prev_raw_buttons = raw_held;
}

static void input_consume_current_buttons(void){
    scanKeys();
    memset(g_action_state, 0, sizeof(g_action_state));
    g_prev_raw_buttons = keysHeld();
}

static void inventory_clear(void){
    memset(g_inventory, 0, sizeof(g_inventory));
}

static bool item_is_block_item(u8 item){
    return item >= 1 && item <= BLK_COUNT &&
           item != (BLK_GRASS_SIDE + 1) &&
           item != (BLK_TRACK_SIDE + 1) &&
           item != (BLK_TNT_LIT + 1);
}

static bool item_is_placeable(u8 item){
    return item_is_block_item(item);
}

static bool item_is_tool(u8 item){
    return item == ITEM_FLINT_STEEL || item == ITEM_PICK_WOOD || item == ITEM_PICK_STONE || item == ITEM_PICK_IRON;
}

static int item_max_stack(u8 item){
    return item_is_tool(item) ? 1 : 64;
}

static bool item_is_pickaxe(u8 item){
    return item == ITEM_PICK_WOOD || item == ITEM_PICK_STONE || item == ITEM_PICK_IRON;
}

static int pickaxe_level(u8 item){
    if(item == ITEM_PICK_WOOD) return 1;
    if(item == ITEM_PICK_STONE) return 2;
    if(item == ITEM_PICK_IRON) return 3;
    return 0;
}

static const char* block_name_from_id(u8 blk){
    switch(blk){
        case BLK_GRASS: return "GRASS";
        case BLK_DIRT: return "DIRT";
        case BLK_STONE: return "STONE";
        case BLK_ORE_IRON: return "IRON ORE";
        case BLK_ORE_GOLD: return "GOLD ORE";
        case BLK_ORE_DIAMOND: return "DIAM ORE";
        case BLK_TRACK: return "TRACK";
        case BLK_COBWEB: return "COBWEB";
        case BLK_TNT:
        case BLK_TNT_LIT: return "TNT";
        case BLK_WATER: return "WATER";
        case BLK_WOOD: return "WOOD";
        case BLK_LEAF: return "LEAF";
        case BLK_SAND: return "SAND";
        case BLK_BRICK: return "BRICK";
        case BLK_PLANK: return "PLANK";
        case BLK_COBBLE: return "COBBLE";
        case BLK_BEDROCK: return "BEDROCK";
        case BLK_OBSIDIAN: return "OBSIDIAN";
        case BLK_NETHERRACK: return "NETHERRACK";
        case BLK_PORTAL: return "PORTAL";
        case BLK_LAVA: return "LAVA";
        case BLK_GRAVEL: return "GRAVEL";
        case BLK_CLAY: return "CLAY";
        case BLK_MOSSY_COBBLE: return "MOSSY COB";
        case BLK_NETHER_BRICK: return "NETHER BR";
        case BLK_BASALT: return "BASALT";
        case BLK_CACTUS: return "CACTUS";
        case BLK_GLASS: return "GLASS";
        case BLK_WOOD_DARK: return "WOOD DARK";
        case BLK_WOOD_LIGHT: return "WOOD LIGHT";
        case BLK_LEAF_DARK: return "LEAF DARK";
        case BLK_LEAF_LIGHT: return "LEAF LIGHT";
        default: return "ITEM";
    }
}

static const char* item_name(u8 item){
    if(item_is_block_item(item)) return block_name_from_id((u8)(item - 1));
    switch(item){
        case ITEM_FLINT_STEEL: return "FLINT N STEEL";
        case ITEM_STICK: return "STICK";
        case ITEM_PICK_WOOD: return "WOOD PICK";
        case ITEM_PICK_STONE: return "STONE PICK";
        case ITEM_PICK_IRON: return "IRON PICK";
        case ITEM_FLINT: return "FLINT";
        default: return "EMPTY";
    }
}

static bool inventory_add_item(u8 item, u8 count){
    if(item == ITEM_NONE || count == 0) return true;
    int max_stack = item_max_stack(item);
    for(int i=0; i<SURVIVAL_INV_SLOTS && count; i++){
        if(g_inventory[i].item == item && g_inventory[i].count < max_stack){
            int room = max_stack - g_inventory[i].count;
            int give = (count < room) ? count : room;
            g_inventory[i].count += give;
            count -= give;
        }
    }
    for(int i=0; i<SURVIVAL_INV_SLOTS && count; i++){
        if(g_inventory[i].item == ITEM_NONE){
            int give = (count < max_stack) ? count : max_stack;
            g_inventory[i].item = item;
            g_inventory[i].count = give;
            count -= give;
        }
    }
    return count == 0;
}

static int inventory_count_item(u8 item){
    int total = 0;
    for(int i=0; i<SURVIVAL_INV_SLOTS; i++) if(g_inventory[i].item == item) total += g_inventory[i].count;
    return total;
}

static bool inventory_remove_item(u8 item, u8 count){
    if(inventory_count_item(item) < count) return false;
    for(int i=0; i<SURVIVAL_INV_SLOTS && count; i++){
        if(g_inventory[i].item != item) continue;
        if(g_inventory[i].count <= count){
            count -= g_inventory[i].count;
            g_inventory[i].item = ITEM_NONE;
            g_inventory[i].count = 0;
        } else {
            g_inventory[i].count -= count;
            count = 0;
        }
    }
    return true;
}

static void inventory_compact(void){
    int write = 0;
    for(int i=0; i<SURVIVAL_INV_SLOTS; i++){
        if(g_inventory[i].item == ITEM_NONE || g_inventory[i].count == 0) continue;
        if(write != i) g_inventory[write] = g_inventory[i];
        write++;
    }
    for(int i=write; i<SURVIVAL_INV_SLOTS; i++){
        g_inventory[i].item = ITEM_NONE;
        g_inventory[i].count = 0;
    }
}

static void survival_reset_stats(void){
    g_player_max_health = 20;
    g_player_health = g_player_max_health;
    g_player_max_air = 10;
    g_player_air = g_player_max_air;
    g_health_flash_timer = 0;
    g_health_flash_unit = -1;
    g_air_flash_timer = 0;
    g_air_flash_unit = -1;
    g_burn_timer = 0;
}

static void survival_init_inventory(void){
    inventory_clear();
    g_hotbar_slot = 0;
    g_hotbar_prev_slot = 0;
}

static void refresh_selected_item(u8* io_curBlk){
    if(!io_curBlk) return;
    if(g_game_mode == MODE_CREATIVE){
        if(g_hotbar_slot < QUICKBAR_SLOTS) *io_curBlk = g_quickbar[g_hotbar_slot];
        return;
    }
    if(g_hotbar_slot < 0 || g_hotbar_slot >= HOTBAR_SLOTS) g_hotbar_slot = 0;
    *io_curBlk = g_inventory[g_hotbar_slot].item;
}

static void clear_dropped_items(void){
    memset(g_dropped_items, 0, sizeof(g_dropped_items));
    memset(g_break_particles, 0, sizeof(g_break_particles));
    memset(g_tnt_entities, 0, sizeof(g_tnt_entities));
}

static void show_toast(const char* msg);
static void set_creative_flying(bool enabled);

static void store_current_dimension_state(void);


static int g_fps = 0;
static EWRAM_DATA char g_fps_text[8] = "FPS:0";
static u32 g_fps_last_vblank = 0;
static u32 g_fps_frame_count = 0;

static inline void fps_set(int fps){
    if(fps < 0) fps = 0;
    if(fps > 999) fps = 999;
    g_fps = fps;
    g_fps_text[0] = 'F'; g_fps_text[1] = 'P'; g_fps_text[2] = 'S'; g_fps_text[3] = ':';
    if(fps >= 100){
        g_fps_text[4] = (char)('0' + (fps / 100));
        g_fps_text[5] = (char)('0' + ((fps / 10) % 10));
        g_fps_text[6] = (char)('0' + (fps % 10));
        g_fps_text[7] = '\0';
    } else if(fps >= 10){
        g_fps_text[4] = (char)('0' + (fps / 10));
        g_fps_text[5] = (char)('0' + (fps % 10));
        g_fps_text[6] = '\0';
    } else {
        g_fps_text[4] = (char)('0' + fps);
        g_fps_text[5] = '\0';
    }
}

static inline u8 auto_frameskip_for_work_ticks(u32 ticks){
    if(!g_auto_frameskip) return 0;
    if(ticks <= CPU_TIMER_TICKS_PER_FRAME) return 0;
    u32 logic_ticks = g_frameskip_logic_est_ticks;
    if(logic_ticks >= CPU_TIMER_TICKS_PER_FRAME) return AUTO_FRAMESKIP_MAX;
    u32 over_budget = ticks - CPU_TIMER_TICKS_PER_FRAME;
    u32 catchup_ticks = CPU_TIMER_TICKS_PER_FRAME - logic_ticks;
    u32 skip = (over_budget + catchup_ticks - 1u) / catchup_ticks;
    if(skip > AUTO_FRAMESKIP_MAX) skip = AUTO_FRAMESKIP_MAX;
    return (u8)skip;
}

static int g_hand_anim_timer = 0;

static void update_render_dims(void) {
    if(g_render_scale < 1) g_render_scale = 1;
    if(g_render_scale > 4) g_render_scale = 4;
    switch(g_render_scale){
        case 1:  g_rw = 240; g_rh = 160; break;
        case 2:  g_rw = 120; g_rh = 80;  break;
        case 3:  g_rw = 80;  g_rh = 53;  break;
        default: g_rw = 60;  g_rh = 40;  break;
    }
}

