/* ===================== WORLD & SAVE SYSTEM ================= */
#define CW 64
#define CH 50
#define CD 64
#define VW 800
#define VD 800
#define STREAM_PREFETCH_MARGIN 30
#define STREAM_TRIGGER_MARGIN 24
#define STREAM_SETTLE_POS 30
#define STREAM_MAX_SHIFT 12
#define STREAM_BASE_PADDING 0
#define STREAM_DECOR_PADDING 4
#define STREAM_HEAVY_NEAR_DIST 12
#define STREAM_BUDGET_BASE_TICKS   262
#define STREAM_BUDGET_FEATURE_TICKS 24
#define STREAM_BUDGET_LIGHT_TICKS   92
#define STREAM_BUDGET_HEAVY_TICKS   52
#define STREAM_BUDGET_EDIT_TICKS    26
static EWRAM_DATA u8 world[CW*CH*CD];
static int g_world_offset_x = 0;
static int g_world_offset_z = 0;
static int g_world_ring_x = 0;
static int g_world_ring_z = 0;

static inline int wrap_world_x(int x){
    if(x < 0) x += CW;
    else if(x >= CW) x -= CW;
    return x;
}

static inline int wrap_world_z(int z){
    if(z < 0) z += CD;
    else if(z >= CD) z -= CD;
    return z;
}

static inline int phys_x_from_local(int x){
    return wrap_world_x(g_world_ring_x + x);
}

static inline int phys_z_from_local(int z){
    return wrap_world_z(g_world_ring_z + z);
}

static inline int widx(int x,int y,int z){
    if(x < 0 || x >= CW || y < 0 || y >= CH || z < 0 || z >= CD) return CW*CH*CD/2;
    return (y*CD + phys_z_from_local(z))*CW + phys_x_from_local(x);
}

static inline int global_x_from_local(int x){ return g_world_offset_x + x; }
static inline int global_z_from_local(int z){ return g_world_offset_z + z; }
static inline int local_x_from_global(int x){ return x - g_world_offset_x; }
static inline int local_z_from_global(int z){ return z - g_world_offset_z; }
#include "world_fast.h"
typedef struct {
    int x0, x1;
    int z0, z1;
} StreamRect;

typedef struct {
    u8 surface_y;
    u8 nether_roof_y;
    u8 biome;
    u8 top_block;
    u8 flags;
    u8 deco_light;
    u8 deco_heavy;
} ColumnGenDesc;

enum {
    COL_FLAG_OCEAN      = 1 << 0,
    COL_FLAG_SAND_BEACH = 1 << 1,
    COL_FLAG_HIGHLANDS  = 1 << 2,
    COL_FLAG_NETHER     = 1 << 3,
    COL_FLAG_SUPERFLAT  = 1 << 4
};

enum {
    DECO_LIGHT_MOSS   = 1 << 0,
    DECO_LIGHT_CACTUS = 1 << 1
};

enum {
    DECO_HEAVY_TREE      = 1 << 0,
    DECO_HEAVY_LAVA      = 1 << 1,
    DECO_HEAVY_FORTRESS  = 1 << 2
};

typedef enum {
    STREAM_SHIFT = 0,
    STREAM_BASE,
    STREAM_TERRAIN_FEATURES,
    STREAM_DECOR_LIGHT,
    STREAM_EDITS,
    STREAM_DECOR_HEAVY,
    STREAM_IDLE
} StreamPhase;

typedef struct {
    bool active;
    bool has_pending;
    StreamPhase phase;
    int old_off_x, old_off_z;
    int new_off_x, new_off_z;
    int shift_x, shift_z;
    int pending_shift_x, pending_shift_z;
    int prefetch_off_x, prefetch_off_z;
    int rect_count;
    StreamRect rects[2];
    int base_rect_index;
    int base_cursor;
    int feature_rect_index;
    int feature_line_cursor;
    int feature_col_cursor;
    int light_rect_index;
    int light_cursor;
    int heavy_rect_index;
    int heavy_cursor;
    int edit_cursor;
    u16 timer_start;
} WorldStreamState;

static EWRAM_DATA WorldStreamState g_stream_state;
static EWRAM_DATA ColumnGenDesc g_stream_desc[CD];

#define SLOT_SIZE 0x4000
#define SLOT0_OFF 0x0000
#define SLOT1_OFF 0x4000
#define SAVE_VERSION 6
#define LEGACY_SAVE_VERSION 5
#define LEGACY_WORLD_W 400
#define LEGACY_WORLD_D 240
#define LEGACY_WORLD_H 32
#define SLOT_HDR_PAD 128
#define MAX_EDITS  2000
#define DIM_EDIT_AREA_SIZE (MAX_EDITS*4)
#define SLOT_CHKSUM_OFF (SLOT_SIZE - 4)
#define CONTROL_CFG_OFF (SLOT_HDR_PAD + DIM_EDIT_AREA_SIZE * DIMENSION_COUNT)
#define CONTROL_CFG_SIZE (SLOT_CHKSUM_OFF - CONTROL_CFG_OFF)
#define CONTROL_CFG_TAG 0x43464731u
#define CONTROL_CFG_VERSION 1
#define MENU_RNG_CFG_OFF (CONTROL_CFG_OFF + sizeof(ControlConfig))
#define MENU_RNG_CFG_TAG 0x4D524E47u
#define MENU_RNG_CFG_VERSION 1
static const char GBA_SAVE_TYPE[] __attribute__((used)) = "SRAM_V110";
static inline void keep_save_id(void){ __asm__ __volatile__("" :: "r"(GBA_SAVE_TYPE)); }

typedef struct {
    fix posx, posy, posz;
    s16 yaw, pitch;
    u8 valid;
    u8 reserved[3];
} DimPlayerState;

typedef struct {
    u32 tag;
    u32 version;
    u32 seed;
    u8  world_type;
    u8  game_mode;
    u8  allow_mode_switch;
    u8  current_dimension;
    u8  selected_hotbar;
    u8  creative_cur_item;
    u8  health;
    u8  max_health;
    u8  air;
    u8  max_air;
    u16 edit_count[DIMENSION_COUNT];
    u8  creative_quickbar[QUICKBAR_SLOTS];
    u8  reserved0[3];
    InventorySlot inventory[SURVIVAL_INV_SLOTS];
    DimPlayerState dim_state[DIMENSION_COUNT];
} SaveSlotHeader;

static EWRAM_DATA u32 g_edit_log[DIMENSION_COUNT][MAX_EDITS];
static u16 g_edit_count[DIMENSION_COUNT] = {0};
static u32 g_world_seed = 0;
static WorldType g_world_type = WORLD_CLASSIC;
typedef struct { v3 pos; int yaw, pitch; v3 vel; bool onGround; } Player;
static Player pl;
static DimPlayerState g_dim_state[DIMENSION_COUNT];
static u32 g_menu_rng_state = 0xA531D24Bu;

typedef struct {
    u32 tag;
    u16 version;
    u16 reserved;
    u32 rng_state;
    u32 checksum;
} MenuRngConfig;

static void set_creative_flying(bool enabled){
    if(g_creative_flying == enabled) return;
    g_creative_flying = enabled;
    pl.vel.y = 0;
    pl.onGround = false;
    show_toast(enabled ? "FLYING ON" : "FLYING OFF");
}

static void set_spectator_mode(bool enabled, bool* io_hud_state, bool show_message){
    bool* hud_state = io_hud_state ? io_hud_state : &g_show_hud;
    if(enabled){
        if(g_game_mode != MODE_CREATIVE) return;
        if(!g_spectator_mode){
            g_spectator_saved_hud = *hud_state;
            g_spectator_saved_hud_valid = true;
        }
        g_spectator_mode = true;
        g_creative_flying = true;
        *hud_state = false;
        pl.vel.x = 0;
        pl.vel.y = 0;
        pl.vel.z = 0;
        pl.onGround = false;
        if(show_message) show_toast("SPECTATOR ON");
        return;
    }

    if(!g_spectator_mode) return;
    g_spectator_mode = false;
    g_creative_flying = false;
    if(g_spectator_saved_hud_valid) *hud_state = g_spectator_saved_hud;
    g_spectator_saved_hud_valid = false;
    pl.vel.x = 0;
    pl.vel.y = 0;
    pl.vel.z = 0;
    pl.onGround = false;
    if(show_message) show_toast("SPECTATOR OFF");
}

static void store_current_dimension_state(void){
    g_dim_state[g_dimension].posx = pl.pos.x + I2F(g_world_offset_x);
    g_dim_state[g_dimension].posy = pl.pos.y;
    g_dim_state[g_dimension].posz = pl.pos.z + I2F(g_world_offset_z);
    g_dim_state[g_dimension].yaw = (s16)pl.yaw;
    g_dim_state[g_dimension].pitch = (s16)pl.pitch;
    g_dim_state[g_dimension].valid = 1;
}

static inline u32 pack_edit(int x, int y, int z, u8 v){
    return ((u32)(x & 1023))
         | ((u32)(z & 1023) << 10)
         | ((u32)(y & 63) << 20)
         | ((u32)(v & 63) << 26);
}

static inline int edit_x(u32 e){ return (int)(e & 1023u); }
static inline int edit_z(u32 e){ return (int)((e >> 10) & 1023u); }
static inline int edit_y(u32 e){ return (int)((e >> 20) & 63u); }
static inline u8  edit_val(u32 e){ return (u8)((e >> 26) & 63u); }

static inline bool edit_pos_matches(u32 e, int x, int y, int z){
    return edit_x(e) == x && edit_y(e) == y && edit_z(e) == z;
}

static u32 convert_legacy_edit(u32 e){
    int idx = (int)(e & 0x3FFFFFu);
    u8 val = (u8)((e >> 22) & 0xFFu);
    if(idx < 0 || idx >= (LEGACY_WORLD_W * LEGACY_WORLD_D * LEGACY_WORLD_H)) return 0;
    int x = idx % LEGACY_WORLD_W;
    int yz = idx / LEGACY_WORLD_W;
    int z = yz % LEGACY_WORLD_D;
    int y = yz / LEGACY_WORLD_D;
    if(x < 0 || x >= VW || z < 0 || z >= VD || y < 0 || y >= CH) return 0;
    return pack_edit(x, y, z, val);
}

static void editlog_clear_all(void){
    for(int i=0; i<DIMENSION_COUNT; i++) g_edit_count[i] = 0;
}

static void editlog_add_or_update(int x, int y, int z, u8 val){
    u16* count = &g_edit_count[g_dimension];
    for(int i=(int)(*count)-1; i>=0; --i){
        if(edit_pos_matches(g_edit_log[g_dimension][i], x, y, z)){
            g_edit_log[g_dimension][i] = pack_edit(x, y, z, val);
            return;
        }
    }
    if(*count < MAX_EDITS) g_edit_log[g_dimension][(*count)++] = pack_edit(x, y, z, val);
}

static void apply_dimension_edits(int dim){
    if(dim < 0 || dim >= DIMENSION_COUNT) return;
    u16 count = g_edit_count[dim];
    if(count > MAX_EDITS) count = MAX_EDITS;
    for(u16 i=0; i<count; i++){
        u32 e = g_edit_log[dim][i];
        int x = edit_x(e);
        int y = edit_y(e);
        int z = edit_z(e);
        u8 val = edit_val(e);
        int lx = local_x_from_global(x);
        int lz = local_z_from_global(z);
        if(lx >= 0 && lx < CW && y >= 0 && y < CH && lz >= 0 && lz < CD) world[widx(lx,y,lz)] = val;
    }
}

static inline void sram_write(u32 off, const void* src, u32 size){
    const u8* s = (const u8*)src; volatile u8* d = SRAM_BASE + off;
    for(u32 i=0;i<size;i++) d[i] = s[i];
}
static inline void sram_read(u32 off, void* dst, u32 size){
    volatile const u8* s = SRAM_BASE + off; u8* d = (u8*)dst;
    for(u32 i=0;i<size;i++) d[i] = s[i];
}
static u32 checksum32(const void* data, u32 size){
    const u8* p = (const u8*)data; u32 sum = 0;
    for(u32 i=0;i<size;i++) sum = (sum*16777619u) ^ p[i];
    return sum;
}

static void controls_build_config(ControlConfig* cfg){
    memset(cfg, 0, sizeof(*cfg));
    cfg->tag = CONTROL_CFG_TAG;
    cfg->version = CONTROL_CFG_VERSION;
    cfg->preset_index = (s16)input_active_preset();
    memcpy(cfg->bindings, g_action_bindings, sizeof(g_action_bindings));
    cfg->checksum = checksum32(cfg, sizeof(*cfg) - sizeof(cfg->checksum));
}

static bool controls_config_valid(const ControlConfig* cfg){
    if(cfg->tag != CONTROL_CFG_TAG) return false;
    if(cfg->version != CONTROL_CFG_VERSION) return false;
    for(int i=0; i<INPUT_ACTION_COUNT; i++){
        if(cfg->bindings[i].count == 0 || cfg->bindings[i].count > 3) return false;
        for(int j=0; j<cfg->bindings[i].count; j++){
            if(cfg->bindings[i].buttons[j] >= INPUT_BTN_COUNT) return false;
        }
    }
    return cfg->checksum == checksum32(cfg, sizeof(*cfg) - sizeof(cfg->checksum));
}

static bool controls_read_config_slot(int slot, ControlConfig* out_cfg){
    u32 base = (slot == 0) ? SLOT0_OFF : SLOT1_OFF;
    ControlConfig cfg;
    sram_read(base + CONTROL_CFG_OFF, &cfg, sizeof(cfg));
    if(!controls_config_valid(&cfg)) return false;
    if(out_cfg) *out_cfg = cfg;
    return true;
}

static void controls_save_config(void){
    ControlConfig cfg;
    controls_build_config(&cfg);
    sram_write(SLOT0_OFF + CONTROL_CFG_OFF, &cfg, sizeof(cfg));
    sram_write(SLOT1_OFF + CONTROL_CFG_OFF, &cfg, sizeof(cfg));
}

static void controls_load_config(void){
    ControlConfig cfg;
    if(controls_read_config_slot(0, &cfg) || controls_read_config_slot(1, &cfg)){
        input_apply_bindings(cfg.bindings);
    } else {
        input_apply_preset(0);
    }
}

static void menu_rng_build_config(MenuRngConfig* cfg){
    memset(cfg, 0, sizeof(*cfg));
    cfg->tag = MENU_RNG_CFG_TAG;
    cfg->version = MENU_RNG_CFG_VERSION;
    cfg->rng_state = g_menu_rng_state ? g_menu_rng_state : 0xA531D24Bu;
    cfg->checksum = checksum32(cfg, sizeof(*cfg) - sizeof(cfg->checksum));
}

static bool menu_rng_config_valid(const MenuRngConfig* cfg){
    if(cfg->tag != MENU_RNG_CFG_TAG) return false;
    if(cfg->version != MENU_RNG_CFG_VERSION) return false;
    if(cfg->rng_state == 0) return false;
    return cfg->checksum == checksum32(cfg, sizeof(*cfg) - sizeof(cfg->checksum));
}

static bool menu_rng_read_slot(int slot, MenuRngConfig* out_cfg){
    u32 base = (slot == 0) ? SLOT0_OFF : SLOT1_OFF;
    MenuRngConfig cfg;
    sram_read(base + MENU_RNG_CFG_OFF, &cfg, sizeof(cfg));
    if(!menu_rng_config_valid(&cfg)) return false;
    if(out_cfg) *out_cfg = cfg;
    return true;
}

static void menu_rng_save_state(void){
    MenuRngConfig cfg;
    menu_rng_build_config(&cfg);
    sram_write(SLOT0_OFF + MENU_RNG_CFG_OFF, &cfg, sizeof(cfg));
    sram_write(SLOT1_OFF + MENU_RNG_CFG_OFF, &cfg, sizeof(cfg));
}

static void menu_rng_load_state(void){
    MenuRngConfig cfg;
    if(menu_rng_read_slot(0, &cfg) || menu_rng_read_slot(1, &cfg)){
        g_menu_rng_state = cfg.rng_state;
    } else {
        u32 seed = 0xA531D24Bu;
        seed ^= ((u32)REG_TM3CNT_L << 16);
        seed ^= ((u32)REG_VCOUNT << 8);
        g_menu_rng_state = seed ? seed : 0xA531D24Bu;
        menu_rng_save_state();
    }
}

static bool slot_compute_and_compare(u32 base, u32* out_calc, u32* out_stored){
    u8 hdrbuf[SLOT_HDR_PAD]; sram_read(base + 0, hdrbuf, SLOT_HDR_PAD);
    u32 stored = 0; sram_read(base + SLOT_CHKSUM_OFF, &stored, sizeof(stored));
    u32 sum = checksum32(hdrbuf, SLOT_HDR_PAD);
    u8 tmp[64];
    u32 remaining = DIM_EDIT_AREA_SIZE * DIMENSION_COUNT, off = 0;
    while(remaining){
        u32 chunk = (remaining > sizeof(tmp)) ? (u32)sizeof(tmp) : remaining;
        sram_read(base + SLOT_HDR_PAD + off, tmp, chunk);
        sum ^= checksum32(tmp, chunk);
        off += chunk; remaining -= chunk;
    }
    if(out_calc) *out_calc = sum; if(out_stored) *out_stored = stored;
    return (sum == stored);
}
static bool slot_is_valid(int slot, SaveSlotHeader* out_hdr){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    u8 pad[SLOT_HDR_PAD]; sram_read(base + 0, pad, SLOT_HDR_PAD);
    SaveSlotHeader hdr; memcpy(&hdr, pad, sizeof(hdr));
    if(hdr.tag != 0x534C4F54u || (hdr.version != 4 && hdr.version != LEGACY_SAVE_VERSION && hdr.version != SAVE_VERSION)) return false;
    u32 calc, stored;
    if(!slot_compute_and_compare(base, &calc, &stored)) return false;
    if(out_hdr) *out_hdr = hdr;
    return true;
}
static void save_world_to_slot_raw(int slot, u8 curBlk){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    SaveSlotHeader hdr; memset(&hdr, 0, sizeof(hdr));
    hdr.tag = 0x534C4F54u;
    hdr.version = SAVE_VERSION;
    hdr.seed = g_world_seed;
    hdr.world_type = (u8)g_world_type;
    hdr.game_mode = (u8)g_game_mode;
    hdr.allow_mode_switch = g_allow_mode_switch ? 1u : 0u;
    hdr.current_dimension = (u8)g_dimension;
    hdr.selected_hotbar = (u8)g_hotbar_slot;
    hdr.creative_cur_item = curBlk;
    hdr.health = g_player_health;
    hdr.max_health = g_player_max_health;
    hdr.air = g_player_air;
    hdr.max_air = g_player_max_air;
    memcpy(hdr.creative_quickbar, g_quickbar, sizeof(g_quickbar));
    hdr.reserved0[0] = g_spectator_mode ? 1u : 0u;
    memcpy(hdr.inventory, g_inventory, sizeof(g_inventory));
    for(int dim=0; dim<DIMENSION_COUNT; dim++){
        hdr.edit_count[dim] = g_edit_count[dim];
        hdr.dim_state[dim] = g_dim_state[dim];
    }
    u8 pad[SLOT_HDR_PAD]; memset(pad, 0, sizeof(pad)); memcpy(pad, &hdr, sizeof(hdr));
    sram_write(base + 0, pad, SLOT_HDR_PAD);
    u8 zero = 0;
    for(u32 i=0;i<DIM_EDIT_AREA_SIZE * DIMENSION_COUNT;i++) SRAM_BASE[base + SLOT_HDR_PAD + i] = zero;
    for(int dim=0; dim<DIMENSION_COUNT; dim++){
        u16 count = g_edit_count[dim];
        if(count > MAX_EDITS) count = MAX_EDITS;
        if(count){
            volatile u8* d = SRAM_BASE + base + SLOT_HDR_PAD + (dim * DIM_EDIT_AREA_SIZE);
            for(u16 i=0; i<count; i++){
                u32 e = g_edit_log[dim][i];
                u32 o = (u32)i * 4u;
                d[o + 0] = (u8)(e & 0xFFu);
                d[o + 1] = (u8)((e >> 8) & 0xFFu);
                d[o + 2] = (u8)((e >> 16) & 0xFFu);
                d[o + 3] = (u8)((e >> 24) & 0xFFu);
            }
        }
    }
    u8 tmp[64];
    u32 sum = checksum32(pad, SLOT_HDR_PAD);
    u32 remaining = DIM_EDIT_AREA_SIZE * DIMENSION_COUNT, off = 0;
    while(remaining){
        u32 chunk = (remaining > sizeof(tmp)) ? (u32)sizeof(tmp) : remaining;
        sram_read(base + SLOT_HDR_PAD + off, tmp, chunk);
        sum ^= checksum32(tmp, chunk);
        off += chunk; remaining -= chunk;
    }
    sram_write(base + SLOT_CHKSUM_OFF, &sum, sizeof(sum));
}
static bool save_world_and_verify(int slot, u8 curBlk){
    save_world_to_slot_raw(slot, curBlk);
    u32 calc=0, stored=0;
    return slot_compute_and_compare((slot==0)?SLOT0_OFF:SLOT1_OFF, &calc, &stored);
}
static void gen_world(void);
static void progress_screen_begin(ProgressScreenState* state);
static void progress_screen_end(const ProgressScreenState* state);
static void draw_progress_screen(const char* title, const char* status, int percent);
static void stream_center_on_global(int gx, int gz);
static void stream_reset_state(void);
static void stream_maybe_recenter(void);
static bool load_world_from_slot(int slot, u8* io_curBlk){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    ProgressScreenState screen;
    if(!slot_is_valid(slot, NULL)) return false;
    u8 pad[SLOT_HDR_PAD]; sram_read(base + 0, pad, SLOT_HDR_PAD);
    SaveSlotHeader hdr; memcpy(&hdr, pad, sizeof(hdr));
    g_world_seed = hdr.seed;
    g_world_type = (hdr.world_type <= WORLD_HIGHLANDS) ? (WorldType)hdr.world_type : WORLD_CLASSIC;
    g_game_mode = (hdr.game_mode == MODE_SURVIVAL) ? MODE_SURVIVAL : MODE_CREATIVE;
    g_allow_mode_switch = hdr.allow_mode_switch ? true : false;
    g_spectator_mode = false;
    g_spectator_saved_hud_valid = false;
    g_dimension = (hdr.current_dimension < DIMENSION_COUNT) ? hdr.current_dimension : 0;
    g_hotbar_slot = (hdr.selected_hotbar < HOTBAR_SLOTS) ? hdr.selected_hotbar : 0;
    g_hotbar_prev_slot = g_hotbar_slot;
    if(hdr.version == 4){
        g_player_max_health = hdr.max_health ? (u8)(hdr.max_health * 2) : 20;
        g_player_health = (u8)(hdr.health * 2);
    } else {
        g_player_max_health = hdr.max_health ? hdr.max_health : 20;
        g_player_health = hdr.health;
    }
    if(g_player_max_health < 2) g_player_max_health = 20;
    if(g_player_health > g_player_max_health) g_player_health = g_player_max_health;
    g_player_max_air = hdr.max_air ? hdr.max_air : 10;
    g_player_air = hdr.air;
    if(g_player_air > g_player_max_air) g_player_air = g_player_max_air;
    memcpy(g_quickbar, hdr.creative_quickbar, sizeof(g_quickbar));
    memcpy(g_inventory, hdr.inventory, sizeof(g_inventory));
    for(int dim=0; dim<DIMENSION_COUNT; dim++) g_dim_state[dim] = hdr.dim_state[dim];
    g_health_flash_timer = 0;
    g_health_flash_unit = -1;
    g_air_flash_timer = 0;
    g_air_flash_unit = -1;
    g_burn_timer = 0;
    buildPalette();
    progress_screen_begin(&screen);
    draw_progress_screen("LOADING WORLD", "READING SAVE", 8);
    stream_reset_state();
    if(g_dim_state[g_dimension].valid){
        int gx = F2I(g_dim_state[g_dimension].posx);
        int gz = F2I(g_dim_state[g_dimension].posz);
        stream_center_on_global(gx, gz);
    } else {
        stream_center_on_global(VW/2, VD/2);
    }
    draw_progress_screen("LOADING WORLD", "BUILDING WORLD", 24);
    gen_world();
    draw_progress_screen("LOADING WORLD", "READING BLOCKS", 70);
    for(int dim=0; dim<DIMENSION_COUNT; dim++){
        u16 count = hdr.edit_count[dim];
        if(count > MAX_EDITS) count = MAX_EDITS;
        g_edit_count[dim] = count;
        volatile const u8* s = SRAM_BASE + base + SLOT_HDR_PAD + (dim * DIM_EDIT_AREA_SIZE);
        for(u16 i=0; i<count; i++){
            u32 o = (u32)i * 4u;
            u32 raw = (u32)s[o + 0]
                    | ((u32)s[o + 1] << 8)
                    | ((u32)s[o + 2] << 16)
                    | ((u32)s[o + 3] << 24);
            if(hdr.version == SAVE_VERSION) g_edit_log[dim][i] = raw;
            else g_edit_log[dim][i] = convert_legacy_edit(raw);
        }
        for(u16 i=count; i<MAX_EDITS; i++) g_edit_log[dim][i] = 0;
    }
    draw_progress_screen("LOADING WORLD", "RESTORING WORLD", 88);
    apply_dimension_edits(g_dimension);
    if(g_dim_state[g_dimension].valid){
        pl.pos.x = g_dim_state[g_dimension].posx - I2F(g_world_offset_x);
        pl.pos.y = g_dim_state[g_dimension].posy;
        pl.pos.z = g_dim_state[g_dimension].posz - I2F(g_world_offset_z);
        pl.yaw = g_dim_state[g_dimension].yaw;
        pl.pitch = g_dim_state[g_dimension].pitch;
    }
    draw_progress_screen("LOADING WORLD", "SPAWNING PLAYER", 100);
    if(io_curBlk){
        if(g_game_mode == MODE_CREATIVE) *io_curBlk = hdr.creative_cur_item ? hdr.creative_cur_item : *io_curBlk;
        else *io_curBlk = g_inventory[g_hotbar_slot].item;
    }
    if(hdr.reserved0[0] && g_game_mode == MODE_CREATIVE) set_spectator_mode(true, &g_show_hud, false);
    progress_screen_end(&screen);
    return true;
}
static inline void world_set_and_log(int x,int y,int z,u8 val){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return;
    int id = widx(x,y,z);
    world[id] = val;
    editlog_add_or_update(global_x_from_local(x), y, global_z_from_local(z), val);
}

