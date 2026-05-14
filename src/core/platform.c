#include <gba.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

#include "../assets/image0.c"
#include "../assets/image1.c"

extern const u8 image0[];
extern const u8 image1[];

/* ===================== ATTRIBUTES & SECTIONS ================== */
#ifndef IWRAM_FN
#define IWRAM_FN   __attribute__((section(".iwram")))
#endif
#ifndef IWRAM_CODE
#define IWRAM_CODE IWRAM_FN
#endif
#ifndef ARM_CODE
#define ARM_CODE   __attribute__((target("arm")))
#endif
#ifndef EWRAM_DATA
#define EWRAM_DATA __attribute__((section(".ewram")))
#endif
#ifndef IWRAM_DATA
#define IWRAM_DATA __attribute__((section(".iwram")))
#endif
#ifndef HOT_INLINE
#define HOT_INLINE static inline __attribute__((always_inline))
#endif

#ifndef PERF_PROFILE
#define PERF_PROFILE 0
#endif
#ifndef PERF_OVERLAY
#define PERF_OVERLAY 0
#endif
#ifndef PERF_AUTOSTART
#define PERF_AUTOSTART 0
#endif
#ifndef PERF_HALT_AFTER_WORLDGEN
#define PERF_HALT_AFTER_WORLDGEN 0
#endif
#ifndef PERF_WORLD_SEED
#define PERF_WORLD_SEED 0x1A2B3C4Du
#endif
#ifndef PERF_WORLD_TYPE
#define PERF_WORLD_TYPE 0
#endif
#ifndef PERF_RENDER_SCALE
#define PERF_RENDER_SCALE 2
#endif
#ifndef PERF_VIS_DIST
#define PERF_VIS_DIST 9
#endif
#ifndef PERF_FACE_LIMIT
#define PERF_FACE_LIMIT 1600
#endif

/* ===================== TYPEDEFS & DEFINES ================== */
typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t  fix;

typedef enum {
    MODE_SURVIVAL,
    MODE_CREATIVE
} GameMode;

typedef enum {
    INPUT_BTN_A,
    INPUT_BTN_B,
    INPUT_BTN_SELECT,
    INPUT_BTN_START,
    INPUT_BTN_RIGHT,
    INPUT_BTN_LEFT,
    INPUT_BTN_UP,
    INPUT_BTN_DOWN,
    INPUT_BTN_R,
    INPUT_BTN_L,
    INPUT_BTN_COUNT
} InputButton;

typedef enum {
    ACT_FORWARD,
    ACT_BACK,
    ACT_LEFT,
    ACT_RIGHT,
    ACT_TURN_LEFT,
    ACT_TURN_RIGHT,
    ACT_LOOK_UP,
    ACT_LOOK_DOWN,
    ACT_JUMP_FLY_UP,
    ACT_FLY_DOWN,
    ACT_BREAK,
    ACT_PLACE_USE,
    ACT_SPRINT,
    ACT_INVENTORY,
    ACT_HOTBAR_LEFT,
    ACT_HOTBAR_RIGHT,
    ACT_PAUSE,
    ACT_RESET_GRAPHICS,
    INPUT_ACTION_COUNT
} InputAction;

typedef struct {
    u8 count;
    u8 buttons[3];
} InputChord;

typedef struct {
    const char* name;
    InputChord bindings[INPUT_ACTION_COUNT];
} ControlPreset;

typedef struct {
    u32 tag;
    u16 version;
    s16 preset_index;
    InputChord bindings[INPUT_ACTION_COUNT];
    u32 checksum;
} ControlConfig;

typedef struct {
    bool held;
    bool pressed;
} ActionState;

#define INPUT_CHORD1(a)       {1, {(a), 0, 0}}
#define INPUT_CHORD2(a,b)     {2, {(a), (b), 0}}
#define INPUT_CHORD3(a,b,c)   {3, {(a), (b), (c)}}

typedef struct { 
    bool start_from_load; 
    int  load_slot;
    GameMode game_mode;
    bool allow_mode_switch;
} MenuResult;

typedef enum { 
    PM_RESUME, 
    PM_TO_MAINMENU 
} PauseResult;

typedef enum { 
    WORLD_CLASSIC, 
    WORLD_SUPERFLAT,
    WORLD_HIGHLANDS
} WorldType;

typedef struct {
    u8 item;
    u8 count;
} InventorySlot;

typedef struct {
    bool hit;
    int x, y, z;
    int face; // 0=PX, 1=NX, 2=PY, 3=NY, 4=PZ, 5=NZ
} RayHit;

#define SCREEN_W 240
#define SCREEN_H 160

static int g_rw = 240;
static int g_rh = 160;
static int g_last_hw_scale = -1;

#define VRAM_PAGE0 ((volatile u8*)0x06000000)
#define VRAM_PAGE1 ((volatile u8*)0x0600A000)
#define PAL_BG_MEM ((volatile u16*)0x05000000)

/* ===================== FIXED POINT MATH ================== */
#define PRECISION 16
#define FONE   ((fix)(1 << PRECISION))
#define F(x)   ((fix)((x) * (1 << PRECISION)))
#define I2F(i) ((fix)((i) << PRECISION))
#define F2I(x) ((int)((x) >> PRECISION))

HOT_INLINE fix fmuli(fix a, fix b){
    int32_t low;
    fix res;
    __asm__ volatile (
        "smull %0, %1, %2, %3 \n\t"
        "mov   %1, %1, lsl #16 \n\t"
        "orr   %1, %1, %0, lsr #16 \n\t"
        : "=&r"(low), "=&r"(res)
        : "r"(a), "r"(b)
        : "cc"
    );
    return res;
}

static inline fix fdivi(fix a, fix b){
    if(b == 0) return (a < 0) ? -FONE*1000 : FONE*1000;
    return (fix)(Div((int32_t)a<<8, b)<<8);
}

static const u8 tex_light_lut[4][8] = {
    {0,1,2,3,4,5,6,7},
    {1,2,3,4,5,6,7,7},
    {2,3,4,5,6,7,7,7},
    {3,4,5,6,7,7,7,7},
};

static inline fix fixabs(fix x){
    fix m = x >> 31;
    return (x ^ m) - m;
}

/* ===================== DISPLAY HELPERS ===================== */
static u8* backbuffer = (u8*)VRAM_PAGE0;
static inline void vblank(void) { VBlankIntrWait(); }

static volatile u32 g_vblank_count = 0;
static void on_vblank(void) {
    g_vblank_count++;
}

static inline u8* vram_backbuffer(void){
    return (u8*)((REG_DISPCNT & BACKBUFFER) ? VRAM_PAGE0 : VRAM_PAGE1);
}

#define CPU_TIMER_CYCLES_PER_TICK 64u
#define CPU_TIMER_TICKS_PER_FRAME 4389u
#define AUTO_FRAMESKIP_MAX 5u

static inline u16 frame_timer_now(void){ return REG_TM3CNT_L; }
static inline u16 frame_timer_elapsed(u16 start){ return (u16)(REG_TM3CNT_L - start); }
static EWRAM_DATA u32 g_frameskip_logic_est_ticks = 1990;

#if PERF_PROFILE
#define PROF_CYCLES_PER_TICK CPU_TIMER_CYCLES_PER_TICK
static EWRAM_DATA volatile u32 g_prof_frame_work_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_frame_work_cycles = 0;
static EWRAM_DATA volatile u32 g_prof_logic_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_sky_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_collect_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_draw_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_post_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_render_hud_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_total_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_terrain_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_detail_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_light_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_decor_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_detail_max_line_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_gen_detail_columns = 0;
static EWRAM_DATA volatile u32 g_prof_gen_runs = 0;
static EWRAM_DATA volatile u32 g_prof_frameskip_pending = 0;
static EWRAM_DATA volatile u32 g_prof_frameskip_logic_ticks = 0;
static EWRAM_DATA volatile u32 g_prof_break_marker = 0;

static inline u16 prof_now(void){ return frame_timer_now(); }
static inline u16 prof_elapsed(u16 start){ return frame_timer_elapsed(start); }
static inline void prof_record_max(volatile u32* dst, u32 value){
    if(value > *dst) *dst = value;
}

void ARM_CODE perf_debug_trap(void){
    g_prof_break_marker++;
    __asm__ volatile("");
}
#endif

static void set_hardware_scale(int scale) {
 
    int aff_step = 256 / scale; 
    
    REG_BG2PA = aff_step; 
    REG_BG2PB = 0;        
    REG_BG2PC = 0;        
    REG_BG2PD = aff_step; 

    REG_BG2X = 0;
    REG_BG2Y = 0;
    g_last_hw_scale = scale;
}

static inline void present_and_flip(void){
    vblank();
    REG_DISPCNT ^= BACKBUFFER;
    backbuffer = vram_backbuffer();
}

static void apply_hardware_scale(int scale){
    if(scale != g_last_hw_scale){
        set_hardware_scale(scale);
        g_last_hw_scale = scale;
    }
}

