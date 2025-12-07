/*

    GBA Inlcude File
    Added 12/6/2025  @ 5:51PM by Digi-Space Productions (EGAMatsu)

*/

#ifndef GBAINC_
#define GBAINC_

#include <gba.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>

extern const u8 image0[];
extern const u8 image1[];


typedef uint8_t  u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int32_t  fix;

/* GBA RAM and Page structure */
#undef IWRAM_CODE
#define IWRAM_CODE __attribute__((section(".iwram"), noinline))
#define ARM_CODE   __attribute__((target("arm")))

#ifndef EWRAM_DATA
#define EWRAM_DATA __attribute__((section(".ewram")))
#endif
#ifndef IWRAM_DATA
#define IWRAM_DATA __attribute__((section(".iwram")))
#endif

#define W 240
#define H 160

#define VRAM_PAGE0 ((volatile u8*)0x06000000)
#define VRAM_PAGE1 ((volatile u8*)0x0600A000)
#define PAL_BG_MEM ((volatile u16*)0x05000000)


#define PRECISION 16
#define FONE   ((fix)(1 << PRECISION))
#define F(x)   ((fix)((x) * (1 << PRECISION)))
#define I2F(i) ((fix)((i) << PRECISION))
#define F2I(x) ((int)((x) >> PRECISION))

/* GBA RAM/SRAM */
#define SRAM_BASE ((volatile u8*)0x0E000000)
#define SLOT_SIZE 0x3C00       
#define SLOT0_OFF 0x0000
#define SLOT1_OFF 0x3C00       
#define SLOT_HDR_PAD 64
#define MAX_EDITS  3776        
#define EDIT_AREA_SIZE (MAX_EDITS*4)  
#define SLOT_CHKSUM_OFF (SLOT_SIZE - 4)

/* Math, video and save management */
static inline fix fmuli(fix a, fix b){ return (fix)(((int32_t)(a>>8)*(b>>8))); }

static inline fix fdivi(fix a, fix b){
    if(b == 0) return (a < 0) ? -FONE*1000 : FONE*1000;
    return (fix)(Div((int32_t)a<<8, b)<<8);
}


/* Define save type */
static const char GBA_SAVE_TYPE[] __attribute__((used)) = "SRAM_V110";
static inline void keep_save_id(void){ __asm__ __volatile__("" :: "r"(GBA_SAVE_TYPE)); }

/* Page management*/
static volatile u8* backbuffer  = VRAM_PAGE1;
static inline void vblank(void) { VBlankIntrWait(); }
static inline void flip(void) {
    REG_DISPCNT ^= BACKBUFFER;
    backbuffer = (volatile u8*)((REG_DISPCNT & BACKBUFFER) ? VRAM_PAGE0 : VRAM_PAGE1);
}

/* Math */
static inline fix flerp(fix a, fix b, fix t){ return a + fmuli((b-a), t); }
static inline fix fixabs(fix x){ return (x < 0) ? -x : x; }

fix IWRAM_CODE ARM_CODE fsqrt(fix n) {
    if (n <= 0) return 0;
    fix root = n, last;
    int iter = 0;
    do { 
        last = root; 
        root = (root + fdivi(n, root)) >> 1; 
        iter++;
    } while (root != last && iter < 20);
    return root;
}

#endif