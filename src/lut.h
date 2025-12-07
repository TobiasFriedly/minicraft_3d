/*

    LUT header
    Made 12/6/2025 @ 6:00 PM by Digi-Space Productions

*/

#ifndef LUT_H_
#define LUT_H_

#include "platform.h"

#define LUT_N 512
static EWRAM_DATA fix sinLUT[LUT_N];
static inline fix fsin(int a){ return sinLUT[a & (LUT_N-1)]; }
static inline fix fcos(int a){ return sinLUT[(a + (LUT_N>>2)) & (LUT_N-1)]; }

#define INVZ_LUT_SIZE 256
#define INVZ_NEAR_FIX F(0.25f)
#define INVZ_FAR_FIX  F(40.0f)
static IWRAM_DATA fix invz_lut[INVZ_LUT_SIZE];
static IWRAM_DATA fix reciprocal_lut[256];

static void build_luts(void){
    for(int i=0; i<LUT_N; i++){
        sinLUT[i] = F(sinf((float)i * 2.0f * 3.14159265f / (float)LUT_N));
    }
    for(int i=0; i<INVZ_LUT_SIZE; i++){
        fix z = INVZ_NEAR_FIX + ((INVZ_FAR_FIX - INVZ_NEAR_FIX) * i) / INVZ_LUT_SIZE;
        invz_lut[i] = fdivi(FONE, z);
    }
    reciprocal_lut[0] = FONE;
    for(int i=1;i<256;i++) reciprocal_lut[i] = fdivi(FONE, I2F(i));
}

static IWRAM_CODE fix fast_invz(fix z){
    if(z <= INVZ_NEAR_FIX) return invz_lut[0];
    if(z >= INVZ_FAR_FIX)  return invz_lut[INVZ_LUT_SIZE-1];
    fix range = INVZ_FAR_FIX - INVZ_NEAR_FIX;
    fix normalized = fdivi(z - INVZ_NEAR_FIX, range);
    int idx = F2I(fmuli(normalized, I2F(INVZ_LUT_SIZE-1)));
    if(idx < 0) idx = 0;
    if(idx >= INVZ_LUT_SIZE-1) idx = INVZ_LUT_SIZE-1;
    return invz_lut[idx];
}

#endif