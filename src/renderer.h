/*

    Renderer header
    Created 12/6/2025 @ 5:57 PM by Digi-Space Productions (EGAMatsu)

*/

#ifndef RENDERER_H_
#define RENDERER_H_

#include "platform.h"

typedef struct { fix x,y,z; } v3;
typedef struct { int x,y; fix u,v; } v2uv;

#include "textures.h"

static inline u16 shadeRGB15(u16 c, int light){
    int r = (c & 31), g = ((c>>5) & 31), b = ((c>>10)&31);
    int num = 4 + light * 2; 
    int den = 16;
    r = (r * num) / den; if(r>31) r=31;
    g = (g * num) / den; if(g>31) g=31;
    b = (b * num) / den; if(b>31) b=31;
    return (u16)(r | (g<<5) | (b<<10));
}

#define SKY_START_IDX 128
#define VOID_START_IDX 160
#define UI_START_IDX 250
#define SKY_COLORS 32

#include "palette.h"
static void generate_textures(void){
    memcpy(textures, texture_data_rom, sizeof(texture_data_rom));
}

#endif