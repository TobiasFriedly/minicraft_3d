/*

    Palette header
    Made 12/6/2025 @ 6:06 PM by Digi-Space Productions (EGAMatsu)

*/

#ifndef PALETTE_H_
#define PALETTE_H_

#include "platform.h"

static void buildPalette(void){
    for(int b=0; b<BLK_COUNT; b++){
        for(int s=0; s<SHADES_PER_BLOCK; s++){
        if (b == BLK_GRASS_SIDE) {
                if (s <= 4) {
                    PAL_BG_MEM[b*8 + s] = shadeRGB15(RGB5(14, 9, 4), s*2 + 2); 
                } else {
                    PAL_BG_MEM[b*8 + s] = shadeRGB15(RGB5(3, 13, 3), (s - 5) * 4 + 4);
                }
            } else if (b == BLK_PORTAL) {
                int bright = 12 + s*2; if(bright>31) bright=31;
                PAL_BG_MEM[b*8 + s] = RGB5(bright, 6, bright); 
            } else if (b == BLK_LAVA) {
                if (s <= 2) {
                    PAL_BG_MEM[b*8 + s] = RGB5(25 + s*2, 4 + s*2, 0); 
                } else {
                    int r = 31;
                    int g = 12 + (s-3)*4; if(g>31) g=31;
                    PAL_BG_MEM[b*8 + s] = RGB5(r,g,0);
                }
            } else {
                PAL_BG_MEM[b*8 + s] = shadeRGB15(palBaseColors[b], s*2);
            }
        }
    }
    
    if (g_dimension == 0) {
        for(int i=0; i<SKY_COLORS; i++) {
            int r = 31 - (21 * i) / SKY_COLORS;
            int g = 31 - (13 * i) / SKY_COLORS;
            int b = 31;
            PAL_BG_MEM[SKY_START_IDX + i] = RGB5(r,g,b);
        }
        for(int i=0; i<SKY_COLORS; i++) {
            int val = 31 - (26 * i) / SKY_COLORS;
            PAL_BG_MEM[VOID_START_IDX + i] = RGB5(val, val, val);
        }
    } else {
        for(int i=0; i<SKY_COLORS; i++) {
            int r = 16 - (8 * i) / SKY_COLORS;
            PAL_BG_MEM[SKY_START_IDX + i] = RGB5(r,3,3);
        }
        for(int i=0; i<SKY_COLORS; i++) {
             int r = 10 - (8 * i) / SKY_COLORS;
             PAL_BG_MEM[VOID_START_IDX + i] = RGB5(r, 0, 0);
        }
    }
    PAL_BG_MEM[255] = RGB5(31,31,31); 
    PAL_BG_MEM[254] = RGB5(31,31,0);  
    PAL_BG_MEM[253] = RGB5(15,15,15); 
    PAL_BG_MEM[252] = RGB5(8,8,8);    
    PAL_BG_MEM[251] = RGB5(20,0,0);
    PAL_BG_MEM[250] = RGB5(20,20,20); 
}

#endif