/*

    Platform header
    Added 12/6/2025 @ 5:53PM by Digi-Space Productions (EGA Matsu)

*/

#ifndef PLATFORM_H_
#define PLATFORM_H_

extern int g_dimension;

#ifdef PLAT_GBA
    #include "gbainc.h"
#endif

#include "lut.h"
#include "blocks.h"
#include "renderer.h"

#include "image0.c"
#include "image1.c"

#include "worldinc.h"

#endif