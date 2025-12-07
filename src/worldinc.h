/*

    World Include 
    Added 12/6/2025 @ 5:52 PM by Digi-Space Productions (EGAMatsu)

*/

#ifndef WRLD_H_
#define WRLD_H_

typedef struct { 
    bool start_from_load; 
    int  load_slot; 
} MenuResult;

typedef enum { 
    PM_RESUME, 
    PM_TO_MAINMENU 
} PauseResult;

typedef enum { 
    WORLD_CLASSIC, 
    WORLD_SUPERFLAT 
} WorldType;

#endif