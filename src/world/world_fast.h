#ifndef WORLD_FAST_H
#define WORLD_FAST_H

static inline int world_row_base_phys(int y, int phys_z){
    return (y * CD + phys_z) * CW;
}

static inline u8* world_row_ptr_phys(int y, int phys_z){
    return world + world_row_base_phys(y, phys_z);
}

static inline const u8* world_row_ptr_phys_const(int y, int phys_z){
    return world + world_row_base_phys(y, phys_z);
}

static inline u8* world_row_ptr_local(int y, int z){
    return world_row_ptr_phys(y, phys_z_from_local(z));
}

static inline const u8* world_row_ptr_local_const(int y, int z){
    return world_row_ptr_phys_const(y, phys_z_from_local(z));
}

static inline u8 world_get_local_fast(int x, int y, int z){
    return world_row_ptr_local_const(y, z)[phys_x_from_local(x)];
}

static inline bool block_id_is_solid(u8 id){
    if(!id) return false;
    u8 t = (u8)(id - 1);
    return (t != BLK_WATER &&
            t != BLK_LEAF &&
            t != BLK_LEAF_DARK &&
            t != BLK_LEAF_LIGHT &&
            t != BLK_PORTAL &&
            t != BLK_LAVA);
}

#endif
