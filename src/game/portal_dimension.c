static void try_ignite_portal_rect(int x, int y, int z) {
    if(world[widx(x,y,z)] != (BLK_OBSIDIAN+1)) return;
    int py = y + 1;
    if(world[widx(x,py,z)] != 0) return;
    
    int min_x = x, max_x = x;
    while(min_x > 0 && world[widx(min_x-1, py, z)] == 0) min_x--;
    while(max_x < CW-1 && world[widx(max_x+1, py, z)] == 0) max_x++;
    
    int min_y = py, max_y = py;
    while(max_y < CH-1) {
        bool row_valid = true;
        for(int ix=min_x; ix<=max_x; ix++) if(world[widx(ix, max_y+1, z)] != 0) row_valid = false;
        if(row_valid) max_y++; else break;
    }
    int w = max_x - min_x + 1, h = max_y - min_y + 1;
    if(w >= 2 && h >= 3) {
        bool ok = true;
        for(int ix=min_x; ix<=max_x; ix++) {
            if(world[widx(ix, min_y-1, z)] != (BLK_OBSIDIAN+1)) ok=false;
            if(world[widx(ix, max_y+1, z)] != (BLK_OBSIDIAN+1)) ok=false;
        }
        for(int iy=min_y; iy<=max_y; iy++) {
             if(world[widx(min_x-1, iy, z)] != (BLK_OBSIDIAN+1)) ok=false;
             if(world[widx(max_x+1, iy, z)] != (BLK_OBSIDIAN+1)) ok=false;
        }
        if(ok) {
            for(int iy=min_y; iy<=max_y; iy++) for(int ix=min_x; ix<=max_x; ix++) world_set_and_log(ix, iy, z, BLK_PORTAL+1);
            return;
        }
    }
    int min_z = z, max_z = z;
    while(min_z > 0 && world[widx(x, py, min_z-1)] == 0) min_z--;
    while(max_z < CD-1 && world[widx(x, py, max_z+1)] == 0) max_z++;
    
    min_y = py; max_y = py;
    while(max_y < CH-1) {
        bool row_valid = true;
        for(int iz=min_z; iz<=max_z; iz++) if(world[widx(x, max_y+1, iz)] != 0) row_valid = false;
        if(row_valid) max_y++; else break;
    }
    w = max_z - min_z + 1; h = max_y - min_y + 1;
    if(w >= 2 && h >= 3) {
         bool ok = true;
         for(int iz=min_z; iz<=max_z; iz++) {
            if(world[widx(x, min_y-1, iz)] != (BLK_OBSIDIAN+1)) ok=false;
            if(world[widx(x, max_y+1, iz)] != (BLK_OBSIDIAN+1)) ok=false;
         }
         for(int iy=min_y; iy<=max_y; iy++) {
             if(world[widx(x, iy, min_z-1)] != (BLK_OBSIDIAN+1)) ok=false;
             if(world[widx(x, iy, max_z+1)] != (BLK_OBSIDIAN+1)) ok=false;
         }
         if(ok) {
            for(int iy=min_y; iy<=max_y; iy++) for(int iz=min_z; iz<=max_z; iz++) world_set_and_log(x, iy, iz, BLK_PORTAL+1);
            return;
         }
    }
}

static void use_flint_and_steel_at(int x, int y, int z){
    if(ignite_tnt_block_with_fuse(x, y, z, 96)) return;
    try_ignite_portal_rect(x, y, z);
}

static void build_portal_structure(int x, int y, int z) {
    for(int dx=-2; dx<=3; dx++) {
        for(int dz=-2; dz<=2; dz++) {
            int id = world[widx(x+dx, y-1, z+dz)];
            if (id == 0 || id == (BLK_LAVA+1) || id == (BLK_WATER+1)) {
                world_set_and_log(x+dx, y-1, z+dz, g_dimension == 1 ? (BLK_NETHERRACK+1) : (BLK_STONE+1));
            }
            for(int h=0; h<4; h++) world_set_and_log(x+dx, y+h, z+dz, 0);
        }
    }
    world_set_and_log(x, y-1, z, BLK_OBSIDIAN+1); world_set_and_log(x+1, y-1, z, BLK_OBSIDIAN+1);
    world_set_and_log(x, y+3, z, BLK_OBSIDIAN+1); world_set_and_log(x+1, y+3, z, BLK_OBSIDIAN+1);
    for(int h=0; h<3; h++) { world_set_and_log(x-1, y+h, z, BLK_OBSIDIAN+1); world_set_and_log(x+2, y+h, z, BLK_OBSIDIAN+1); }
    for(int h=0; h<3; h++) { world_set_and_log(x, y+h, z, BLK_PORTAL+1); world_set_and_log(x+1, y+h, z, BLK_PORTAL+1); }
}

static void switch_dimension(u8 curBlk) {
    (void)curBlk;
    show_toast(g_dimension == 0 ? "ENTERING NETHER..." : "RETURNING...");
    render_textured(BLK_PORTAL+1, (RayHit){0}); present_and_flip();
    for(int i=0;i<60;i++) vblank();
    store_current_dimension_state();
    clear_dropped_items();
    g_creative_flying = false;
    g_jump_tap_timer = 0;
    int target_dim = (g_dimension == 0) ? 1 : 0;
    g_dimension = target_dim;
    buildPalette();
    stream_reset_state();
    if(g_dim_state[g_dimension].valid){
        stream_center_on_global(F2I(g_dim_state[g_dimension].posx), F2I(g_dim_state[g_dimension].posz));
    } else {
        stream_center_on_global(VW/2, VD/2);
    }
    gen_world();
    apply_dimension_edits(g_dimension);
    if (!g_dim_state[g_dimension].valid) {
        int spawn_x = CW/2, spawn_z = CD/2, spawn_y = CH-1;
        if (g_dimension == 1) {
             spawn_y = CH/2;
             while(spawn_y > 5 && world[widx(spawn_x, spawn_y-1, spawn_z)] == 0) spawn_y--;
             if(world[widx(spawn_x, spawn_y, spawn_z)] != 0) spawn_y = 16;
        } else {
            if(g_world_type == WORLD_SUPERFLAT){ spawn_y=11; }
            else if(!find_overworld_spawn(&spawn_x, &spawn_y, &spawn_z)){
                for(int y=CH-1;y>=0;y--){
                    u8 id = world[widx(spawn_x,y,spawn_z)];
                    if(id){ u8 t = id-1; if(t!=BLK_WATER && t!=BLK_LEAF && t!=BLK_LEAF_DARK && t!=BLK_LEAF_LIGHT && t!=BLK_LAVA){ spawn_y=y+1; break; } }
                }
                if(spawn_y < 10) spawn_y=25;
            }
        }
        build_portal_structure(spawn_x, spawn_y, spawn_z);
        pl.pos.x=I2F(spawn_x) + F(0.5f); pl.pos.y=I2F(spawn_y); pl.pos.z=I2F(spawn_z) + F(2.0f);
        pl.vel.x=0; pl.vel.y=0; pl.vel.z=0;
        pl.yaw=LUT_N/2; pl.pitch=0; pl.onGround=false;
        store_current_dimension_state();
    } else {
        pl.pos.x = g_dim_state[g_dimension].posx - I2F(g_world_offset_x);
        pl.pos.y = g_dim_state[g_dimension].posy;
        pl.pos.z = g_dim_state[g_dimension].posz - I2F(g_world_offset_z);
        pl.yaw = g_dim_state[g_dimension].yaw;
        pl.pitch = g_dim_state[g_dimension].pitch;
        pl.vel.x = pl.vel.y = pl.vel.z = 0;
    }
}
