/* ===================== INPUT & PHYSICS ===================== */
static struct { bool movement_active; bool rotation_active; } input_state = {false, false};
static volatile u32 g_frame = 0;
static bool g_is_sprinting = false;

static void show_toast(const char* msg);

static inline int scaled_fov_val(void){
    int fov = g_fov_val;
    if(g_render_scale > 1) fov += (g_render_scale - 1) * 30;
    if(g_render_scale == 2){
        if(fov > 200) fov = 200;
    } else {
        if(fov > 150) fov = 150;
    }
    if(fov < 30) fov = 30;
    return fov;
}

static inline fix scaled_focal(void){
    int fov = scaled_fov_val();
    fix focal = F(220 - fov);
    if(g_is_sprinting) focal = fmuli(focal, F(0.94f));
    return focal;
}

static bool cam_basis(v3* fx, v3* fy, v3* fz){
    pl.yaw = pl.yaw & (LUT_N-1);
    int p_min = -(LUT_N>>2) + 5, p_max =  (LUT_N>>2) - 5;
    if(pl.pitch > p_max) pl.pitch = p_max;
    if(pl.pitch < p_min) pl.pitch = p_min;
    
    fix sy = fsin(pl.yaw),  cy = fcos(pl.yaw);
    fix sp = fsin(pl.pitch), cp = fcos(pl.pitch);
    
    if(cp == 0) cp = FONE/100;
    
    fz->x = fmuli(sy, cp);  fz->y = -sp;    fz->z = fmuli(cy, cp);
    fx->x =  cy;            fx->y =  0;     fx->z = -sy;
    fy->x = fmuli(sy, sp);  fy->y =  cp;    fy->z = fmuli(cy, sp);
    return !(fz->x==0 && fz->y==0 && fz->z==0);
}

static ARM_CODE __attribute__((noinline)) void handle_input(void){
    bool spectator = (g_game_mode == MODE_CREATIVE && g_spectator_mode);
    bool creative_flight = spectator || (g_game_mode == MODE_CREATIVE && g_creative_flying);
    bool moving_forward = g_action_state[ACT_FORWARD].held;
    bool moving_back = g_action_state[ACT_BACK].held;
    bool moving_left = g_action_state[ACT_LEFT].held;
    bool moving_right = g_action_state[ACT_RIGHT].held;
    bool turning_left = g_action_state[ACT_TURN_LEFT].held;
    bool turning_right = g_action_state[ACT_TURN_RIGHT].held;
    bool looking_up = g_action_state[ACT_LOOK_UP].held;
    bool looking_down = g_action_state[ACT_LOOK_DOWN].held;
    bool jump_up = g_action_state[ACT_JUMP_FLY_UP].held;
    bool fly_down = g_action_state[ACT_FLY_DOWN].held;
    fix creative_speed = g_is_sprinting ? F(18.0f/60.f) : F(10.0f/60.f);
    fix creative_vertical_speed = g_is_sprinting ? F(18.0f/60.f) : F(10.0f/60.f);

    fix speed = creative_flight ? creative_speed : (g_is_sprinting ? F(13.0f/60.f) : F(6.0f/60.f));
    const fix turn_speed = 5;

    v3 fx, fy, fz;
    if(!cam_basis(&fx, &fy, &fz)) return;
    v3 move_fwd = { -fx.z, 0, fx.x };
    v3 wish = {0,0,0};
    input_state.movement_active = false;
    
    bool has_fwd = false, has_strafe = false;
    if(moving_forward) { wish.x += move_fwd.x; wish.z += move_fwd.z; input_state.movement_active = true; has_fwd = true; }
    if(moving_back)    { wish.x -= move_fwd.x; wish.z -= move_fwd.z; input_state.movement_active = true; has_fwd = true; }
    if(moving_left)    { wish.x -= fx.x; wish.z -= fx.z; input_state.movement_active = true; has_strafe = true; }
    if(moving_right)   { wish.x += fx.x; wish.z += fx.z; input_state.movement_active = true; has_strafe = true; }

    if(has_fwd && has_strafe) {
        const fix INV_SQRT2 = 46340;
        wish.x = fmuli(wish.x, INV_SQRT2);
        wish.z = fmuli(wish.z, INV_SQRT2);
    }

    pl.vel.x = fmuli(wish.x, speed);
    pl.vel.z = fmuli(wish.z, speed);

    if(creative_flight){
        pl.vel.y = 0;
        if(jump_up) pl.vel.y = creative_vertical_speed;
        else if(fly_down) pl.vel.y = -creative_vertical_speed;
    } else if(jump_up && pl.onGround) {
        pl.vel.y = F(22.5f/60.f);
    }

    input_state.rotation_active = false;
    if(turning_left) { pl.yaw = (pl.yaw - turn_speed) & (LUT_N-1); input_state.rotation_active = true; }
    if(turning_right) { pl.yaw = (pl.yaw + turn_speed) & (LUT_N-1); input_state.rotation_active = true; }
    if(looking_up) { pl.pitch -= 3; input_state.rotation_active = true; }
    if(looking_down) { pl.pitch += 3; input_state.rotation_active = true; }
}

static inline bool solid_at(int x,int y,int z){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return false;
    return block_id_is_solid(world_get_local_fast(x, y, z));
}

static void collide_axis(char axis){
    fix r = F(0.3f), h = F(1.6f);
    int minx = F2I(pl.pos.x - r) - 1, maxx = F2I(pl.pos.x + r) + 1;
    int miny = F2I(pl.pos.y) - 1,     maxy = F2I(pl.pos.y + h) + 1;
    int minz = F2I(pl.pos.z - r) - 1, maxz = F2I(pl.pos.z + r) + 1;
    if(minx < 0) minx = 0; if(maxx >= CW) maxx = CW-1;
    if(miny < 0) miny = 0; if(maxy >= CH) maxy = CH-1;
    if(minz < 0) minz = 0; if(maxz >= CD) maxz = CD-1;

    for(int y = miny; y <= maxy; y++){
        for(int z = minz; z <= maxz; z++){
            const u8* row = world_row_ptr_local_const(y, z);
            for(int x = minx; x <= maxx; x++){
                if(!block_id_is_solid(row[phys_x_from_local(x)])) continue;
                fix bx0 = I2F(x), by0 = I2F(y), bz0 = I2F(z);
                fix bx1 = I2F(x+1), by1 = I2F(y+1), bz1 = I2F(z+1);
                fix px0 = pl.pos.x - r, px1 = pl.pos.x + r;
                fix py0 = pl.pos.y,     py1 = pl.pos.y + h;
                fix pz0 = pl.pos.z - r, pz1 = pl.pos.z + r;
                if(px1 <= bx0 || px0 >= bx1 || py1 <= by0 || py0 >= by1 || pz1 <= bz0 || pz0 >= bz1) continue;

                if(axis == 'x'){
                    if(pl.vel.x > 0) pl.pos.x = bx0 - r; else pl.pos.x = bx1 + r;
                    pl.vel.x = 0;
                } else if(axis == 'z'){
                    if(pl.vel.z > 0) pl.pos.z = bz0 - r; else pl.pos.z = bz1 + r;
                    pl.vel.z = 0;
                } else {
                    if(pl.vel.y > 0) pl.pos.y = by0 - h; else { pl.pos.y = by1; pl.onGround = true; }
                    pl.vel.y = 0;
                }
            }
        }
    }
}

#define PHYS_GRAV_FIX (F(135.0f/60.f/60.f))
#define PHYS_MAG_1 (-F(108.0f/60.f))
#define PHYS_MAG_2 (F(15.0f/60.f))

static int g_portal_timer = 0;
static void switch_dimension(u8 curBlk);

static void update_falling_blocks(void){
    if(!g_falling_blocks) return;
    static int fx = 0;
    const int step = 4;
    for(int x = fx; x < fx + step; x++){
        int wx = x;
        if(wx >= CW) wx -= CW;
        for(int z=0; z<CD; z++){
            for(int y=1; y<CH-1; y++){
                u8 id = world[widx(wx, y, z)];
                if(id == BLK_SAND+1 || id == BLK_GRAVEL+1){
                    int below = widx(wx, y-1, z);
                    if(world[below] == 0){
                        world[below] = id;
                        world[widx(wx, y, z)] = 0;
                    }
                }
            }
        }
    }
    fx += step;
    if(fx >= CW) fx = 0;
}

static void physics_step(u8 curBlk){
    int wx = F2I(pl.pos.x);
    int wy_body = F2I(pl.pos.y + F(0.2f));
    int wy_head = F2I(pl.pos.y + F(1.2f));
    int wz = F2I(pl.pos.z);
    bool in_water = false;
    bool head_in_water = false;
    bool in_lava = false;
    if(wx >= 0 && wx < CW && wz >= 0 && wz < CD){
        int phys_x = phys_x_from_local(wx);
        int phys_z = phys_z_from_local(wz);
        if(wy_body >= 0 && wy_body < CH){
            const u8* row = world_row_ptr_phys_const(wy_body, phys_z);
            u8 body = row[phys_x];
            if(body == BLK_WATER+1) in_water = true;
            if(body == BLK_LAVA+1) in_lava = true;
        }
        if(wy_head >= 0 && wy_head < CH){
            const u8* row = world_row_ptr_phys_const(wy_head, phys_z);
            u8 head = row[phys_x];
            if(head == BLK_WATER+1){ in_water = true; head_in_water = true; }
            if(head == BLK_LAVA+1) in_lava = true;
        }
    }
    g_in_water = in_water;
    g_head_in_water = head_in_water;

    if(g_game_mode != MODE_CREATIVE){
        if(g_spectator_mode) set_spectator_mode(false, &g_show_hud, false);
        if(g_creative_flying) g_creative_flying = false;
    }

    pl.onGround = false;
    fix old_vel_y = pl.vel.y;
    if(g_game_mode == MODE_CREATIVE && g_spectator_mode){
        pl.pos.x += pl.vel.x;
        pl.pos.y += pl.vel.y;
        pl.pos.z += pl.vel.z;
    } else if(g_game_mode == MODE_CREATIVE && g_creative_flying){
        // Flight mode keeps vertical velocity under direct input control.
    } else if(in_water){
        pl.vel.y -= (PHYS_GRAV_FIX >> 2);
        if(pl.vel.y < (PHYS_MAG_1 >> 2)) pl.vel.y = (PHYS_MAG_1 >> 2);
        if(g_action_state[ACT_JUMP_FLY_UP].held){
            pl.vel.y += F(18.0f/60.f);
            if(pl.vel.y > F(12.0f/60.f)) pl.vel.y = F(12.0f/60.f);
        }
        pl.vel.x = fmuli(pl.vel.x, F(0.6f));
        pl.vel.z = fmuli(pl.vel.z, F(0.6f));
    } else {
        pl.vel.y -= PHYS_GRAV_FIX;
        if(pl.vel.y < PHYS_MAG_1) pl.vel.y = PHYS_MAG_1;
    }
    
    if(!(g_game_mode == MODE_CREATIVE && g_spectator_mode)){
        pl.pos.x += pl.vel.x; collide_axis('x');
        pl.pos.y += pl.vel.y; collide_axis('y');
        pl.pos.z += pl.vel.z; collide_axis('z');
    }
    if(g_game_mode == MODE_SURVIVAL && pl.onGround && old_vel_y < -F(24.0f/60.f)){
        int dmg = F2I((-old_vel_y) * 60) - 24;
        dmg /= 5;
        if(dmg > 0) survival_damage(dmg);
    }
    
    if(pl.pos.x < F(0.5f)) pl.pos.x = F(0.5f);
    if(pl.pos.x > I2F(CW-1) - F(0.5f)) pl.pos.x = I2F(CW-1) - F(0.5f);
    if(pl.pos.z < F(0.5f)) pl.pos.z = F(0.5f);
    if(pl.pos.z > I2F(CD-1) - F(0.5f)) pl.pos.z = I2F(CD-1) - F(0.5f);
    if(pl.pos.y > I2F(CH-2)) pl.pos.y = I2F(CH-2);
    
    if(pl.vel.x > PHYS_MAG_2)  pl.vel.x = PHYS_MAG_2;
    if(pl.vel.x < -PHYS_MAG_2) pl.vel.x = -PHYS_MAG_2;
    if(pl.vel.z > PHYS_MAG_2)  pl.vel.z = PHYS_MAG_2;
    if(pl.vel.z < -PHYS_MAG_2) pl.vel.z = -PHYS_MAG_2;
    
    if(g_game_mode == MODE_CREATIVE && g_spectator_mode && pl.pos.y < -I2F(10)){
        pl.pos.y = -I2F(10);
    } else if(pl.pos.y < -I2F(10)){
        if(g_game_mode == MODE_SURVIVAL) g_player_health = 0;
        else {
            pl.pos.x = I2F(CW>>1); pl.pos.y = I2F(25); pl.pos.z = I2F(CD>>1);
            pl.vel.x = pl.vel.y = pl.vel.z = 0;
        }
    }

    if(g_game_mode == MODE_SURVIVAL){
        if(head_in_water){
            static int air_tick = 0;
            air_tick++;
            if(air_tick >= 20){
                air_tick = 0;
                if(g_player_air > 0) set_player_air_units((int)g_player_air - 1);
                else survival_damage(1);
            }
        } else {
            static int air_recover = 0;
            air_recover++;
            if(air_recover >= 6){
                air_recover = 0;
                if(g_player_air < g_player_max_air) set_player_air_units((int)g_player_air + 1);
            }
        }
        if(in_lava){
            g_burn_timer = 24;
            if(g_frame % 8 == 0) survival_damage(1);
        }
    }
    
    int px = F2I(pl.pos.x), py = F2I(pl.pos.y + F(0.5f)), pz = F2I(pl.pos.z);
    u8 id = 0;
    if(px >= 0 && px < CW && pz >= 0 && pz < CD){
        int phys_x = phys_x_from_local(px);
        int phys_z = phys_z_from_local(pz);
        if(py >= 0 && py < CH) id = world_row_ptr_phys_const(py, phys_z)[phys_x];
        if(id != BLK_PORTAL + 1){
            int py0 = F2I(pl.pos.y);
            if(py0 >= 0 && py0 < CH) id = world_row_ptr_phys_const(py0, phys_z)[phys_x];
        }
    }
    if(id == BLK_PORTAL + 1) {
        if(++g_portal_timer > 90) { switch_dimension(curBlk); g_portal_timer = 0; }
    } else g_portal_timer = 0;
}

