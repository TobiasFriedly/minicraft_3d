static RayHit get_target_block(void) {
    v3 fwd; {
        fix sy = fsin(pl.yaw), cy = fcos(pl.yaw);
        fix sp = fsin(pl.pitch), cp = fcos(pl.pitch);
        fwd.x = fmuli(sy, cp); fwd.y = -sp; fwd.z = fmuli(cy, cp);
        if(fwd.x==0 && fwd.y==0 && fwd.z==0) fwd.z=FONE;
    }
    v3 pos = { pl.pos.x, pl.pos.y + F(1.5f), pl.pos.z };
    int bx = F2I(pos.x), by = F2I(pos.y), bz = F2I(pos.z);
    int step_x = (fwd.x > 0) ? 1 : ((fwd.x < 0) ? -1 : 0);
    int step_y = (fwd.y > 0) ? 1 : ((fwd.y < 0) ? -1 : 0);
    int step_z = (fwd.z > 0) ? 1 : ((fwd.z < 0) ? -1 : 0);
    const fix ray_inf = F(1000.0f);
    fix abs_x = fixabs(fwd.x);
    fix abs_y = fixabs(fwd.y);
    fix abs_z = fixabs(fwd.z);
    fix dist_x = ray_inf;
    fix dist_y = ray_inf;
    fix dist_z = ray_inf;
    if(step_x > 0) dist_x = I2F(bx + 1) - pos.x;
    else if(step_x < 0) dist_x = pos.x - I2F(bx);
    if(step_y > 0) dist_y = I2F(by + 1) - pos.y;
    else if(step_y < 0) dist_y = pos.y - I2F(by);
    if(step_z > 0) dist_z = I2F(bz + 1) - pos.z;
    else if(step_z < 0) dist_z = pos.z - I2F(bz);
    fix max_t = (fix)(((g_vis_dist + 1) * 12 * FONE) / 10);
    fix max_dist_x = step_x ? fmuli(max_t, abs_x) : 0;
    fix max_dist_y = step_y ? fmuli(max_t, abs_y) : 0;
    fix max_dist_z = step_z ? fmuli(max_t, abs_z) : 0;
    int cached_y = -1, cached_z = -1;
    const u8* cached_row = NULL;

    while(1){
        int face;
        if(step_x &&
           (!step_y || fmuli(dist_x, abs_y) <= fmuli(dist_y, abs_x)) &&
           (!step_z || fmuli(dist_x, abs_z) <= fmuli(dist_z, abs_x))){
            if(dist_x > max_dist_x) break;
            bx += step_x;
            dist_x += FONE;
            face = (step_x > 0) ? FACE_NEGX : FACE_POSX;
        } else if(step_y &&
                  (!step_z || fmuli(dist_y, abs_z) <= fmuli(dist_z, abs_y))){
            if(dist_y > max_dist_y) break;
            by += step_y;
            dist_y += FONE;
            face = (step_y > 0) ? FACE_NEGY : FACE_POSY;
        } else {
            if(!step_z || dist_z > max_dist_z) break;
            bz += step_z;
            dist_z += FONE;
            face = (step_z > 0) ? FACE_NEGZ : FACE_POSZ;
        }

        if(bx<0||bx>=CW||by<0||by>=CH||bz<0||bz>=CD) return (RayHit){false,0,0,0,0};
        if(by != cached_y || bz != cached_z){
            cached_row = world_row_ptr_local_const(by, bz);
            cached_y = by;
            cached_z = bz;
        }
        if(cached_row[phys_x_from_local(bx)]) return (RayHit){true, bx, by, bz, face};
    }
    return (RayHit){false,0,0,0,0};
}

static void draw_selection_outline(RayHit hit, const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos) {
    if(!hit.hit) return;
    fix x = I2F(hit.x), y = I2F(hit.y), z = I2F(hit.z);
    fix x1 = x + FONE, y1 = y + FONE, z1 = z + FONE;
    v3 corners[4];
    switch(hit.face) {
        case FACE_POSX: corners[0]=(v3){x1,y,z}; corners[1]=(v3){x1,y1,z}; corners[2]=(v3){x1,y1,z1}; corners[3]=(v3){x1,y,z1}; break;
        case FACE_NEGX: corners[0]=(v3){x,y,z}; corners[1]=(v3){x,y1,z}; corners[2]=(v3){x,y1,z1}; corners[3]=(v3){x,y,z1}; break;
        case FACE_POSY: corners[0]=(v3){x,y1,z}; corners[1]=(v3){x1,y1,z}; corners[2]=(v3){x1,y1,z1}; corners[3]=(v3){x,y1,z1}; break;
        case FACE_NEGY: corners[0]=(v3){x,y,z}; corners[1]=(v3){x1,y,z}; corners[2]=(v3){x1,y,z1}; corners[3]=(v3){x,y,z1}; break;
        case FACE_POSZ: corners[0]=(v3){x,y,z1}; corners[1]=(v3){x1,y,z1}; corners[2]=(v3){x1,y1,z1}; corners[3]=(v3){x,y1,z1}; break;
        case FACE_NEGZ: corners[0]=(v3){x,y,z}; corners[1]=(v3){x1,y,z}; corners[2]=(v3){x1,y1,z}; corners[3]=(v3){x,y1,z}; break;
        default: return;
    }
    int sx[4], sy[4];
    for(int i=0; i<4; i++) {
        v3 pc = world_to_cam(corners[i].x, corners[i].y, corners[i].z, fx, fy, fz, cam_pos);
        if(!cam_to_screen(&pc, &sx[i], &sy[i])) return; 
    }
    for(int i=0; i<4; i++) draw_line_clipped(sx[i], sy[i], sx[(i+1)%4], sy[(i+1)%4], 251);
}

#if PERF_PROFILE && PERF_OVERLAY
static void perf_draw_overlay(void){
    char buf[32];
    int y = 2;
    int w = (g_rw < 130) ? g_rw - 2 : 128;
    if(w < 1) w = 1;
    fill_rect(backbuffer, 1, 1, w, 43, 252);
    snprintf(buf, sizeof(buf), "CPU:%lu", (unsigned long)g_prof_frame_work_ticks);
    draw_text3x5(backbuffer, 3, y, buf, 1, 255); y += 7;
    snprintf(buf, sizeof(buf), "LOG:%lu", (unsigned long)g_prof_logic_ticks);
    draw_text3x5(backbuffer, 3, y, buf, 1, 255); y += 7;
    snprintf(buf, sizeof(buf), "REN:%lu", (unsigned long)g_prof_render_ticks);
    draw_text3x5(backbuffer, 3, y, buf, 1, 255); y += 7;
    snprintf(buf, sizeof(buf), "COL:%lu DR:%lu", (unsigned long)g_prof_render_collect_ticks,
             (unsigned long)g_prof_render_draw_ticks);
    draw_text3x5(backbuffer, 3, y, buf, 1, 255);
}
#endif

