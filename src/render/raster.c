#include "mode4_fast.h"

static IWRAM_CODE ARM_CODE void draw_span_affine_line(
    u16* dst_line, int x0, int x1,
    fix u, fix v, fix du_dx, fix dv_dx,
    const u8* tex, int base_idx, int light_val)
{
    if(x1 < 0 || x0 >= g_rw) return;

    if(x0 < 0){
        int dx = -x0;
        u += du_dx * dx;
        v += dv_dx * dx;
        x0 = 0;
    }
    if(x1 >= g_rw) x1 = g_rw - 1;

    int width = x1 - x0 + 1;
    if(width <= 0) return;

    if(!g_textures_enabled){
        const u8* shade = tex_light_lut[light_val & 3];
        mode4_fill_span_asm(dst_line, x0, width, (u8)(shade[4] | base_idx));
        return;
    }

    mode4_draw_span_affine_asm(dst_line, x0, width, u, v, du_dx, dv_dx, tex);
}

HOT_INLINE ARM_CODE void cam_to_screen_focal_block(const v3* pc, fix focal, int* sx, int* sy){
    fix invz = fmuli(focal, fast_invz(pc->z));
    *sx = (g_rw >> 1) + F2I(fmuli(pc->x, invz));
    *sy = (g_rh >> 1) - F2I(fmuli(pc->y, invz));
}

typedef struct {
    int y0, y1;
    fix x, dx;
    fix u, du;
    fix v, dv;
} QuadEdge;

static inline void quad_sort_edges(QuadEdge** sorted, int count){
    for(int i = 1; i < count; i++){
        QuadEdge* e = sorted[i];
        int j = i - 1;
        while(j >= 0 && sorted[j]->y0 > e->y0){
            sorted[j + 1] = sorted[j];
            j--;
        }
        sorted[j + 1] = e;
    }
}

static inline bool quad_edge_init(v2uv a, v2uv b, QuadEdge* e){
    if(a.y == b.y) return false;
    if(b.y < a.y){ v2uv t = a; a = b; b = t; }
    if(b.y <= 0 || a.y >= g_rh) return false;

    int h = b.y - a.y;
    fix inv = inv_int(h);
    e->dx = fmuli(I2F(b.x - a.x), inv);
    e->du = fmuli(b.u - a.u, inv);
    e->dv = fmuli(b.v - a.v, inv);

    e->y0 = a.y;
    e->y1 = b.y;
    if(e->y0 < 0) e->y0 = 0;
    if(e->y1 > g_rh) e->y1 = g_rh;
    if(e->y0 >= e->y1) return false;

    int skip = e->y0 - a.y;
    e->x = I2F(a.x) + e->dx * skip;
    e->u = a.u + e->du * skip;
    e->v = a.v + e->dv * skip;
    return true;
}

static IWRAM_CODE ARM_CODE void draw_quad_affine(v2uv p0, v2uv p1, v2uv p2, v2uv p3, const u8* tex, int base_idx, int light_val){
    int det = (p1.x - p0.x) * (p2.y - p0.y) - (p2.x - p0.x) * (p1.y - p0.y);
    if(det == 0) return;
    int dy20 = (p2.y - p0.y);
    int dy10 = (p1.y - p0.y);
    int32_t du10 = (int32_t)(p1.u - p0.u);
    int32_t du20 = (int32_t)(p2.u - p0.u);
    int32_t dv10 = (int32_t)(p1.v - p0.v);
    int32_t dv20 = (int32_t)(p2.v - p0.v);
    int32_t num_u = du10 * dy20 - du20 * dy10;
    int32_t num_v = dv10 * dy20 - dv20 * dy10;
    fix invDet = inv_int_fast(det < 0 ? -det : det);
    if(det < 0) invDet = -invDet;
    fix du_dx = fmuli((fix)num_u, invDet);
    fix dv_dx = fmuli((fix)num_v, invDet);

    QuadEdge edges[4];
    int edge_count = 0;
    int min_y = g_rh;
    int max_y = 0;
    if(quad_edge_init(p0, p1, &edges[edge_count])) edge_count++;
    if(quad_edge_init(p1, p2, &edges[edge_count])) edge_count++;
    if(quad_edge_init(p2, p3, &edges[edge_count])) edge_count++;
    if(quad_edge_init(p3, p0, &edges[edge_count])) edge_count++;
    if(edge_count < 2) return;

    for(int i = 0; i < edge_count; i++){
        if(edges[i].y0 < min_y) min_y = edges[i].y0;
        if(edges[i].y1 > max_y) max_y = edges[i].y1;
    }
    if(min_y < 0) min_y = 0;
    if(max_y > g_rh) max_y = g_rh;

    QuadEdge* sorted[4];
    for(int i = 0; i < edge_count; i++) sorted[i] = &edges[i];
    quad_sort_edges(sorted, edge_count);

    QuadEdge* active0 = NULL;
    QuadEdge* active1 = NULL;
    int next_edge = 0;
    u16* dst_line = (u16*)backbuffer + (min_y * (SCREEN_W >> 1));

    for(int y = min_y; y < max_y; y++){
        if(active0 && y >= active0->y1) active0 = NULL;
        if(active1 && y >= active1->y1) active1 = NULL;
        while(next_edge < edge_count && sorted[next_edge]->y0 <= y){
            QuadEdge* e = sorted[next_edge++];
            if(e->y1 > y){
                if(!active0) active0 = e;
                else if(!active1) active1 = e;
            }
        }
        if(active0 && active1){
            QuadEdge* left = active0;
            QuadEdge* right = active1;
            if(right->x < left->x){
                left = active1;
                right = active0;
            }
            draw_span_affine_line(dst_line, F2I(left->x), F2I(right->x + F(0.5f)),
                                  left->u, left->v, du_dx, dv_dx, tex, base_idx, light_val);
            active0->x += active0->dx;
            active0->u += active0->du;
            active0->v += active0->dv;
            active1->x += active1->dx;
            active1->u += active1->du;
            active1->v += active1->dv;
        }
        dst_line += (SCREEN_W >> 1);
    }
}

static IWRAM_CODE ARM_CODE void draw_block_face_textured(
    const Face* f, const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos, fix focal)
{
    const fix U0 = 0, U1 = I2F(15), V0 = 0, V1 = I2F(15);

    u8 base_type = f->blk;
    int light_val = 2;
    if(f->dir == FACE_NEGY) light_val = 1;
    else if(f->dir == FACE_POSY || f->dir == FACE_POSZ || f->dir == FACE_NEGZ) light_val = 3;

    u8 render_block = base_type;
    if(base_type == BLK_GRASS) {
        if(f->dir == FACE_POSY) render_block = BLK_GRASS;
        else if(f->dir == FACE_NEGY) render_block = BLK_DIRT;
        else render_block = BLK_GRASS_SIDE;
    } else if(base_type == BLK_TRACK) {
        render_block = (f->dir == FACE_POSY) ? BLK_TRACK : BLK_TRACK_SIDE;
    }

    int base_idx = g_textures_enabled ? 0 : (block_flat_palette_type(render_block) << 3);
    const u8* tex = shaded_texture_for(render_block, light_val);

    const v3 stepX = { fx->x, fy->x, fz->x };
    const v3 stepY = { fx->y, fy->y, fz->y };
    const v3 stepZ = { fx->z, fy->z, fz->z };

    fix wx0 = I2F(f->x), wy0 = I2F(f->y), wz0 = I2F(f->z);
    fix wx1 = wx0 + FONE, wy1 = wy0 + FONE, wz1 = wz0 + FONE;

    v3 pc[4];
    v2uv p[4];

    switch(f->dir){
        case FACE_POSX:
            pc[0] = world_to_cam(wx1, wy0, wz0, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepY.x, pc[0].y + stepY.y, pc[0].z + stepY.z };
            pc[3] = (v3){ pc[0].x + stepZ.x, pc[0].y + stepZ.y, pc[0].z + stepZ.z };
            pc[2] = (v3){ pc[1].x + stepZ.x, pc[1].y + stepZ.y, pc[1].z + stepZ.z };
            p[0].u = U0; p[0].v = V1; p[1].u = U0; p[1].v = V0; p[2].u = U1; p[2].v = V0; p[3].u = U1; p[3].v = V1;
            break;
        case FACE_NEGX:
            pc[0] = world_to_cam(wx0, wy0, wz1, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepY.x, pc[0].y + stepY.y, pc[0].z + stepY.z };
            pc[3] = (v3){ pc[0].x - stepZ.x, pc[0].y - stepZ.y, pc[0].z - stepZ.z };
            pc[2] = (v3){ pc[1].x - stepZ.x, pc[1].y - stepZ.y, pc[1].z - stepZ.z };
            p[0].u = U1; p[0].v = V1; p[1].u = U1; p[1].v = V0; p[2].u = U0; p[2].v = V0; p[3].u = U0; p[3].v = V1;
            break;
        case FACE_POSY:
            pc[0] = world_to_cam(wx0, wy1, wz0, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepX.x, pc[0].y + stepX.y, pc[0].z + stepX.z };
            pc[3] = (v3){ pc[0].x + stepZ.x, pc[0].y + stepZ.y, pc[0].z + stepZ.z };
            pc[2] = (v3){ pc[1].x + stepZ.x, pc[1].y + stepZ.y, pc[1].z + stepZ.z };
            p[0].u = U0; p[0].v = V0; p[1].u = U1; p[1].v = V0; p[2].u = U1; p[2].v = V1; p[3].u = U0; p[3].v = V1;
            break;
        case FACE_NEGY:
            pc[0] = world_to_cam(wx0, wy0, wz1, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepX.x, pc[0].y + stepX.y, pc[0].z + stepX.z };
            pc[3] = (v3){ pc[0].x - stepZ.x, pc[0].y - stepZ.y, pc[0].z - stepZ.z };
            pc[2] = (v3){ pc[1].x - stepZ.x, pc[1].y - stepZ.y, pc[1].z - stepZ.z };
            p[0].u = U0; p[0].v = V1; p[1].u = U1; p[1].v = V1; p[2].u = U1; p[2].v = V0; p[3].u = U0; p[3].v = V0;
            break;
        case FACE_POSZ:
            pc[0] = world_to_cam(wx0, wy0, wz1, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepY.x, pc[0].y + stepY.y, pc[0].z + stepY.z };
            pc[3] = (v3){ pc[0].x + stepX.x, pc[0].y + stepX.y, pc[0].z + stepX.z };
            pc[2] = (v3){ pc[1].x + stepX.x, pc[1].y + stepX.y, pc[1].z + stepX.z };
            p[0].u = U0; p[0].v = V1; p[1].u = U0; p[1].v = V0; p[2].u = U1; p[2].v = V0; p[3].u = U1; p[3].v = V1;
            break;
        case FACE_NEGZ:
        default:
            pc[0] = world_to_cam(wx1, wy0, wz0, fx, fy, fz, cam_pos);
            pc[1] = (v3){ pc[0].x + stepY.x, pc[0].y + stepY.y, pc[0].z + stepY.z };
            pc[3] = (v3){ pc[0].x - stepX.x, pc[0].y - stepX.y, pc[0].z - stepX.z };
            pc[2] = (v3){ pc[1].x - stepX.x, pc[1].y - stepX.y, pc[1].z - stepX.z };
            p[0].u = U1; p[0].v = V1; p[1].u = U1; p[1].v = V0; p[2].u = U0; p[2].v = V0; p[3].u = U0; p[3].v = V1;
            break;
    }

    for(int i = 0; i < 4; i++){
        if(pc[i].z <= g_near_z) pc[i].z = g_near_z + F(0.01f);
        cam_to_screen_focal_block(&pc[i], focal, &p[i].x, &p[i].y);
    }
    draw_quad_affine(p[0], p[1], p[2], p[3], tex, base_idx, light_val);
}
