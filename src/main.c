/*

Minicraft 3D by Tobias Friedly / GameOfTobi 
https://www.youtube.com/c/gameoftobi

version 0.4

Last Edited 12/6/2025


*/

#include "platform.h"
int g_dimension = 0; 

#define CW 80
#define CH 32
#define CD 80
static EWRAM_DATA u8 world[CW*CH*CD];

static inline int widx(int x,int y,int z){ 
    if(x < 0 || x >= CW || y < 0 || y >= CH || z < 0 || z >= CD) return CW*CH*CD/2;
    return (y*CD + z)*CW + x; 
}

typedef struct {
    u32 tag; u32 version; u32 seed;
    u8  superflat, dimension;
    u16 edit_count;
    fix posx, posy, posz;
    s16 yaw, pitch;
    u8  curBlk;
    u8  reserved1[8];
} SaveSlotHeader;

static EWRAM_DATA u32 g_edit_log[MAX_EDITS];
static u16 g_edit_count = 0;
static u32 g_world_seed = 0;
static bool g_world_superflat = false;
typedef struct { v3 pos; int yaw, pitch; v3 vel; bool onGround; } Player;
static Player pl;

static inline u32 pack_edit(int idx, u8 v){ return (((u32)idx & 0x3FFFFu) | ((u32)v << 18)); }
static inline int  edit_idx(u32 e){ return (int)(e & 0x3FFFFu); }
static inline u8   edit_val(u32 e){ return (u8)((e >> 18) & 0xFFu); }

static void editlog_clear(void){ g_edit_count = 0; }
static void editlog_add_or_update(int idx, u8 val){
    for(int i=(int)g_edit_count-1; i>=0; --i){
        if(edit_idx(g_edit_log[i]) == idx){ g_edit_log[i] = pack_edit(idx,val); return; }
    }
    if(g_edit_count < MAX_EDITS) g_edit_log[g_edit_count++] = pack_edit(idx,val);
}

static inline void sram_write(u32 off, const void* src, u32 size){
    const u8* s = (const u8*)src; volatile u8* d = SRAM_BASE + off;
    for(u32 i=0;i<size;i++) d[i] = s[i];
}
static inline void sram_read(u32 off, void* dst, u32 size){
    volatile const u8* s = SRAM_BASE + off; u8* d = (u8*)dst;
    for(u32 i=0;i<size;i++) d[i] = s[i];
}
static u32 checksum32(const void* data, u32 size){
    const u8* p = (const u8*)data; u32 sum = 0;
    for(u32 i=0;i<size;i++) sum = (sum*16777619u) ^ p[i];
    return sum;
}
static void slot_zero_edit_area(u32 base){
    u8 zero = 0;
    for(u32 i=0;i<EDIT_AREA_SIZE;i++) SRAM_BASE[base + SLOT_HDR_PAD + i] = zero;
}
static bool slot_compute_and_compare(u32 base, u32* out_calc, u32* out_stored){
    u8 hdrbuf[SLOT_HDR_PAD];
    sram_read(base + 0, hdrbuf, SLOT_HDR_PAD);
    u32 stored = 0;
    sram_read(base + SLOT_CHKSUM_OFF, &stored, sizeof(stored));
    u32 sum = checksum32(hdrbuf, SLOT_HDR_PAD);
    u8 tmp[64];
    u32 remaining = EDIT_AREA_SIZE, off = 0;
    while(remaining){
        u32 chunk = (remaining > sizeof(tmp)) ? (u32)sizeof(tmp) : remaining;
        sram_read(base + SLOT_HDR_PAD + off, tmp, chunk);
        sum ^= checksum32(tmp, chunk);
        off += chunk; remaining -= chunk;
    }
    if(out_calc) *out_calc = sum;
    if(out_stored) *out_stored = stored;
    return (sum == stored);
}
static bool slot_is_valid(int slot, SaveSlotHeader* out_hdr){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    u8 pad[SLOT_HDR_PAD];
    sram_read(base + 0, pad, SLOT_HDR_PAD);
    SaveSlotHeader hdr; memcpy(&hdr, pad, sizeof(hdr));
    if(hdr.tag != 0x534C4F54u || hdr.version != 1) return false; 
    u32 calc, stored;
    if(!slot_compute_and_compare(base, &calc, &stored)) return false;
    if(out_hdr) *out_hdr = hdr;
    return true;
}
static void save_world_to_slot_raw(int slot, u8 curBlk){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    SaveSlotHeader hdr; memset(&hdr, 0, sizeof(hdr));
    hdr.tag = 0x534C4F54u; hdr.version = 1;
    hdr.seed = g_world_seed; hdr.superflat = g_world_superflat ? 1u : 0u;
    hdr.dimension = (u8)g_dimension;
    hdr.edit_count = g_edit_count;
    hdr.posx = pl.pos.x; hdr.posy = pl.pos.y; hdr.posz = pl.pos.z;
    hdr.yaw = (s16)pl.yaw; hdr.pitch = (s16)pl.pitch; hdr.curBlk = curBlk;
    u8 pad[SLOT_HDR_PAD]; memset(pad, 0, sizeof(pad)); memcpy(pad, &hdr, sizeof(hdr));
    sram_write(base + 0, pad, SLOT_HDR_PAD);
    slot_zero_edit_area(base);
    if(g_edit_count) sram_write(base + SLOT_HDR_PAD, g_edit_log, g_edit_count*sizeof(u32));
    u8 tmp[64];
    u32 sum = checksum32(pad, SLOT_HDR_PAD);
    u32 remaining = EDIT_AREA_SIZE, off = 0;
    while(remaining){
        u32 chunk = (remaining > sizeof(tmp)) ? (u32)sizeof(tmp) : remaining;
        sram_read(base + SLOT_HDR_PAD + off, tmp, chunk);
        sum ^= checksum32(tmp, chunk);
        off += chunk; remaining -= chunk;
    }
    sram_write(base + SLOT_CHKSUM_OFF, &sum, sizeof(sum));
}
static bool save_world_and_verify(int slot, u8 curBlk){
    save_world_to_slot_raw(slot, curBlk);
    u32 calc=0, stored=0;
    return slot_compute_and_compare((slot==0)?SLOT0_OFF:SLOT1_OFF, &calc, &stored);
}
static void gen_world(void);

static bool load_world_from_slot(int slot, u8* io_curBlk){
    u32 base = (slot==0)? SLOT0_OFF : SLOT1_OFF;
    if(!slot_is_valid(slot, NULL)) return false;
    u8 pad[SLOT_HDR_PAD];
    sram_read(base + 0, pad, SLOT_HDR_PAD);
    SaveSlotHeader hdr; memcpy(&hdr, pad, sizeof(hdr));
    g_world_seed = hdr.seed; g_world_superflat = (hdr.superflat != 0);
    g_dimension = hdr.dimension;
    
    buildPalette();

    gen_world();
    u16 count = hdr.edit_count; if(count > MAX_EDITS) count = MAX_EDITS;
    sram_read(base + SLOT_HDR_PAD, g_edit_log, count*sizeof(u32));
    g_edit_count = count;
    for(u16 i=0;i<count;i++){
        int idx = edit_idx(g_edit_log[i]); u8 val = edit_val(g_edit_log[i]);
        if(idx >= 0 && idx < (CW*CH*CD)) world[idx] = val;
    }
    pl.pos.x = hdr.posx; pl.pos.y = hdr.posy; pl.pos.z = hdr.posz;
    pl.yaw = hdr.yaw; pl.pitch = hdr.pitch;
    if(io_curBlk) *io_curBlk = (hdr.curBlk ? hdr.curBlk : *io_curBlk);
    return true;
}
static inline void world_set_and_log(int x,int y,int z,u8 val){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return;
    int id = widx(x,y,z);
    world[id] = val;
    editlog_add_or_update(id, val);
}


static inline u32 noise2_u16(int x,int z){
    u32 s = (u32)( (u32)(x + (int)g_world_seed) * 73856093u) ^ (u32)( (u32)(z + (int)(g_world_seed>>1)) * 19349663u);
    s ^= s>>13; s *= 0x27d4eb2d; s ^= s>>15;
    return (s & 0xFFFFu);
}

static fix vnoise_norm(int x, int z, int scalePow2, int seed){
    int mask = (1<<scalePow2) - 1;
    int gx = x >> scalePow2, gz = z >> scalePow2;
    fix fx = F((x & mask) / (float)(1<<scalePow2));
    fix fz = F((z & mask) / (float)(1<<scalePow2));
    u32 n00 = noise2_u16(gx+seed,   gz+seed);
    u32 n10 = noise2_u16(gx+1+seed, gz+seed);
    u32 n01 = noise2_u16(gx+seed,   gz+1+seed);
    u32 n11 = noise2_u16(gx+1+seed, gz+1+seed);
    fix a = (fix)((n00<<PRECISION) / 65535u);
    fix b = (fix)((n10<<PRECISION) / 65535u);
    fix c = (fix)((n01<<PRECISION) / 65535u);
    fix d = (fix)((n11<<PRECISION) / 65535u);
    fix ix0 = flerp(a, b, fx);
    fix ix1 = flerp(c, d, fx);
    return flerp(ix0, ix1, fz);
}

static void gen_nether(void) {
    
    for(int x=0; x<CW; x++){
        for(int z=0; z<CD; z++){
            world[widx(x,0,z)] = BLK_BEDROCK+1;
            world[widx(x,CH-1,z)] = BLK_BEDROCK+1;
            
            fix n1 = vnoise_norm(x, z, 4, 100);
            fix n2 = vnoise_norm(x, z, 3, 200);
            fix hfix = fmuli(n1, F(14)) + fmuli(n2, F(7));
            int h = 5 + F2I(hfix);
            if(h < 4) h = 4;
            if(h > CH-6) h = CH-6;

            for(int y=1; y<CH-1; y++){
                if (y < h) world[widx(x,y,z)] = BLK_NETHERRACK+1;
                else if (y < 12) world[widx(x,y,z)] = BLK_LAVA+1; 
            }
        }
    }
}

static void gen_world(void){
    memset(world, 0, sizeof(world));
    if (g_dimension == 1) {
        gen_nether();
        return;
    }

    if(g_world_superflat){
        const int flatHeight = 10;
        for(int x=0; x<CW; x++){
            for(int z=0; z<CD; z++){
                world[widx(x,0,z)] = BLK_BEDROCK+1;
                for(int y=1; y<flatHeight-3; y++) world[widx(x,y,z)] = BLK_STONE+1;
                for(int y=flatHeight-3; y<flatHeight; y++) world[widx(x,y,z)] = BLK_DIRT+1;
                world[widx(x,flatHeight,z)] = BLK_GRASS+1;
            }
        }
        return;
    }
    
    const int seaLevel = 14;
    for(int x=0; x<CW; x++){
        for(int z=0; z<CD; z++){
            world[widx(x,0,z)] = BLK_BEDROCK+1;
            fix n1 = vnoise_norm(x, z, 4, 11);
            fix n2 = vnoise_norm(x, z, 3, 37);
            fix n3 = vnoise_norm(x, z, 2, 73);
            fix hfix = fmuli(n1, F(12)) + fmuli(n2, F(6)) + fmuli(n3, F(3));
            int h = 10 + F2I(hfix);
            if (h < 8)  h = 8;
            if (h > CH-2) h = CH-2;

            bool sandBeach = (h <= seaLevel + 1 && h >= seaLevel - 1);

            for(int y=1; y<CH; y++){
                if      (y < h-4)                         world[widx(x,y,z)] = BLK_STONE+1;
                else if (y < h)                           world[widx(x,y,z)] = sandBeach ? BLK_SAND+1 : BLK_DIRT+1;
                else if (y == h)                          world[widx(x,y,z)] = sandBeach ? BLK_SAND+1 : BLK_GRASS+1;
                else if (y <= seaLevel && y > h)          world[widx(x,y,z)] = BLK_WATER+1;
            }

            if(h >= 15 && !sandBeach && (noise2_u16(x+101, z+777) & 1023) < 8){
                int tx = x, tz = z;
                int ty = h + 1;
                int th = 4 + (noise2_u16(x+55, z+33) & 1);
                for(int i=0; i<th && ty+i < CH; i++){
                    world[widx(tx, ty+i, tz)] = BLK_WOOD+1;
                }
                int top = ty + th;
                for(int dy=-2; dy<=2; dy++){
                    for(int dx=-2; dx<=2; dx++){
                        for(int dz=-2; dz<=2; dz++){
                            if(abs(dx)+abs(dz) > 3) continue;
                            int lx = tx+dx, ly = top+dy, lz = tz+dz;
                            if(lx>=0 && lx<CW && ly>=0 && ly<CH && lz>=0 && lz<CD) {
                                if(world[widx(lx,ly,lz)] == 0) {
                                    world[widx(lx,ly,lz)] = BLK_LEAF+1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}


static struct {
    bool movement_active;
    bool rotation_active;
} input_state = {false, false};

static volatile u32 g_frame = 0;
static bool g_is_sprinting = false;
static u32 g_last_up_tap_frame = 0;

static bool cam_basis(v3* fx, v3* fy, v3* fz){
    pl.yaw = pl.yaw & (LUT_N-1);
    int p_min = -(LUT_N>>2) + 5;
    int p_max =  (LUT_N>>2) - 5;
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

static void handle_input(void){
    scanKeys();
    u16 k = keysHeld();
    u16 kd = keysDown();
    bool select_held = (k & KEY_SELECT);
    
    if(!select_held && (kd & KEY_UP)){
        if(g_frame - g_last_up_tap_frame <= 18) g_is_sprinting = !g_is_sprinting;
        g_last_up_tap_frame = g_frame;
    }

    fix speed = (k & KEY_START) || g_is_sprinting ? F(10.0f/60.f) : F(6.0f/60.f);
    const fix turn_speed = 5;

    v3 fx, fy, fz;
    if(!cam_basis(&fx, &fy, &fz)) return;
    
    v3 wish = {0,0,0};
    input_state.movement_active = false;
    if(!select_held){
        if(k & KEY_UP)   { wish.x += fz.x; wish.z += fz.z; input_state.movement_active = true; }
        if(k & KEY_DOWN) { wish.x -= fz.x; wish.z -= fz.z; input_state.movement_active = true; }
        if(k & KEY_LEFT) { wish.x -= fx.x; wish.z -= fx.z; input_state.movement_active = true; }
        if(k & KEY_RIGHT){ wish.x += fx.x; wish.z += fx.z; input_state.movement_active = true; }
    }

    fix len_sq = fmuli(wish.x, wish.x) + fmuli(wish.z, wish.z);
    if(len_sq > 0) {
        fix len = fsqrt(len_sq);
        if(len > F(0.01f)){
            wish.x = fdivi(wish.x, len);
            wish.z = fdivi(wish.z, len);
        }
    }
    pl.vel.x = fmuli(wish.x, speed);
    pl.vel.z = fmuli(wish.z, speed);

    if((k & KEY_B) && pl.onGround && !select_held) pl.vel.y = F(22.5f/60.f);

    input_state.rotation_active = false;
    if(!select_held){
        if(k & KEY_L) { pl.yaw = (pl.yaw - turn_speed) & (LUT_N-1); input_state.rotation_active = true; }
        if(k & KEY_R) { pl.yaw = (pl.yaw + turn_speed) & (LUT_N-1); input_state.rotation_active = true; }
    }
    if(!input_state.movement_active && select_held){
        if(k & KEY_L) { pl.pitch -= 3; input_state.rotation_active = true; }
        if(k & KEY_R) { pl.pitch += 3; input_state.rotation_active = true; }
    }
}

static inline bool solid_at(int x,int y,int z){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return false;
    u8 id = world[widx(x,y,z)];
    if(!id) return false;
    u8 t = id - 1;
    return (t != BLK_WATER && t != BLK_LEAF && t != BLK_PORTAL && t != BLK_LAVA);
}

static IWRAM_CODE ARM_CODE void collide_axis(char axis){
    fix r = F(0.3f), h = F(1.6f);
    int minx = F2I(pl.pos.x - r) - 1, maxx = F2I(pl.pos.x + r) + 1;
    int miny = F2I(pl.pos.y) - 1,     maxy = F2I(pl.pos.y + h) + 1;
    int minz = F2I(pl.pos.z - r) - 1, maxz = F2I(pl.pos.z + r) + 1;

    if(minx < 0) minx = 0; 
    if(maxx >= CW) maxx = CW-1;
    if(miny < 0) miny = 0; 
    if(maxy >= CH) maxy = CH-1;
    if(minz < 0) minz = 0; 
    if(maxz >= CD) maxz = CD-1;

    for(int y = miny; y <= maxy; y++){
        for(int z = minz; z <= maxz; z++){
            for(int x = minx; x <= maxx; x++){
                if(!solid_at(x,y,z)) continue;
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

static IWRAM_CODE ARM_CODE void physics_step(u8 curBlk){
    pl.onGround = false;
    pl.vel.y -= PHYS_GRAV_FIX;
    if(pl.vel.y < PHYS_MAG_1) pl.vel.y = PHYS_MAG_1;
    
    pl.pos.x += pl.vel.x; collide_axis('x');
    pl.pos.y += pl.vel.y; collide_axis('y');
    pl.pos.z += pl.vel.z; collide_axis('z');
    
    if(pl.pos.x < I2F(1)) pl.pos.x = I2F(1);
    if(pl.pos.x > I2F(CW-2)) pl.pos.x = I2F(CW-2);
    if(pl.pos.z < I2F(1)) pl.pos.z = I2F(1);
    if(pl.pos.z > I2F(CD-2)) pl.pos.z = I2F(CD-2);
    if(pl.pos.y > I2F(CH-2)) pl.pos.y = I2F(CH-2);
    
    if(pl.vel.x > PHYS_MAG_2)  pl.vel.x = PHYS_MAG_2;
    if(pl.vel.x < -PHYS_MAG_2) pl.vel.x = -PHYS_MAG_2;
    if(pl.vel.z > PHYS_MAG_2)  pl.vel.z = PHYS_MAG_2;
    if(pl.vel.z < -PHYS_MAG_2) pl.vel.z = -PHYS_MAG_2;
    
    if(pl.pos.y < -I2F(10)){
        pl.pos.x = I2F(CW>>1);
        pl.pos.y = I2F(25); 
        pl.pos.z = I2F(CD>>1);
        pl.vel.x = pl.vel.y = pl.vel.z = 0;
    }
    
    int px = F2I(pl.pos.x);
    int py = F2I(pl.pos.y + F(0.5f));
    int pz = F2I(pl.pos.z);
    u8 id = world[widx(px,py,pz)];
    if(id == BLK_PORTAL + 1) {
        g_portal_timer++;
        if(g_portal_timer > 90) { 
            switch_dimension(curBlk);
            g_portal_timer = 0;
        }
    } else {
        int py_feet = F2I(pl.pos.y);
        id = world[widx(px,py_feet,pz)];
        if(id == BLK_PORTAL + 1) {
             g_portal_timer++;
             if(g_portal_timer > 90) {
                 switch_dimension(curBlk);
                 g_portal_timer = 0;
             }
        } else {
            g_portal_timer = 0;
        }
    }
}


static int g_vis_dist = 8;
static fix g_focal    = F(130.0f);
static fix g_frustum_margin = F(1.2f);
static fix g_near_z = F(0.25f);
static const fix FAR_Z = F(40.0f);

enum { FACE_POSX=0, FACE_NEGX=1, FACE_POSY=2, FACE_NEGY=3, FACE_POSZ=4, FACE_NEGZ=5 };

typedef struct {
    int x,y,z;
    u8  dir;
    u8  blk;
    fix depth;
} Face;

#define MAX_FACES 1600
static EWRAM_DATA Face faceBuf[MAX_FACES];
static int faceCount = 0;

static inline bool air_or_outside(int x,int y,int z){
    if(x<0||x>=CW||y<0||y>=CH||z<0||z>=CD) return true;
    u8 id = world[widx(x,y,z)];
    return (id == 0 || id == (BLK_PORTAL+1)); 
}

static IWRAM_CODE ARM_CODE bool in_view_frustum(
    fix bx, fix by, fix bz, const v3* fz, const v3* fx, const v3* fy, const v3* cam_pos)
{
    fix dx = bx - cam_pos->x;
    fix dy = by - cam_pos->y;
    fix dz = bz - cam_pos->z;
    
    fix depth = fmuli(dx, fz->x) + fmuli(dy, fz->y) + fmuli(dz, fz->z);
    if(depth < g_near_z || depth > FAR_Z) return false;
    if(depth <= 0) return false;
    
    fix lat_x = fmuli(dx, fx->x) + fmuli(dy, fx->y) + fmuli(dz, fx->z);
    fix lat_y = fmuli(dx, fy->x) + fmuli(dy, fy->y) + fmuli(dz, fy->z);
    
    fix base_threshold = (depth >> 1) + (depth >> 2);
    fix threshold = fmuli(base_threshold, g_frustum_margin) + F(0.7f);
    
    if(fixabs(lat_x) > threshold) return false;
    if(fixabs(lat_y) > threshold) return false;
    return true;
}

static IWRAM_CODE ARM_CODE void emit_face_if_visible(
    int x,int y,int z, u8 blk, u8 dir, const v3* fz, const v3* cam_pos)
{
    bool vis = false;
    int nx=x,ny=y,nz=z;
    switch(dir){
        case FACE_POSX: nx=x+1; vis=air_or_outside(nx,ny,nz); break;
        case FACE_NEGX: nx=x-1; vis=air_or_outside(nx,ny,nz); break;
        case FACE_POSY: ny=y+1; vis=air_or_outside(nx,ny,nz); break;
        case FACE_NEGY: ny=y-1; vis=air_or_outside(nx,ny,nz); break;
        case FACE_POSZ: nz=z+1; vis=air_or_outside(nx,ny,nz); break;
        case FACE_NEGZ: nz=z-1; vis=air_or_outside(nx,ny,nz); break;
    }
    if(!vis) return;

    v3 n;
    switch(dir){
        case FACE_POSX: n.x=FONE; n.y=0; n.z=0; break;
        case FACE_NEGX: n.x=-FONE;n.y=0; n.z=0; break;
        case FACE_POSY: n.x=0; n.y=FONE; n.z=0; break;
        case FACE_NEGY: n.x=0; n.y=-FONE;n.z=0; break;
        case FACE_POSZ: n.x=0; n.y=0; n.z=FONE; break;
        default:        n.x=0; n.y=0; n.z=-FONE; break;
    }
    fix ndotf = fmuli(n.x, fz->x) + fmuli(n.y, fz->y) + fmuli(n.z, fz->z);
    if(ndotf > F(0.15f)) return;

    fix cx = I2F(x) + F(0.5f);
    fix cy = I2F(y) + F(0.5f);
    fix cz = I2F(z) + F(0.5f);
    switch(dir){
        case FACE_POSX: cx=I2F(x+1); break;
        case FACE_NEGX: cx=I2F(x);   break;
        case FACE_POSY: cy=I2F(y+1); break;
        case FACE_NEGY: cy=I2F(y);   break;
        case FACE_POSZ: cz=I2F(z+1); break;
        case FACE_NEGZ: cz=I2F(z);   break;
    }

    v3 rel = { cx-cam_pos->x, cy-cam_pos->y, cz-cam_pos->z };
    fix depth = fmuli(rel.x, fz->x) + fmuli(rel.y, fz->y) + fmuli(rel.z, fz->z);
    if(depth <= g_near_z || depth > FAR_Z) return;

    if(faceCount < MAX_FACES){
        faceBuf[faceCount++] = (Face){x,y,z,dir,blk,depth};
    }
}

static IWRAM_CODE ARM_CODE v3 world_to_cam(
    fix wx, fix wy, fix wz, const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos)
{
    v3 rel = { wx-cam_pos->x, wy-cam_pos->y, wz-cam_pos->z };
    v3 out;
    out.x = fmuli(rel.x, fx->x) + fmuli(rel.y, fx->y) + fmuli(rel.z, fx->z);
    out.y = fmuli(rel.x, fy->x) + fmuli(rel.y, fy->y) + fmuli(rel.z, fy->z);
    out.z = fmuli(rel.x, fz->x) + fmuli(rel.y, fz->y) + fmuli(rel.z, fz->z);
    return out;
}

static IWRAM_CODE ARM_CODE bool cam_to_screen(const v3* pc, int* sx, int* sy){
    if(pc->z <= g_near_z) return false;
    fix focal = g_is_sprinting ? fmuli(g_focal, F(0.9f)) : g_focal;
    fix invz = fmuli(focal, fast_invz(pc->z));
    fix px = fmuli(pc->x, invz);
    fix py = fmuli(pc->y, invz);
    if(px < -I2F(W*2) || px > I2F(W*3) || py < -I2F(H*2) || py > I2F(H*3)) return false;
    *sx = (W>>1) + F2I(px);
    *sy = (H>>1) - F2I(py);
    return true;
}

static IWRAM_CODE void draw_sky_gradient(void) {
    int horizon_y = (H/2) - (pl.pitch >> 2); 
    if (horizon_y < 0) horizon_y = 0;
    if (horizon_y > H) horizon_y = H;
    u16* vram = (u16*)backbuffer; 
    
    
    for (int y = 0; y < horizon_y; y++) {
        int grad = (y * SKY_COLORS) / (horizon_y > 0 ? horizon_y : 1);
        if(grad >= SKY_COLORS) grad = SKY_COLORS - 1;
        int color_idx = SKY_START_IDX + (SKY_COLORS - 1 - grad);
        if (color_idx > 250) color_idx = 250;
        u16 val = (color_idx) | (color_idx << 8);
        u32 row_offset = (y * W) / 2;
        for (int x = 0; x < W/2; x+=2) {
            vram[row_offset + x] = val;
            vram[row_offset + x + 1] = val;
        }
    }
    
    
    int ground_h = H - horizon_y;
    for (int y = horizon_y; y < H; y++) {
        int dist = y - horizon_y;
        int grad = (dist * SKY_COLORS) / (ground_h > 0 ? ground_h : 1);
        if(grad >= SKY_COLORS) grad = SKY_COLORS - 1;
        int color_idx = VOID_START_IDX + grad;
        if (color_idx > 250) color_idx = 250;
        u16 val = (color_idx) | (color_idx << 8);
        u32 row_offset = (y * W) / 2;
        for (int x = 0; x < W/2; x++) {
            vram[row_offset + x] = val;
        }
    }
}

static IWRAM_CODE ARM_CODE void draw_span_affine(
    int y, int x0, int x1,
    fix u, fix v, fix du_dx, fix dv_dx,
    const u8* tex, int base_idx, int light_val)
{
    if(y < 0 || y >= H) return;
    if(x0 > x1){ int t=x0; x0=x1; x1=t; }
    if(x1 < 0 || x0 >= W) return;

    if(x0 < 0){
        int dx = -x0;
        u += (fix)((int64_t)du_dx * dx);
        v += (fix)((int64_t)dv_dx * dx);
        x0 = 0;
    }
    if(x1 >= W) x1 = W-1;

    int width = x1 - x0 + 1;
    if(width <= 0) return;

    u8* dst = (u8*)backbuffer + y*W + x0;
    
    if(((uintptr_t)dst) & 1){
        int tu = (u>>16)&7, tv=(v>>16)&7;
        int lvl = (int)tex[(tv<<3)+tu] + light_val;
        if(lvl > 7) lvl = 7; 
        *dst++ = base_idx | lvl;
        u+=du_dx; v+=dv_dx;
        width--;
        if(width<=0) return;
    }

    u16* dst16 = (u16*)dst;
    int pairs = width>>1;
    while(pairs--){
        int tu1=(u>>16)&7, tv1=(v>>16)&7;
        int lvl1=(int)tex[(tv1<<3)+tu1]+light_val;
        if(lvl1>7) lvl1=7;
        u8 c1=base_idx|lvl1;
        u+=du_dx; v+=dv_dx;

        int tu2=(u>>16)&7, tv2=(v>>16)&7;
        int lvl2=(int)tex[(tv2<<3)+tu2]+light_val;
        if(lvl2>7) lvl2=7;
        u8 c2=base_idx|lvl2;
        u+=du_dx; v+=dv_dx;

        *dst16++ = (u16)(c1 | (c2<<8));
    }

    if(width & 1){
        int tu=(u>>16)&7, tv=(v>>16)&7;
        int lvl=(int)tex[(tv<<3)+tu]+light_val;
        if(lvl>7) lvl=7;
        *((u8*)dst16) = base_idx|lvl;
    }
}

static inline void sort3_uv(v2uv* a, v2uv* b, v2uv* c){
    if(b->y < a->y){ v2uv t=*a;*a=*b;*b=t; }
    if(c->y < a->y){ v2uv t=*a;*a=*c;*c=t; }
    if(c->y < b->y){ v2uv t=*b;*b=*c;*c=t; }
}

static IWRAM_CODE ARM_CODE void draw_tri_affine(
    v2uv p0, v2uv p1, v2uv p2,
    const u8* tex, int base_idx, int light_val)
{
    sort3_uv(&p0,&p1,&p2);
    if(p0.y==p2.y) return;
    if(p2.y < 0 || p0.y >= H) return;

    int deti = (p1.x - p0.x)*(p2.y - p0.y) - (p2.x - p0.x)*(p1.y - p0.y);
    if(deti==0) return;
    
    int64_t num_u = (int64_t)(p1.u - p0.u)*(p2.y - p0.y) - (int64_t)(p2.u - p0.u)*(p1.y - p0.y);
    int64_t num_v = (int64_t)(p1.v - p0.v)*(p2.y - p0.y) - (int64_t)(p2.v - p0.v)*(p1.y - p0.y);
    fix du_dx = Div((int32_t)num_u, deti);
    fix dv_dx = Div((int32_t)num_v, deti);

    int h01 = p1.y - p0.y;
    int h02 = p2.y - p0.y;
    int h12 = p2.y - p1.y;

    fix dx01 = (h01>0)? Div((p1.x - p0.x) << PRECISION, h01) : 0;
    fix dx02 = (h02>0)? Div((p2.x - p0.x) << PRECISION, h02) : 0;
    fix dx12 = (h12>0)? Div((p2.x - p1.x) << PRECISION, h12) : 0;

    fix du01 = (h01>0)? Div(p1.u - p0.u, h01) : 0;
    fix dv01 = (h01>0)? Div(p1.v - p0.v, h01) : 0;
    fix du02 = (h02>0)? Div(p2.u - p0.u, h02) : 0;
    fix dv02 = (h02>0)? Div(p2.v - p0.v, h02) : 0;
    fix du12 = (h12>0)? Div(p2.u - p1.u, h12) : 0;
    fix dv12 = (h12>0)? Div(p2.v - p1.v, h12) : 0;

    fix x_long_at_y1 = I2F(p0.x) + dx02 * h01;
    bool long_left = (x_long_at_y1 < I2F(p1.x));

    fix xl=0,xr=0,ul=0,vl=0,dxl=0,dxr=0,dul=0,dvl=0;

    if(h01>0){
        if(long_left){
            xl=I2F(p0.x); dxl=dx02; ul=p0.u; dul=du02; vl=p0.v; dvl=dv02;
            xr=I2F(p0.x); dxr=dx01;
        }else{
            xl=I2F(p0.x); dxl=dx01; ul=p0.u; dul=du01; vl=p0.v; dvl=dv01;
            xr=I2F(p0.x); dxr=dx02;
        }
        int y_start=p0.y;
        if(y_start<0){
            int skip=-y_start;
            xl+=dxl*skip; xr+=dxr*skip;
            ul+=dul*skip; vl+=dvl*skip;
            y_start=0;
        }
        for(int y=y_start; y<p1.y && y<H; y++){
            draw_span_affine(y, F2I(xl), F2I(xr+F(0.5f)), ul, vl, du_dx, dv_dx, tex, base_idx, light_val);
            xl+=dxl; xr+=dxr; ul+=dul; vl+=dvl;
        }
    }

    if(h12>0){
        if(long_left){
            xl = I2F(p0.x) + dx02*h01;
            ul = p0.u + du02*h01;
            vl = p0.v + dv02*h01;
            dxl = dx02; dul=du02; dvl=dv02;
            xr = I2F(p1.x); dxr=dx12;
        }else{
            xr = I2F(p0.x) + dx02*h01;
            dxr = dx02;
            xl = I2F(p1.x); dxl=dx12;
            ul = p1.u; dul=du12;
            vl = p1.v; dvl=dv12;
        }
        int y_start=p1.y;
        if(y_start<0){
            int skip=-y_start;
            xl+=dxl*skip; xr+=dxr*skip;
            ul+=dul*skip; vl+=dvl*skip;
            y_start=0;
        }
        int y_end=p2.y;
        if(y_end>=H) y_end=H-1;
        for(int y=y_start; y<=y_end; y++){
            draw_span_affine(y, F2I(xl), F2I(xr+F(0.5f)), ul, vl, du_dx, dv_dx, tex, base_idx, light_val);
            xl+=dxl; xr+=dxr; ul+=dul; vl+=dvl;
        }
    }
}

static inline void draw_quad_affine(
    v2uv p0, v2uv p1, v2uv p2, v2uv p3,
    const u8* tex, int base_idx, int light_val)
{
    draw_tri_affine(p0,p1,p2,tex,base_idx,light_val);
    draw_tri_affine(p0,p2,p3,tex,base_idx,light_val);
}

static IWRAM_CODE ARM_CODE void sort_faces_insertion(Face* arr, int n){
    for(int i=1; i<n; i++){
        Face key = arr[i];
        int j = i - 1;
        while(j >= 0 && arr[j].depth < key.depth){
            arr[j+1] = arr[j];
            j--;
        }
        arr[j+1] = key;
    }
}

static IWRAM_CODE ARM_CODE void project_and_draw_quad_affine(
    const v3* q, u8 blk_type, int shade_level,
    const v3* fx, const v3* fy, const v3* fz,
    const v3* cam_pos,
    const fix* us, const fix* vs)
{
    v2uv p[4];
    int behind=0;

    for(int i=0;i<4;i++){
        v3 pc = world_to_cam(q[i].x,q[i].y,q[i].z,fx,fy,fz,cam_pos);
        int sx,sy;
        if(!cam_to_screen(&pc,&sx,&sy)){
            behind++;
        }else{
            p[i].x=sx; p[i].y=sy;
        }
        p[i].u=us[i]; p[i].v=vs[i];
    }
    if(behind>0) return; 

    const u8* tex = textures[blk_type];
    int base_idx = blk_type << 3; 
    int light_val = (shade_level >> 2); 
    
    draw_quad_affine(p[0],p[1],p[2],p[3],tex, base_idx, light_val);
}

static IWRAM_CODE ARM_CODE void draw_block_face_textured(
    const Face* f, const v3* fx, const v3* fy, const v3* fz, const v3* cam_pos)
{
    fix x = I2F(f->x), y=I2F(f->y), z=I2F(f->z);
    fix x1=I2F(f->x+1), y1=I2F(f->y+1), z1=I2F(f->z+1);

    v3 wv[4];
    fix us[4], vs[4];
    const fix U0=0, U1=I2F(7), V0=0, V1=I2F(7);

   switch(f->dir){
        case FACE_POSX:
            wv[0]=(v3){x1,y, z};  wv[1]=(v3){x1,y1,z};
            wv[2]=(v3){x1,y1,z1}; wv[3]=(v3){x1,y, z1};
            us[0]=U0;vs[0]=V1; us[1]=U0;vs[1]=V0; us[2]=U1;vs[2]=V0; us[3]=U1;vs[3]=V1;
            break;
        case FACE_NEGX:
            wv[0]=(v3){x, y, z1}; wv[1]=(v3){x, y1,z1};
            wv[2]=(v3){x, y1,z }; wv[3]=(v3){x, y, z };
            us[0]=U1;vs[0]=V1; us[1]=U1;vs[1]=V0; us[2]=U0;vs[2]=V0; us[3]=U0;vs[3]=V1;
            break;
        case FACE_POSY:
            wv[0]=(v3){x, y1,z };  wv[1]=(v3){x1,y1,z };
            wv[2]=(v3){x1,y1,z1};  wv[3]=(v3){x, y1,z1};
            us[0]=U0;vs[0]=V0; us[1]=U1;vs[1]=V0; us[2]=U1;vs[2]=V1; us[3]=U0;vs[3]=V1;
            break;
        case FACE_NEGY:
            wv[0]=(v3){x, y, z1};  wv[1]=(v3){x1,y, z1};
            wv[2]=(v3){x1,y, z };  wv[3]=(v3){x, y, z };
            us[0]=U0;vs[0]=V1; us[1]=U1;vs[1]=V1; us[2]=U1;vs[2]=V0; us[3]=U0;vs[3]=V0;
            break;
        case FACE_POSZ:
            wv[0]=(v3){x, y, z1};  wv[1]=(v3){x, y1,z1};
            wv[2]=(v3){x1,y1,z1};  wv[3]=(v3){x1,y, z1};
            us[0]=U0;vs[0]=V1; us[1]=U0;vs[1]=V0; us[2]=U1;vs[2]=V0; us[3]=U1;vs[3]=V1;
            break;
        case FACE_NEGZ:
        default:
            wv[0]=(v3){x1,y, z};  wv[1]=(v3){x1,y1,z};
            wv[2]=(v3){x, y1,z};  wv[3]=(v3){x, y, z};
            us[0]=U1;vs[0]=V1; us[1]=U1;vs[1]=V0; us[2]=U0;vs[2]=V0; us[3]=U0;vs[3]=V1;
            break;
    }

    u8 base_type = f->blk;
    int shade = 11; 
    if(f->dir == FACE_POSY) shade = 15; 
    else if(f->dir == FACE_NEGY) shade = 6; 
    else if(f->dir == FACE_POSZ || f->dir == FACE_NEGZ) shade = 13; 

    u8 render_block = base_type;
    
    if(base_type == BLK_GRASS) {
        if(f->dir == FACE_POSY) render_block = BLK_GRASS; 
        else if(f->dir == FACE_NEGY) render_block = BLK_DIRT; 
        else render_block = BLK_GRASS_SIDE; 
    }

    project_and_draw_quad_affine(wv,render_block,shade,fx,fy,fz,cam_pos,us,vs);
}


static inline void memset_page8(volatile u8* page, u8 val){
    u32 pat = (u32)val; pat |= pat<<8; pat |= pat<<16;
    CpuFastSet(&pat, (void*)page, ((W*H)>>2) | COPY32 | FILL);
}

static void draw_block_bar(u8 selected_blk){
    const int bar_h = 20;
    const int bar_y = H - bar_h;
    const int slot_w = 22;
    const int slots = 5;
    const int bar_start_x = (W - slots * slot_w) >> 1;
    
    u8 bg_color = 252;
    for(int y = bar_y; y < H; y++){
        for(int x = bar_start_x - 4; x < bar_start_x + slots * slot_w + 4; x++){
            if(x >= 0 && x < W) backbuffer[y * W + x] = bg_color;
        }
    }
    
    int total_buildable = 14;
    int buildable[] = {BLK_GRASS, BLK_DIRT, BLK_STONE, BLK_WOOD, BLK_BRICK, BLK_PLANK, BLK_COBBLE, BLK_SAND, BLK_LEAF, BLK_OBSIDIAN, BLK_NETHERRACK, BLK_LAVA, BLK_WATER, ITEM_FLINT_STEEL};
    int cur_idx = 0;
    for(int i=0;i<total_buildable;i++) {
        int check = (buildable[i] == ITEM_FLINT_STEEL) ? ITEM_FLINT_STEEL : buildable[i]+1;
        if(check == selected_blk) cur_idx = i;
    }
    
    int start_idx = cur_idx - 2;
    if(start_idx < 0) start_idx = 0;
    if(start_idx > total_buildable - 5) start_idx = total_buildable - 5;

    for(int i = 0; i < slots; i++){
        int b_idx = start_idx + i;
        int blk_type = buildable[b_idx];
        int x = bar_start_x + i * slot_w;
        
        bool is_selected = (selected_blk == (blk_type == ITEM_FLINT_STEEL ? ITEM_FLINT_STEEL : blk_type + 1));
        u8 border_color = is_selected ? 254 : 253;
        
        int border_width = is_selected ? 2 : 1;
        for(int bw = 0; bw < border_width; bw++){
            for(int bx = x + bw; bx < x + slot_w - 2 - bw && bx < W; bx++){
                if(bx >= 0){
                    backbuffer[(bar_y + bw) * W + bx] = border_color;
                    backbuffer[(H-1-bw) * W + bx] = border_color;
                }
            }
            for(int by = bar_y + bw; by < H - bw; by++){
                if(x + bw >= 0 && x + bw < W) backbuffer[by * W + x + bw] = border_color;
                if(x + slot_w - 3 - bw >= 0 && x + slot_w - 3 - bw < W) 
                    backbuffer[by * W + x + slot_w - 3 - bw] = border_color;
            }
        }
        
        u8 cube_color;
        if (blk_type == ITEM_FLINT_STEEL) {
             cube_color = 250; 
        } else {
             cube_color = blk_type * 8 + 4;
        }

        int cube_size = 11;
        int cube_x =  x + ( (slot_w - cube_size) >> 1 );
        int cube_y =  bar_y + ( (bar_h - cube_size) >> 1 );
        
        for(int cy = cube_y; cy < cube_y + cube_size && cy < H; cy++){
            for(int cx = cube_x; cx < cube_x + cube_size && cx < W; cx++){
                if(cx >= 0) {
                    if (blk_type == ITEM_FLINT_STEEL) {
                        if (cy < cube_y+2 || cy > cube_y + cube_size - 3 || cx < cube_x + 3)
                            backbuffer[cy * W + cx] = cube_color;
                    } else {
                        backbuffer[cy * W + cx] = cube_color;
                    }
                }
            }
        }
    }
}

#define RGB332_IDX(r3,g3,b2) ((u8)(((r3)<<5)|((g3)<<2)|(b2)))
#define DIRT_COLOR RGB332_IDX(3,2,0)
#define MENU_TEXT_WHITE 255
#define MENU_TEXT_SELECTED 254
#define MENU_TEXT_UNSELECTED 253
#define MENU_TEXT_LABEL 253

static inline u16 rgb332_to_rgb555(u8 v){
    int r = (v >> 5) & 0x7;
    int g = (v >> 2) & 0x7;
    int b =  v       & 0x3;
    int R = (r * 31 + 3) / 7;
    int G = (g * 31 + 3) / 7;
    int B = (b * 31 + 1) / 3;
    if (R > 31) R = 31;
    if (G > 31) G = 31;
    if (B > 31) B = 31;
    return (u16)(R | (G<<5) | (B<<10));
}

static void menu_build_rgb332_palette(u16* out){
    for(int i=0;i<256;i++) out[i] = rgb332_to_rgb555((u8)i);
}

static void menu_apply_fade(const u16* base, int factor, int steps){
    if(steps <= 0) steps = 1;
    for(int i=0;i<256;i++){
        u16 c = base[i];
        int r = (c & 31) * factor / steps;
        int g = ((c >> 5) & 31) * factor / steps;
        int b = ((c >> 10) & 31) * factor / steps;
        PAL_BG_MEM[i] = (u16)(r | (g<<5) | (b<<10));
    }
}

static void menu_blit_image_both_pages(const u8* img){
    CpuFastSet(img, (void*)VRAM_PAGE0, ((W*H)>>2) | COPY32);
    CpuFastSet(img, (void*)VRAM_PAGE1, ((W*H)>>2) | COPY32);
}

static inline void fill_rect(volatile u8* dst,int x,int y,int w,int h,u8 c){
    int x1=x+w-1, y1=y+h-1;
    if(x>=W||y>=H||x1<0||y1<0) return;
    if(x<0){ w+=x; x=0; } if(y<0){ h+=y; y=0; }
    if(x+w>W) w=W-x;
    if(y+h>H) h=H-y;
    for(int yy=0; yy<h; yy++){
        int base=(y+yy)*W + x;
        for(int xx=0; xx<w; xx++) dst[base+xx]=c;
    }
}

typedef struct { char ch; u8 r[5]; } Glyph3x5;
static const Glyph3x5 font3x5[] = {
    {' ',{0,0,0,0,0}}, {'A',{0b010,0b101,0b111,0b101,0b101}}, {'B',{0b110,0b101,0b110,0b101,0b110}},
    {'C',{0b011,0b100,0b100,0b100,0b011}}, {'D',{0b110,0b101,0b101,0b101,0b110}}, {'E',{0b111,0b100,0b110,0b100,0b111}},
    {'F',{0b111,0b100,0b110,0b100,0b100}}, {'G',{0b011,0b100,0b101,0b101,0b011}}, {'H',{0b101,0b101,0b111,0b101,0b101}},
    {'I',{0b111,0b010,0b010,0b010,0b111}}, {'J',{0b111,0b001,0b001,0b101,0b010}}, {'K',{0b101,0b110,0b100,0b110,0b101}},
    {'L',{0b100,0b100,0b100,0b100,0b111}}, {'M',{0b101,0b111,0b111,0b101,0b101}}, {'N',{0b101,0b111,0b111,0b111,0b101}},
    {'O',{0b111,0b101,0b101,0b101,0b111}}, {'P',{0b110,0b101,0b110,0b100,0b100}}, {'Q',{0b111,0b101,0b101,0b111,0b011}},
    {'R',{0b110,0b101,0b110,0b110,0b101}}, {'S',{0b011,0b100,0b010,0b001,0b110}}, {'T',{0b111,0b010,0b010,0b010,0b010}},
    {'U',{0b101,0b101,0b101,0b101,0b111}}, {'V',{0b101,0b101,0b101,0b101,0b010}}, {'W',{0b101,0b101,0b111,0b111,0b101}},
    {'X',{0b101,0b101,0b010,0b101,0b101}}, {'Y',{0b101,0b101,0b010,0b010,0b010}}, {'Z',{0b111,0b001,0b010,0b100,0b111}},
    {'0',{0b111,0b101,0b101,0b101,0b111}}, {'1',{0b010,0b110,0b010,0b010,0b111}}, {'2',{0b111,0b001,0b111,0b100,0b111}},
    {'3',{0b111,0b001,0b111,0b001,0b111}}, {'4',{0b101,0b101,0b111,0b001,0b001}}, {'5',{0b111,0b100,0b111,0b001,0b111}},
    {'6',{0b111,0b100,0b111,0b101,0b111}}, {'7',{0b111,0b001,0b010,0b100,0b100}}, {'8',{0b111,0b101,0b111,0b101,0b111}},
    {'9',{0b111,0b101,0b111,0b001,0b111}}, {':',{0b000,0b010,0b000,0b010,0b000}}, {'+',{0b000,0b010,0b111,0b010,0b000}},
    {'-',{0b000,0b000,0b111,0b000,0b000}}, {'/',{ 0b001,0b001,0b010,0b100,0b100}}, {'.',{0b000,0b000,0b000,0b000,0b010}},
    {'(',{0b010,0b100,0b100,0b100,0b010}}, {')',{0b010,0b001,0b001,0b001,0b010}}, {'!',{0b010,0b010,0b010,0b000,0b010}},
    {'[',{0b110,0b100,0b100,0b100,0b110}}, {']',{0b011,0b001,0b001,0b001,0b011}},
};

static const Glyph3x5* glyph_find(char ch){
    for(unsigned i=0;i<sizeof(font3x5)/sizeof(font3x5[0]);++i)
        if(font3x5[i].ch==ch) return &font3x5[i];
    return &font3x5[0];
}

static void draw_text3x5(volatile u8* dst,int x,int y,const char* s,int scale,u8 col){
    int cx=x, cy=y;
    for(; *s; ++s){
        char ch=*s;
        if(ch=='n' || ch=='\n'){ cy += (5*scale)+scale; cx=x; continue; } 
        const Glyph3x5* g = glyph_find(ch>='a'&&ch<='z'? ch-32: ch);
        for(int ry=0; ry<5; ++ry){
            u8 row = g->r[ry];
            for(int rx=0; rx<3; ++rx){
                if(row & (1<<(2-rx))) fill_rect(dst, cx+rx*scale, cy+ry*scale, scale, scale, col);
            }
        }
        cx += (3*scale) + scale;
    }
}

static void draw_text3x5_both(const char* text, int x, int y, int scale, u8 color){
    draw_text3x5(VRAM_PAGE0, x, y, text, scale, color);
    draw_text3x5(VRAM_PAGE1, x, y, text, scale, color);
}

static void draw_text3x5_centered_both(const char* text, int y, int scale, u8 color){
    int len=0; while(text[len]) len++;
    if(len==0) return;
    int charW = 4*scale;
    int text_w = len * charW;
    int x0 = (W - text_w)/2;
    draw_text3x5_both(text, x0, y, scale, color);
}

static char g_toast_msg[32];
static int g_toast_timer = 0;

static void show_toast(const char* msg) {
    strncpy(g_toast_msg, msg, 31);
    g_toast_msg[31] = '\0';
    g_toast_timer = 90;
}

static void render_toast(void) {
    if(g_toast_timer > 0) {
        int len=0; while(g_toast_msg[len]) len++;
        int scale = 2;
        int w = len * 4 * scale + 4;
        int h = 7 * scale;
        int x = (W - w) / 2;
        int y = H - 35;
        fill_rect(backbuffer, x, y, w, h, 253); 
        draw_text3x5(backbuffer, x+2, y+1, g_toast_msg, scale, 255);
        g_toast_timer--;
    }
}


static void show_controls(void){
    u16 pal[256]; menu_build_rgb332_palette(pal);
    for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    
    const char* pages[][8] = {
        {"MOVEMENT","", "DPAD","MOVE","L R","TURN","",""},
        {"MOVEMENT","", "SEL+L R","LOOK UP DOWN","","","",""},
        {"ACTIONS","", "B","JUMP","A","BREAK BLOCK","",""},
        {"ACTIONS","", "SEL+A","PLACE BLOCK","","","",""},
        {"RENDER","", "SEL+LEFT RIGHT","DISTANCE","SEL+UP DOWN","FOV","",""},
        {"OTHER","", "UPx2","TOGGLE SPRINT","START","PAUSE MENU","",""},
        {"OTHER","", "SEL+START","CYCLE BLOCK","","","",""}
    };
    
    int current_page = 0;
    int total_pages = 7;
    int last_page = -1;
    
    while(1){
        if(current_page != last_page){
            memset_page8(VRAM_PAGE0, DIRT_COLOR);
            memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_centered_both("CONTROLS", 12, 3, MENU_TEXT_WHITE);

            int y = 45;
            for(int i=0; i<8; i++){
                if(pages[current_page][i][0] != '\0'){
                    if(i == 0){
                        draw_text3x5_centered_both(pages[current_page][i], y, 3, MENU_TEXT_WHITE);
                        y += 22;
                    } else if(pages[current_page][i][0]==' ' && pages[current_page][i][1]=='\0'){
                        y += 8;
                    } else if(i % 2 == 0){
                        draw_text3x5_centered_both(pages[current_page][i], y, 2, MENU_TEXT_SELECTED);
                        y += 14;
                    } else {
                        draw_text3x5_centered_both(pages[current_page][i], y, 2, MENU_TEXT_LABEL);
                        y += 16;
                    }
                }
            }

            char page_str[20];
            snprintf(page_str, sizeof(page_str), "PAGE %d/%d", current_page+1, total_pages);
            draw_text3x5_centered_both(page_str, H-25, 2, MENU_TEXT_SELECTED);
            draw_text3x5_centered_both("L R CHANGE   START EXIT", H-12, 2, MENU_TEXT_LABEL);
            last_page = current_page;
        }
        
        vblank();
        scanKeys();
        u16 kd = keysDown();
        if(kd & KEY_START) break;
        if(kd & KEY_L) { current_page--; if(current_page < 0) current_page = total_pages - 1; }
        if(kd & KEY_R) { current_page++; if(current_page >= total_pages) current_page = 0; }
    }
}

static WorldType select_world_type(void){
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int selected = 0, last_selected = -1;
    
    while(1){
        if(selected != last_selected){
            memset_page8(VRAM_PAGE0, DIRT_COLOR);
            memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_centered_both("WORLD TYPE", 35, 3, MENU_TEXT_WHITE);
            draw_text3x5_centered_both("CLASSIC", 80, 3, (selected == 0) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            draw_text3x5_centered_both("SUPERFLAT", 110, 3, (selected == 1) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            last_selected = selected;
        }
        vblank(); scanKeys(); u16 kd = keysDown();
        if(kd & KEY_UP) selected = 0;
        if(kd & KEY_DOWN) selected = 1;
        if(kd & (KEY_A | KEY_START)) return (selected == 0) ? WORLD_CLASSIC : WORLD_SUPERFLAT;
    }
}

static u32 select_seed(void){
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int selected = 0, last_selected = -1;
    
    while(1){
        if(selected != last_selected){
            memset_page8(VRAM_PAGE0, DIRT_COLOR);
            memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_centered_both("SEED TYPE", 35, 3, MENU_TEXT_WHITE);
            draw_text3x5_centered_both("RANDOM", 80, 3, (selected == 0) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            draw_text3x5_centered_both("CUSTOM", 110, 3, (selected == 1) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            last_selected = selected;
        }
        
        vblank(); scanKeys(); u16 kd = keysDown();
        
        if(kd & (KEY_A | KEY_START)){
            if(selected == 0){
                u32 seed = (u32)REG_VCOUNT ^ ((u32)REG_TM0CNT_L << 1);
                return (seed == 0) ? 0xA53F1D77u : seed;
            } else {
                u32 seed = 0; char disp[25] = ""; int cnt = 0, last = -1;
                while(1){
                    if(cnt != last){
                        memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
                        draw_text3x5_centered_both("INPUT SEED", 25, 3, MENU_TEXT_WHITE);
                        draw_text3x5_centered_both("PRESS BUTTONS", 55, 2, MENU_TEXT_LABEL);
                        draw_text3x5_centered_both(disp, 90, 2, MENU_TEXT_SELECTED);
                        draw_text3x5_centered_both("START CONFIRM", 125, 2, MENU_TEXT_LABEL);
                        last = cnt;
                    }
                    vblank(); scanKeys(); u16 kd2 = keysDown();
                    if(kd2 & KEY_START) break;
                    if(cnt < 20){
                        if(kd2 & KEY_UP)    { seed = (seed << 1) ^ 0x01; disp[cnt++] = 'U'; disp[cnt] = 0; }
                        if(kd2 & KEY_DOWN)  { seed = (seed << 1) ^ 0x02; disp[cnt++] = 'D'; disp[cnt] = 0; }
                        if(kd2 & KEY_LEFT)  { seed = (seed << 1) ^ 0x03; disp[cnt++] = 'L'; disp[cnt] = 0; }
                        if(kd2 & KEY_RIGHT) { seed = (seed << 1) ^ 0x04; disp[cnt++] = 'R'; disp[cnt] = 0; }
                        if(kd2 & KEY_A)     { seed = (seed << 1) ^ 0x05; disp[cnt++] = 'A'; disp[cnt] = 0; }
                        if(kd2 & KEY_B)     { seed = (seed << 1) ^ 0x06; disp[cnt++] = 'B'; disp[cnt] = 0; }
                        if(kd2 & KEY_L)     { seed = (seed << 1) ^ 0x07; disp[cnt++] = 'L'; disp[cnt] = 0; }
                        if(kd2 & KEY_R)     { seed = (seed << 1) ^ 0x08; disp[cnt++] = 'R'; disp[cnt] = 0; }
                    }
                }
                return (seed == 0) ? 0xA53F1D77u : seed;
            }
        } else { 
            if(kd & KEY_UP) selected = 0; 
            if(kd & KEY_DOWN) selected = 1; 
        }
    }
}

static MenuResult choose_new_or_load(void){
    MenuResult r = {false,-1};
    u16 pal[256]; menu_build_rgb332_palette(pal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = pal[i];
    int sel = 0, last=-1;
    while(1){
        if(sel!=last){
            memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
            draw_text3x5_centered_both("START GAME", 20, 3, MENU_TEXT_WHITE);
            draw_text3x5_centered_both("NEW WORLD",  70, 3, (sel==0)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED);
            draw_text3x5_centered_both("LOAD WORLD", 100,3, (sel==1)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED);
            draw_text3x5_centered_both("A SELECT   B BACK", H-14, 2, MENU_TEXT_LABEL);
            last=sel;
        }
        vblank(); scanKeys(); u16 kd=keysDown();
        if(kd & KEY_UP)   sel=0; 
        if(kd & KEY_DOWN) sel=1;
        if(kd & KEY_B){ r.start_from_load=false; r.load_slot=-1; return r; }
        if(kd & (KEY_A|KEY_START)){
            if(sel==0){ r.start_from_load=false; r.load_slot=-1; return r; }
            else{
                int slotSel=0, last2=-1;
                while(1){
                    SaveSlotHeader h0,h1; bool v0 = slot_is_valid(0,&h0); bool v1 = slot_is_valid(1,&h1);
                    if(slotSel!=last2){
                        memset_page8(VRAM_PAGE0, DIRT_COLOR); memset_page8(VRAM_PAGE1, DIRT_COLOR);
                        draw_text3x5_centered_both("LOAD WORLD", 20, 3, MENU_TEXT_WHITE);
                        char s0[32], s1[32]; snprintf(s0,sizeof(s0),"SLOT 1  %s", v0?"USED":"EMPTY"); snprintf(s1,sizeof(s1),"SLOT 2  %s", v1?"USED":"EMPTY");
                        draw_text3x5_centered_both(s0, 70, 3, (slotSel==0)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED);
                        draw_text3x5_centered_both(s1,100, 3, (slotSel==1)?MENU_TEXT_WHITE:MENU_TEXT_UNSELECTED);
                        draw_text3x5_centered_both("A LOAD   B BACK", H-14, 2, MENU_TEXT_LABEL);
                        last2=slotSel;
                    }
                    vblank(); scanKeys(); u16 kd2=keysDown();
                    if(kd2 & KEY_UP) slotSel=0; 
                    if(kd2 & KEY_DOWN) slotSel=1;
                    if(kd2 & KEY_B) break;
                    if(kd2 & (KEY_A|KEY_START)){
                        if(slotSel==0 && slot_is_valid(0,NULL)){ r.start_from_load=true; r.load_slot=0; return r; }
                        if(slotSel==1 && slot_is_valid(1,NULL)){ r.start_from_load=true; r.load_slot=1; return r; }
                    }
                }
                last=-1;
            }
        }
    }
}

static void show_menu(MenuResult* out_result){
    u16 basePal[256]; menu_build_rgb332_palette(basePal); for(int i=0;i<256;i++) PAL_BG_MEM[i] = 0;
    menu_blit_image_both_pages(image1);
    for(int s=0;s<=30;s++){ menu_apply_fade(basePal, s, 30); vblank(); }
    for(int t=0;t<60;t++) vblank();
    for(int s=30;s>=0;s--){ menu_apply_fade(basePal, s, 30); vblank(); }
    menu_blit_image_both_pages(image0);
    for(int s=0;s<=30;s++){ menu_apply_fade(basePal, s, 30); vblank(); }
    
    int selected = 0, last_selected = -1;
    while(1){
        if(selected != last_selected){
            menu_blit_image_both_pages(image0);
            draw_text3x5_centered_both("START GAME", H/2 + 20, 3, (selected == 0) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            draw_text3x5_centered_both("CONTROLS", H/2 + 50, 3, (selected == 1) ? MENU_TEXT_WHITE : MENU_TEXT_UNSELECTED);
            last_selected = selected;
        }
        vblank(); scanKeys(); u16 kd = keysDown();
        if(kd & KEY_UP) selected = 0; 
        if(kd & KEY_DOWN) selected = 1;
        if(kd & (KEY_A | KEY_START)){
            if(selected == 0){
                MenuResult r = choose_new_or_load();
                if(!r.start_from_load){
                    WorldType wtype = select_world_type();
                    if(wtype == WORLD_CLASSIC){ g_world_seed = select_seed(); g_world_superflat = false; } 
                    else { g_world_seed = 0; g_world_superflat = true; }
                    g_dimension = 0;
                    out_result->start_from_load = false; out_result->load_slot = -1; break;
                }else{
                    out_result->start_from_load = true; out_result->load_slot = r.load_slot; break;
                }
            } else {
                show_controls(); menu_blit_image_both_pages(image0);
                for(int s=0;s<=30;s++){ menu_apply_fade(basePal, s, 30); vblank(); }
                last_selected = -1; 
            }
        }
    }
    for(int s=30;s>=0;s--){ menu_apply_fade(basePal, s, 30); vblank(); }
    memset_page8(VRAM_PAGE0, 0); memset_page8(VRAM_PAGE1, 0);
}

static PauseResult show_pause_menu(u8* io_curBlk){
    const u8 C_BG=252, C_WHITE=255, C_HILIGHT=254, C_DIM=253;
    const char* items[] = { "RESUME","SAVE SLOT 1","SAVE SLOT 2","MAIN MENU" };
    const int N = (int)(sizeof(items)/sizeof(items[0]));
    int sel = 0, last = -1;
    while(1){
        if(sel != last){
            memset_page8(VRAM_PAGE0, C_BG); memset_page8(VRAM_PAGE1, C_BG);
            draw_text3x5_centered_both("PAUSED", 20, 3, C_WHITE);
            int y = 50;
            for(int i=0;i<N;i++){
                u8 c = (i==sel)?C_WHITE:C_DIM; draw_text3x5_centered_both(items[i], y, 3, c); y += 20;
            }
            draw_text3x5_centered_both("A SELECT   B/START RESUME", H-14, 2, C_HILIGHT);
            last = sel;
        }
        vblank(); scanKeys(); u16 kd = keysDown();
        if(kd & KEY_UP){ sel--; if(sel<0) sel=N-1; } if(kd & KEY_DOWN){ sel++; if(sel>=N) sel=0; }
        if((kd & KEY_B) || (kd & KEY_START)) break;
        if(kd & KEY_A){
            if(sel == 0){ break; } 
            else if(sel == 1 || sel == 2){
                int slot = (sel==1)?0:1;
                bool ok = save_world_and_verify(slot, *io_curBlk);
                if(ok) draw_text3x5_centered_both(slot==0?"SAVED SLOT 1":"SAVED SLOT 2", H-36, 2, C_HILIGHT);
                else   draw_text3x5_centered_both("SAVE FAILED", H-36, 2, C_HILIGHT);
                for(int t=0;t<60;t++) vblank();
                last = -1; 
            } else if(sel == 3){ return PM_TO_MAINMENU; }
        }
    }
    return PM_RESUME;
}


static IWRAM_CODE ARM_CODE void render_textured(u8 selected_block){
    if(g_vis_dist < 2) g_vis_dist = 2;
    if(g_vis_dist > 20) g_vis_dist = 20;
    if(g_focal < F(50.0f)) g_focal = F(50.0f);
    if(g_focal > F(250.0f)) g_focal = F(250.0f);
    
    draw_sky_gradient();

    v3 fx, fy, fz;
    if(!cam_basis(&fx, &fy, &fz)) return;
    v3 cam_pos = { pl.pos.x, pl.pos.y + F(1.5f), pl.pos.z };

    faceCount = 0;
    
    int cx = F2I(pl.pos.x);
    int cy = F2I(pl.pos.y + F(0.5f));
    int cz = F2I(pl.pos.z);
    
    int xmin = cx - g_vis_dist; if(xmin < 0) xmin = 0;
    int xmax = cx + g_vis_dist; if(xmax >= CW) xmax = CW-1;
    int ymin = cy - g_vis_dist; if(ymin < 0) ymin = 0;
    int ymax = cy + g_vis_dist; if(ymax >= CH) ymax = CH-1;
    int zmin = cz - g_vis_dist; if(zmin < 0) zmin = 0;
    int zmax = cz + g_vis_dist; if(zmax >= CD) zmax = CD-1;

    for(int y=ymin; y<=ymax && faceCount < MAX_FACES; y++){
        for(int z=zmin; z<=zmax && faceCount < MAX_FACES; z++){
            for(int x=xmin; x<=xmax && faceCount < MAX_FACES; x++){
                if(!in_view_frustum(I2F(x)+F(0.5f), I2F(y)+F(0.5f), I2F(z)+F(0.5f), &fz, &fx, &fy, &cam_pos))
                    continue;

                u8 id = world[widx(x,y,z)];
                if(!id) continue;
                u8 blk = id - 1;
                if(blk >= BLK_COUNT) continue;
                
                emit_face_if_visible(x,y,z, blk, FACE_POSX, &fz, &cam_pos);
                emit_face_if_visible(x,y,z, blk, FACE_NEGX, &fz, &cam_pos);
                emit_face_if_visible(x,y,z, blk, FACE_POSY, &fz, &cam_pos);
                emit_face_if_visible(x,y,z, blk, FACE_NEGY, &fz, &cam_pos);
                emit_face_if_visible(x,y,z, blk, FACE_POSZ, &fz, &cam_pos);
                emit_face_if_visible(x,y,z, blk, FACE_NEGZ, &fz, &cam_pos);
            }
        }
    }

    if(faceCount > 1) sort_faces_insertion(faceBuf, faceCount);

    for(int i=0;i<faceCount;i++){
        draw_block_face_textured(&faceBuf[i], &fx,&fy,&fz, &cam_pos);
    }

    u8 cross_idx = 253;
    int cxp = W/2, cyp = H/2;
    for(int i=-4;i<=4;i++){
        if(i==0) continue;
        backbuffer[cyp*W + (cxp+i)] = cross_idx;
        backbuffer[(cyp+i)*W + cxp] = cross_idx;
    }
    
    draw_block_bar(selected_block);
    render_toast();
}

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
    
    int w = max_x - min_x + 1;
    int h = max_y - min_y + 1;
    
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
            for(int iy=min_y; iy<=max_y; iy++)
                for(int ix=min_x; ix<=max_x; ix++)
                    world_set_and_log(ix, iy, z, BLK_PORTAL+1);
            show_toast("PORTAL IGNITED!");
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
    
    w = max_z - min_z + 1;
    h = max_y - min_y + 1;
    
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
            for(int iy=min_y; iy<=max_y; iy++)
                for(int iz=min_z; iz<=max_z; iz++)
                    world_set_and_log(x, iy, iz, BLK_PORTAL+1);
            show_toast("PORTAL IGNITED!");
            return;
         }
    }
}

static void build_portal_structure(int x, int y, int z) {
    for(int dx=-2; dx<=3; dx++) {
        for(int dz=-2; dz<=2; dz++) {
            int id = world[widx(x+dx, y-1, z+dz)];
            if (id == 0 || id == (BLK_LAVA+1) || id == (BLK_WATER+1)) {
                world_set_and_log(x+dx, y-1, z+dz, g_dimension == 1 ? (BLK_NETHERRACK+1) : (BLK_STONE+1));
            }
            for(int h=0; h<4; h++) {
                 world_set_and_log(x+dx, y+h, z+dz, 0);
            }
        }
    }
    world_set_and_log(x, y-1, z, BLK_OBSIDIAN+1);
    world_set_and_log(x+1, y-1, z, BLK_OBSIDIAN+1);
    world_set_and_log(x, y+3, z, BLK_OBSIDIAN+1);
    world_set_and_log(x+1, y+3, z, BLK_OBSIDIAN+1);
    for(int h=0; h<3; h++) {
        world_set_and_log(x-1, y+h, z, BLK_OBSIDIAN+1);
        world_set_and_log(x+2, y+h, z, BLK_OBSIDIAN+1);
    }
    for(int h=0; h<3; h++) {
        world_set_and_log(x, y+h, z, BLK_PORTAL+1);
        world_set_and_log(x+1, y+h, z, BLK_PORTAL+1);
    }
}

static void switch_dimension(u8 curBlk) {
    show_toast(g_dimension == 0 ? "ENTERING NETHER..." : "RETURNING...");
    render_textured(BLK_PORTAL+1); 
    flip(); 
    for(int i=0;i<60;i++) vblank();
    
    save_world_to_slot_raw(g_dimension, curBlk);
    
    int target_dim = (g_dimension == 0) ? 1 : 0;
    
    bool loaded = load_world_from_slot(target_dim, NULL);
    
    if (!loaded) {
        g_dimension = target_dim;
        buildPalette();
        editlog_clear();
        gen_world();
        
        int spawn_x = CW/2;
        int spawn_z = CD/2;
        int spawn_y = CH-1;
        
        if (g_dimension == 1) {
             spawn_y = CH/2;
             while(spawn_y > 5 && world[widx(spawn_x, spawn_y-1, spawn_z)] == 0) spawn_y--;
             if(world[widx(spawn_x, spawn_y, spawn_z)] != 0) spawn_y = 16;
        } else {
            if(g_world_superflat){ spawn_y=11; }
            else{
                for(int y=CH-1;y>=0;y--){
                    u8 id = world[widx(spawn_x,y,spawn_z)];
                    if(id){ u8 t = id-1; if(t!=BLK_WATER && t!=BLK_LEAF && t!=BLK_LAVA){ spawn_y=y+1; break; } }
                }
                if(spawn_y < 10) spawn_y=25;
            }
        }
        
        build_portal_structure(spawn_x, spawn_y, spawn_z);
        
        pl.pos.x=I2F(spawn_x) + F(0.5f);
        pl.pos.y=I2F(spawn_y); 
        pl.pos.z=I2F(spawn_z) + F(2.0f);
        pl.vel.x=0; pl.vel.y=0; pl.vel.z=0;
    }
}

IWRAM_CODE ARM_CODE void mainloop(MenuResult menu_res) {
    u8 curBlk = BLK_GRASS + 1;
    bool loaded_from_slot = false;
    if(menu_res.start_from_load){
        if(load_world_from_slot(menu_res.load_slot, &curBlk)){
            loaded_from_slot = true;
        } else { gen_world(); editlog_clear(); }
    } else { gen_world(); editlog_clear(); }

    if(!loaded_from_slot){
        int spawn_x = CW/2;
        int spawn_z = CD/2;
        int spawn_y = CH - 1;
        if(g_world_superflat){
            spawn_y = 11;
        } else {
            for(int y = CH-1; y >= 0; y--){
                u8 block_id = world[widx(spawn_x, y, spawn_z)];
                if(block_id){
                    u8 t = block_id - 1;
                    if(t != BLK_WATER && t != BLK_LEAF && t != BLK_LAVA){
                        spawn_y = y + 1;
                        break;
                    }
                }
            }
            if(spawn_y < 10) spawn_y = 25;
        }
        pl.pos.x = I2F(spawn_x);
        pl.pos.y = I2F(spawn_y);
        pl.pos.z = I2F(spawn_z);
        pl.yaw = LUT_N/2;
        pl.pitch = 0;
        pl.vel.x = pl.vel.y = pl.vel.z = 0;
        pl.onGround = false;
    }

    u32 frame = 0;
    u8 tuning_cooldown = 0;
    u8 action_cooldown = 0;
    bool select_was_held = false;

    while(1){
        vblank();
        g_frame = frame;
        
        scanKeys();
        u16 kd_early = keysDown();
        u16 kh_early = keysHeld();
        bool select_held_early = (kh_early & KEY_SELECT) != 0;

        if((kd_early & KEY_START) && !select_held_early){
            PauseResult pr = show_pause_menu(&curBlk);
            if(pr == PM_TO_MAINMENU){
                MenuResult r2 = {false,-1}; 
                show_menu(&r2); 
                g_dimension = 0;
                buildPalette();
                bool loaded = false;
                if(r2.start_from_load){ if(load_world_from_slot(r2.load_slot, &curBlk)) loaded = true; }
                if(!loaded){
                    gen_world(); editlog_clear();
                    int sx = CW/2, sz = CD/2, sy = CH-1;
                    if(g_world_superflat){ sy=11; }
                    else{
                        for(int y=CH-1;y>=0;y--){
                            u8 id = world[widx(sx,y,sz)];
                            if(id){ u8 t = id-1; if(t!=BLK_WATER && t!=BLK_LEAF && t!=BLK_LAVA){ sy=y+1; break; } }
                        }
                        if(sy < 10) sy=25;
                    }
                    pl.pos.x=I2F(sx); pl.pos.y=I2F(sy); pl.pos.z=I2F(sz);
                    pl.yaw=LUT_N/2; pl.pitch=0; pl.vel.x=pl.vel.y=pl.vel.z=0; pl.onGround=false;
                    curBlk = BLK_GRASS+1; g_is_sprinting = false;
                }
            }
            frame++; 
            continue;
        }

        if(tuning_cooldown) tuning_cooldown--;
        if(action_cooldown) action_cooldown--;

        handle_input();

        u16 kh = keysHeld();
        bool select_held = (kh & KEY_SELECT);
        bool select_mode_changed = (select_held != select_was_held);
        select_was_held = select_held;

        if(select_held && tuning_cooldown == 0){
            if(kh & KEY_B){
                g_vis_dist = 6;
                g_focal = F(130.0f);
                g_frustum_margin = F(1.2f);
                g_near_z = F(0.25f);
                tuning_cooldown = 15;
            }
            else if(kh & KEY_A){
                if((kh & KEY_LEFT) && g_frustum_margin > F(0.8f)) { g_frustum_margin -= F(0.1f); tuning_cooldown=3; }
                if((kh & KEY_RIGHT) && g_frustum_margin < F(2.0f)) { g_frustum_margin += F(0.1f); tuning_cooldown=3; }
            } else {
                if((kh & KEY_LEFT) && g_vis_dist > 4) { g_vis_dist--; if(g_vis_dist<4) g_vis_dist=4; tuning_cooldown=3; }
                if((kh & KEY_RIGHT) && g_vis_dist < 16) { g_vis_dist++; if(g_vis_dist>16) g_vis_dist=16; tuning_cooldown=3; }
                if(kh & KEY_UP)   { if(g_focal < F(220.0f)) g_focal += F(10.0f); tuning_cooldown=3; }
                if(kh & KEY_DOWN) { if(g_focal > F(70.0f))  g_focal -= F(10.0f); tuning_cooldown=3; }
            }
        }

        v3 fwd; {
            fix sy = fsin(pl.yaw), cy = fcos(pl.yaw);
            fix sp = fsin(pl.pitch), cp = fcos(pl.pitch);
            fwd.x = fmuli(sy, cp); fwd.y = -sp; fwd.z = fmuli(cy, cp);
            if(fwd.x==0 && fwd.y==0 && fwd.z==0) fwd.z=FONE;
        }
        v3 cam_pos = { pl.pos.x, pl.pos.y + F(1.5f), pl.pos.z };

        if((kh & KEY_A) && !select_held && !select_mode_changed && action_cooldown == 0){
            fix step = F(0.25f);
            v3 p = cam_pos;
            for(int s=0; s<g_vis_dist*8; s++){
                p.x += fmuli(fwd.x, step);
                p.y += fmuli(fwd.y, step);
                p.z += fmuli(fwd.z, step);
                int bx = F2I(p.x), by = F2I(p.y), bz = F2I(p.z);
                if(bx<0||bx>=CW||by<0||by>=CH||bz<0||bz>=CD) break;
                u8 block_id = world[widx(bx,by,bz)];
                if(block_id){
                    if (curBlk == ITEM_FLINT_STEEL) {
                        try_ignite_portal_rect(bx,by,bz);
                    } else {
                        world_set_and_log(bx,by,bz, 0);
                    }
                    action_cooldown = 8;
                    break;
                }
            }
        }

        if((kh & KEY_A) && select_held && !select_mode_changed && action_cooldown == 0){
            if (curBlk == ITEM_FLINT_STEEL) {
            } else {
                fix step = F(0.25f);
                v3 p = cam_pos;
                int last_air_x=-1,last_air_y=-1,last_air_z=-1;
                for(int s=0; s<g_vis_dist*8; s++){
                    p.x += fmuli(fwd.x, step);
                    p.y += fmuli(fwd.y, step);
                    p.z += fmuli(fwd.z, step);
                    int bx = F2I(p.x), by = F2I(p.y), bz = F2I(p.z);
                    if(bx<0||bx>=CW||by<0||by>=CH||bz<0||bz>=CD) break;
                    u8 block_id = world[widx(bx,by,bz)];
                    if(block_id){
                        if(last_air_x>=0)
                            world_set_and_log(last_air_x,last_air_y,last_air_z, curBlk);
                        action_cooldown = 8;
                        break;
                    }else{
                        last_air_x=bx; last_air_y=by; last_air_z=bz;
                    }
                }
            }
        }

        if(select_held && (kh & KEY_START) && action_cooldown == 0){
            int buildable[] = {BLK_GRASS, BLK_DIRT, BLK_STONE, BLK_WOOD, BLK_BRICK, BLK_PLANK, BLK_COBBLE, BLK_SAND, BLK_LEAF, BLK_OBSIDIAN, BLK_NETHERRACK, BLK_LAVA, BLK_WATER, ITEM_FLINT_STEEL};
            const char* names[] = {"GRASS", "DIRT", "STONE", "WOOD", "BRICK", "PLANK", "COBBLE", "SAND", "LEAF", "OBSIDIAN", "NETHERRACK", "LAVA", "WATER", "FLINT N STEEL"};
            int count = 14;
            int cur_idx = -1; 
            for(int i=0; i<count; i++){ 
                int check = (buildable[i] == ITEM_FLINT_STEEL) ? ITEM_FLINT_STEEL : buildable[i]+1;
                if(check == curBlk) { cur_idx = i; break; } 
            }
            cur_idx = (cur_idx + 1) % count; 
            if (buildable[cur_idx] == ITEM_FLINT_STEEL) curBlk = ITEM_FLINT_STEEL;
            else curBlk = buildable[cur_idx]+1;
            
            show_toast(names[cur_idx]);
            action_cooldown = 10;
        }

        physics_step(curBlk);
        render_textured(curBlk);
        flip();
        frame++;
    }
}

int main(void){
    irqInit();
    irqEnable(IRQ_VBLANK);
    REG_DISPCNT = MODE_4 | BG2_ENABLE | BACKBUFFER;

    keep_save_id();

    MenuResult menu_res = {false,-1};
    show_menu(&menu_res);

    build_luts();
    buildPalette();
    generate_textures();
    
    mainloop(menu_res);
}
