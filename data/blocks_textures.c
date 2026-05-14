/* ===================== BLOCKS & PALETTE ==================== */
enum { 
    BLK_GRASS=0, BLK_DIRT, BLK_STONE, BLK_WATER, BLK_WOOD, BLK_LEAF, 
    BLK_SAND, BLK_BRICK, BLK_PLANK, BLK_COBBLE, BLK_BEDROCK,
    BLK_OBSIDIAN, BLK_NETHERRACK, BLK_PORTAL, BLK_LAVA,
    BLK_GRAVEL, BLK_CLAY, BLK_MOSSY_COBBLE, BLK_NETHER_BRICK, BLK_BASALT,
    BLK_CACTUS, BLK_GLASS, BLK_WOOD_DARK, BLK_WOOD_LIGHT, BLK_LEAF_DARK, BLK_LEAF_LIGHT,
    BLK_GRASS_SIDE, BLK_ORE_IRON, BLK_ORE_GOLD, BLK_ORE_DIAMOND,
    BLK_TRACK, BLK_COBWEB, BLK_TRACK_SIDE, BLK_TNT, BLK_TNT_LIT,
    BLK_COUNT 
};

#define ITEM_NONE 0
#define ITEM_FLINT_STEEL 100
#define ITEM_STICK 101
#define ITEM_PICK_WOOD 102
#define ITEM_PICK_STONE 103
#define ITEM_PICK_IRON 104
#define ITEM_FLINT 105
#define SHADES_PER_BLOCK 8
#define BLOCK_PALETTE_BLOCKS (BLK_GRASS_SIDE + 1)
#define QUICKBAR_SLOTS 5
#define HOTBAR_SLOTS 6
#define SURVIVAL_INV_SLOTS 12
#define DIMENSION_COUNT 2

static const u8 g_buildable_blocks[] = {
    BLK_GRASS, BLK_DIRT, BLK_STONE, BLK_ORE_IRON, BLK_ORE_GOLD, BLK_ORE_DIAMOND,
    BLK_WOOD, BLK_BRICK, BLK_PLANK, BLK_COBBLE, BLK_MOSSY_COBBLE,
    BLK_GRAVEL, BLK_CLAY, BLK_SAND, BLK_CACTUS, BLK_LEAF, BLK_LEAF_DARK, BLK_LEAF_LIGHT, BLK_WOOD_DARK, BLK_WOOD_LIGHT,
    BLK_GLASS, BLK_OBSIDIAN, BLK_TNT, BLK_NETHER_BRICK, BLK_NETHERRACK, BLK_BASALT,
    BLK_LAVA, BLK_WATER, ITEM_FLINT_STEEL
};
static const char* g_buildable_names[] = {
    "GRASS", "DIRT", "STONE", "IRON ORE", "GOLD ORE", "DIAM ORE",
    "WOOD", "BRICK", "PLANK", "COBBLE", "MOSSY COB",
    "GRAVEL", "CLAY", "SAND", "CACTUS", "LEAF", "LEAF DARK", "LEAF LIGHT", "WOOD DARK", "WOOD LIGHT",
    "GLASS", "OBSIDIAN", "TNT", "NETHER BR", "NETHERRACK", "BASALT",
    "LAVA", "WATER", "FLINT N STEEL"
};
#define BUILDABLE_COUNT ((int)(sizeof(g_buildable_blocks)/sizeof(g_buildable_blocks[0])))

static const u16 palBaseColors[BLK_COUNT] = {
    RGB5(3, 13, 3),   RGB5(14, 9, 4),   RGB5(12, 12, 12), RGB5(4, 8, 24),
    RGB5(16, 11, 6),  RGB5(4, 18, 4),   RGB5(24, 23, 14), RGB5(20, 8, 6),
    RGB5(22, 16, 8),  RGB5(12, 12, 12), RGB5(5, 5, 5),    RGB5(6, 4, 10),
    RGB5(18, 6, 6),   RGB5(28, 5, 31),  RGB5(31, 15, 0),  RGB5(15, 15, 15),
    RGB5(12, 16, 18), RGB5(8, 12, 7),   RGB5(12, 3, 3),   RGB5(7, 7, 8),
    RGB5(7, 14, 6),   RGB5(20, 24, 27), RGB5(10, 6, 3),   RGB5(24, 18, 10),
    RGB5(3, 10, 3),   RGB5(6, 20, 6),   RGB5(10, 7, 4),
    RGB5(18, 13, 9),  RGB5(27, 22, 6),  RGB5(8, 24, 26),
    RGB5(20, 16, 8),  RGB5(24, 24, 24), RGB5(15, 10, 6),
    RGB5(24, 5, 5),   RGB5(31, 28, 20)
};

#define SKY_COLORS 8
#define SKY_START_IDX ((BLK_GRASS_SIDE + 1) * 8)
#define VOID_START_IDX (SKY_START_IDX + SKY_COLORS)

#define TEXTURE_SIZE 16
#define TEXTURE_PIXELS (TEXTURE_SIZE * TEXTURE_SIZE)
#define SRAM_BASE ((volatile u8*)0x0E000000)
#define TEXPACK_SRAM_OFF 0x4000u
#define TEXPACK_SRAM_SIZE 0x4000u
#define TEXPACK_MAGIC 0x4B505447u /* 'GTPK' */
#define TEXPACK_VERSION 1u
#define TEXPACK_VERSION_PALETTES 2u
#define TEXPACK_NAME_LEN 16
#define TEXTURE_PACKED_BYTES (TEXTURE_PIXELS / 2)
#define TEXTURE_CACHE_SLOTS 28
#define TEXTURE_CACHE_EMPTY 0xFFu
#define TEXPACK_RECORD_V1_SIZE (2u + TEXTURE_PIXELS)
#define TEXPACK_RECORD_V2_SIZE (2u + SHADES_PER_BLOCK * 2u + TEXTURE_PIXELS)
#define TEXPACK_TNT_START_IDX (VOID_START_IDX + SKY_COLORS)

static EWRAM_DATA u8 textures[BLK_COUNT][TEXTURE_PACKED_BYTES];
static EWRAM_DATA u16 texture_pack_palettes[BLK_COUNT][SHADES_PER_BLOCK];
static EWRAM_DATA u8 texture_pack_palette_valid[BLK_COUNT];
static EWRAM_DATA u8 shaded_texture_cache[TEXTURE_CACHE_SLOTS][TEXTURE_PIXELS] __attribute__((aligned(4)));
static EWRAM_DATA u8 shaded_texture_lookup[BLK_COUNT][4];
static EWRAM_DATA u8 shaded_texture_cache_block[TEXTURE_CACHE_SLOTS];
static EWRAM_DATA u8 shaded_texture_cache_light[TEXTURE_CACHE_SLOTS];
static EWRAM_DATA u16 shaded_texture_cache_age[TEXTURE_CACHE_SLOTS];
static u16 g_texture_cache_tick = 1;
static bool g_use_16x16_textures = true;
static bool g_texture_pack_enabled = true;
static bool g_sram_texture_pack_present = false;
static bool g_sram_texture_pack_loaded = false;
static char g_sram_texture_pack_name[TEXPACK_NAME_LEN + 1] = "NONE";

static const u8 texture_data_rom[BLK_COUNT][64] = {
    {3,3,2,3,3,2,3,3,2,3,4,2,3,3,2,4,3,2,3,2,3,3,2,3,3,4,2,3,3,2,3,2,2,3,2,4,2,3,3,2,4,2,3,2,3,3,2,3,3,2,3,3,3,2,2,3,3,3,2,4,2,3,3,2},
    {2,3,1,4,2,3,1,3,3,1,3,2,4,1,3,2,1,4,2,3,1,3,2,3,3,2,3,1,3,4,1,3,3,1,3,2,3,4,1,2,4,3,1,3,2,3,2,1,2,3,4,1,3,2,3,4,1,3,2,3,4,1,3,2},
    {1,1,3,3,3,1,1,2,2,2,3,3,3,2,2,1,1,3,3,3,1,1,2,2,2,3,3,3,2,2,1,1,3,3,3,1,1,2,2,3,3,3,2,2,1,1,3,3,1,1,1,2,2,3,3,3,1,1,2,2,3,3,1,1},
    {3,3,1,1,3,3,1,1,1,3,3,1,1,3,3,1,1,1,3,3,1,1,3,3,3,1,1,3,3,1,1,3,3,3,1,1,3,3,1,1,1,3,3,1,1,3,3,1,1,1,3,3,1,1,3,3,3,1,1,3,3,1,1,3},
    {1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2},
    {2,2,2,3,2,2,1,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,2,2,2,2,2,2,2,2,1,1,2,2,2,3,2,2,2,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,3,2,2,2,1,2,2,2},
    {3,2,3,4,2,3,3,2,3,3,2,3,4,2,3,3,2,3,4,3,2,3,3,2,4,2,3,3,2,3,4,2,3,4,2,3,3,2,3,4,2,3,3,2,3,3,2,3,3,2,3,3,4,2,3,3,3,4,2,3,3,2,3,2},
    {3,3,3,0,3,3,3,3,3,3,3,0,3,3,3,3,3,3,3,0,3,3,3,3,0,0,0,0,0,0,0,0,3,3,3,3,3,3,3,0,3,3,3,3,3,3,3,0,3,3,3,3,3,3,3,0,0,0,0,0,0,0,0,0},
    {1,1,1,1,1,1,1,1,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,1,1,1,1,1,1,1,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,2,2,1,1,1,1,1,1,1,1},
    {2,1,3,2,1,3,2,1,3,2,1,2,3,1,2,3,1,3,2,3,1,2,3,2,2,1,3,1,2,3,1,2,3,2,1,2,3,2,1,3,1,3,2,3,1,2,3,1,2,1,3,1,2,3,2,1,3,2,1,2,3,1,2,3},
    {0,4,1,5,0,3,2,5,4,1,5,0,2,5,1,3,5,2,0,3,5,1,4,0,1,5,2,4,0,3,5,2,0,3,5,1,4,0,2,5,3,0,4,2,5,1,0,4,4,1,2,5,0,3,2,1,5,2,0,3,1,4,2,0},
    {1,2,1,1,1,2,1,1,1,1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,1,1,2,1,1,2,1,1,1,2,1,1,1,1,2,1,1,1,2,1,2,1,1,1,2,1,1,1,1,1,2,1,1,1,2,1},
    {2,1,3,2,1,3,2,1,3,2,1,2,3,1,2,3,1,3,2,3,1,2,3,2,2,1,3,1,2,3,1,2,3,2,1,2,3,2,1,3,1,3,2,3,1,2,3,1,2,1,3,1,2,3,2,1,3,2,1,2,3,1,2,3},
    {2,3,4,3,2,3,4,3,3,4,3,2,3,4,3,2,4,3,2,3,4,3,2,3,3,2,3,4,3,2,3,4,2,3,4,3,2,3,4,3,3,4,3,2,3,4,3,2,4,3,2,3,4,3,2,3,3,2,3,4,3,2,3,4},
    {1,4,6,4,1,4,6,4,6,2,0,2,6,4,2,4,4,0,2,0,4,6,1,6,4,6,4,6,4,6,4,6,1,4,6,4,1,4,6,4,6,2,0,2,6,4,2,4,4,0,2,0,4,6,1,6,4,6,4,6,4,6,4,6},
    {2,2,3,2,1,3,2,2,3,2,2,3,2,2,3,1,2,3,2,2,4,2,2,3,2,1,2,3,2,2,3,2,3,2,3,2,1,3,2,2,2,3,2,4,2,2,3,2,2,2,3,2,3,2,1,2,3,2,2,3,2,3,2,2},
    {1,1,2,2,2,1,1,2,1,2,2,1,2,2,1,1,2,2,1,1,2,2,2,1,2,1,1,2,2,1,2,2,1,2,2,2,1,1,2,2,1,1,2,2,1,2,2,1,2,2,1,2,2,1,1,2,2,1,2,2,1,2,2,1},
    {2,1,3,1,2,1,3,1,1,3,2,1,2,3,1,2,2,1,4,2,1,3,2,1,1,2,1,3,2,1,4,2,2,3,1,2,3,1,2,1,1,2,3,1,2,4,1,3,3,1,2,1,3,2,1,2,1,3,1,2,1,3,2,1},
    {3,3,3,3,1,1,1,1,3,4,3,3,1,2,1,1,0,0,0,0,0,0,0,0,1,1,1,1,3,3,3,3,1,2,1,1,3,4,3,3,0,0,0,0,0,0,0,0,3,3,3,3,1,1,1,1,3,4,3,3,1,2,1,1},
    {3,2,3,2,3,2,3,2,2,3,2,3,2,3,2,3,3,2,2,3,2,3,2,2,2,3,2,2,3,2,3,2,3,2,3,2,2,3,2,3,2,2,3,2,3,2,2,3,3,2,2,3,2,3,2,2,3,2,3,2,3,2,3,2},
    {1,1,0,1,1,0,1,1,1,2,1,0,1,1,0,1,0,1,2,1,0,1,1,0,1,0,1,2,1,0,1,1,1,1,0,1,2,1,0,1,0,1,1,0,1,2,1,0,1,0,1,1,0,1,2,1,1,1,0,1,1,0,1,2},
    {1,1,0,0,1,1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,1,0,1},
    {1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2},
    {1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2,1,2,3,2,3,2,1,2},
    {2,2,2,3,2,2,1,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,2,2,2,2,2,2,2,2,1,1,2,2,2,3,2,2,2,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,3,2,2,2,1,2,2,2},
    {2,2,2,3,2,2,1,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,2,2,2,2,2,2,2,2,1,1,2,2,2,3,2,2,2,2,2,1,2,2,2,3,2,2,2,3,2,2,2,1,2,3,2,2,2,1,2,2,2},
    {5,5,5,5,5,5,5,5,5,0,5,0,5,5,0,5,1,0,0,1,0,0,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,1,0,0,0,0,0,0,1,0,0,0,1,0,0,1,0,1,0,1,0,0,1,0,0,0,1},
    {1,1,3,3,3,1,1,2,2,2,3,1,3,2,2,1,1,3,3,5,6,3,2,2,2,3,5,3,7,6,3,1,2,3,6,7,5,6,3,1,1,2,3,5,6,3,2,2,1,1,2,3,3,2,1,1,2,1,1,2,2,1,1,2},
    {1,1,3,3,3,1,1,2,2,2,3,1,3,2,2,1,1,3,3,5,6,3,2,2,2,3,5,3,3,6,3,1,2,3,6,3,7,5,3,1,1,2,3,5,6,3,2,2,1,1,2,3,3,2,1,1,2,1,1,2,2,1,1,2},
    {1,1,3,3,3,1,1,2,2,2,3,1,3,2,2,1,1,3,3,6,7,3,2,2,2,3,6,5,7,6,3,1,2,3,7,6,6,5,3,1,1,2,3,6,7,3,2,2,1,1,2,3,3,2,1,1,2,1,1,2,2,1,1,2},
    {1,5,1,2,1,5,1,2,6,2,6,2,6,2,6,2,1,5,1,2,1,5,1,2,6,2,6,2,6,2,6,2,1,5,1,2,1,5,1,2,6,2,6,2,6,2,6,2,1,5,1,2,1,5,1,2,6,2,6,2,6,2,6,2},
    {7,0,0,0,0,0,0,7,0,7,0,0,0,0,7,0,0,0,7,0,0,7,0,0,0,0,0,7,7,0,0,0,0,0,0,7,7,0,0,0,0,0,7,0,0,7,0,0,0,7,0,0,0,0,7,0,7,0,0,0,0,0,0,7},
    {1,1,2,2,2,1,1,1,2,3,1,1,1,2,3,1,1,2,3,1,1,2,3,1,1,1,2,3,1,1,2,2,2,1,1,1,2,3,1,1,1,2,3,1,1,2,3,1,1,2,3,1,1,1,2,3,1,1,2,2,2,1,1,1},
    {5,4,6,4,5,4,6,4,5,4,6,4,5,4,6,4,4,3,6,3,4,3,6,3,0,0,1,0,0,1,0,0,1,0,1,0,1,1,1,0,4,3,6,3,4,3,6,3,5,4,6,4,5,4,6,4,4,3,6,3,4,3,6,3},
    {7,5,6,5,7,5,6,5,7,5,6,5,7,5,6,5,5,4,6,4,5,4,6,4,0,0,1,0,0,1,0,0,1,0,1,0,1,1,1,0,5,4,6,4,5,4,6,4,7,5,6,5,7,5,6,5,5,4,6,4,5,4,6,4}
};

static inline u16 shadeRGB15(u16 c, int light){
    int r = (c & 31), g = ((c>>5) & 31), b = ((c>>10)&31);
    int num = 4 + light * 2; 
    int den = 16;
    r = (r * num) / den; if(r>31) r=31;
    g = (g * num) / den; if(g>31) g=31;
    b = (b * num) / den; if(b>31) b=31;
    return (u16)(r | (g<<5) | (b<<10));
}

static int g_dimension = 0; 

static void buildPalette(void){
    for(int b=0; b<BLOCK_PALETTE_BLOCKS; b++){
        for(int s=0; s<SHADES_PER_BLOCK; s++){
            if(texture_pack_palette_valid[b]){
                PAL_BG_MEM[b*8 + s] = texture_pack_palettes[b][s];
            } else if (b == BLK_GRASS_SIDE) {
                if (s <= 4) PAL_BG_MEM[b*8 + s] = shadeRGB15(RGB5(14, 9, 4), s*2 + 2); 
                else PAL_BG_MEM[b*8 + s] = shadeRGB15(RGB5(3, 13, 3), (s - 5) * 4 + 4);
            } else if (b == BLK_ORE_IRON) {
                static const u16 iron_pal[8] = {
                    RGB5(7,7,8), RGB5(10,10,11), RGB5(13,13,14), RGB5(16,16,17),
                    RGB5(20,20,21), RGB5(22,18,5), RGB5(27,22,6), RGB5(31,26,9)
                };
                PAL_BG_MEM[b*8 + s] = iron_pal[s];
            } else if (b == BLK_ORE_GOLD) {
                static const u16 gold_pal[8] = {
                    RGB5(7,7,8), RGB5(10,10,11), RGB5(13,13,14), RGB5(16,16,17),
                    RGB5(20,20,21), RGB5(17,12,9), RGB5(22,16,12), RGB5(26,20,16)
                };
                PAL_BG_MEM[b*8 + s] = gold_pal[s];
            } else if (b == BLK_ORE_DIAMOND) {
                static const u16 diamond_pal[8] = {
                    RGB5(7,7,8), RGB5(10,10,11), RGB5(13,13,14), RGB5(16,16,17),
                    RGB5(20,20,21), RGB5(8,20,24), RGB5(12,26,30), RGB5(18,31,31)
                };
                PAL_BG_MEM[b*8 + s] = diamond_pal[s];
            } else if (b == BLK_PORTAL) {
                int bright = 12 + s*2; if(bright>31) bright=31;
                PAL_BG_MEM[b*8 + s] = RGB5(bright, 6, bright); 
            } else if (b == BLK_LAVA) {
                if (s <= 2) PAL_BG_MEM[b*8 + s] = RGB5(25 + s*2, 4 + s*2, 0); 
                else {
                    int r = 31, g = 12 + (s-3)*4; if(g>31) g=31;
                    PAL_BG_MEM[b*8 + s] = RGB5(r,g,0);
                }
            } else if (b == BLK_TNT) {
                static const u16 tnt_pal[8] = {
                    RGB5(2,2,2), RGB5(31,30,24), RGB5(25,23,18), RGB5(10,0,0),
                    RGB5(21,1,1), RGB5(29,3,3), RGB5(5,0,0), RGB5(31,31,31)
                };
                PAL_BG_MEM[b*8 + s] = tnt_pal[s];
            } else if (b == BLK_TNT_LIT) {
                static const u16 tnt_lit_pal[8] = {
                    RGB5(4,4,4), RGB5(31,31,29), RGB5(30,26,20), RGB5(18,2,2),
                    RGB5(28,7,7), RGB5(31,12,8), RGB5(8,0,0), RGB5(31,31,31)
                };
                PAL_BG_MEM[b*8 + s] = tnt_lit_pal[s];
            } else {
                PAL_BG_MEM[b*8 + s] = shadeRGB15(palBaseColors[b], s*2);
            }
        }
    }
    if (g_dimension == 0) {
        for(int i=0; i<SKY_COLORS; i++) {
            int r = 31 - (21 * i) / SKY_COLORS;
            int g = 31 - (13 * i) / SKY_COLORS;
            PAL_BG_MEM[SKY_START_IDX + i] = RGB5(r,g,31);
        }
        for(int i=0; i<SKY_COLORS; i++) {
            int val = 31 - (31 * i) / (SKY_COLORS - 1);
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
    int tnt_pal_src = texture_pack_palette_valid[BLK_TNT] ? BLK_TNT :
                      (texture_pack_palette_valid[BLK_TNT_LIT] ? BLK_TNT_LIT : -1);
    if(tnt_pal_src >= 0){
        for(int s=0; s<SHADES_PER_BLOCK; s++) PAL_BG_MEM[TEXPACK_TNT_START_IDX + s] = texture_pack_palettes[tnt_pal_src][s];
    }
    PAL_BG_MEM[255] = RGB5(31,31,31); 
    PAL_BG_MEM[254] = RGB5(31,31,0);  
    PAL_BG_MEM[253] = RGB5(15,15,15); 
    PAL_BG_MEM[252] = RGB5(8,8,8);    
    PAL_BG_MEM[251] = RGB5(0,0,0);    
    PAL_BG_MEM[250] = RGB5(20,20,20); 
    PAL_BG_MEM[249] = RGB5(31,6,8);
    PAL_BG_MEM[248] = RGB5(20,3,5);
    PAL_BG_MEM[247] = RGB5(18,24,31);
    PAL_BG_MEM[246] = RGB5(8,15,24);
    PAL_BG_MEM[245] = RGB5(12,12,18);
    PAL_BG_MEM[244] = RGB5(6,6,8);
}

#include "texture16_defs.inc"
#undef TEX16

typedef struct {
    u32 magic;
    u16 version;
    u16 header_size;
    u16 block_count;
    u16 tex_size;
    u32 data_size;
    u32 checksum;
    char name[TEXPACK_NAME_LEN];
    u8 reserved[8];
} SramTexturePackHeader;
typedef char SramTexturePackHeader_SizeCheck[(sizeof(SramTexturePackHeader) == 44u) ? 1 : -1];

static const char* texture_pack_status_label(void){
    if(!g_sram_texture_pack_present) return "NONE";
    if(!g_texture_pack_enabled) return "OFF";
    return g_sram_texture_pack_loaded ? "SRAM" : "BAD";
}

static inline void texture_set_texel(int b, int idx, u8 val){
    u8* p = textures[b];
    u8 v = (u8)(val & 7u);
    if(idx & 1) p[idx >> 1] = (u8)((p[idx >> 1] & 0x0Fu) | (u8)(v << 4));
    else        p[idx >> 1] = (u8)((p[idx >> 1] & 0xF0u) | v);
}

static inline u8 texture_get_texel(int b, int idx){
    u8 v = textures[b][idx >> 1];
    return (idx & 1) ? (u8)((v >> 4) & 7u) : (u8)(v & 7u);
}

static void texture_cache_invalidate(void){
    memset(shaded_texture_lookup, 0, sizeof(shaded_texture_lookup));
    for(int i = 0; i < TEXTURE_CACHE_SLOTS; i++){
        shaded_texture_cache_block[i] = TEXTURE_CACHE_EMPTY;
        shaded_texture_cache_light[i] = 0;
        shaded_texture_cache_age[i] = 0;
    }
    g_texture_cache_tick = 1;
}

static inline u32 texture_checksum32(const void* data, u32 size){
    const u8* p = (const u8*)data;
    u32 sum = 0;
    for(u32 i = 0; i < size; i++) sum = (sum * 16777619u) ^ p[i];
    return sum;
}

static inline void texture_sram_read(u32 off, void* dst, u32 size){
    volatile const u8* s = SRAM_BASE + off;
    u8* d = (u8*)dst;
    for(u32 i = 0; i < size; i++) d[i] = s[i];
}

static bool texture_pack_sram_present(void){
    SramTexturePackHeader hdr;
    texture_sram_read(TEXPACK_SRAM_OFF, &hdr, sizeof(hdr));
    return hdr.magic == TEXPACK_MAGIC &&
           (hdr.version == TEXPACK_VERSION || hdr.version == TEXPACK_VERSION_PALETTES) &&
           hdr.header_size >= sizeof(SramTexturePackHeader) &&
           hdr.header_size < TEXPACK_SRAM_SIZE &&
           hdr.tex_size == TEXTURE_SIZE &&
           hdr.block_count <= BLK_COUNT &&
           hdr.data_size <= (TEXPACK_SRAM_SIZE - hdr.header_size);
}

static inline u32 tex_hash(int b, int x, int y){
    u32 h = ((u32)b * 73856093u) ^ ((u32)x * 19349663u) ^ ((u32)y * 83492791u);
    h ^= h >> 16; h *= 0x85ebca6bu;
    h ^= h >> 13; h *= 0xc2b2ae35u;
    h ^= h >> 16;
    return h;
}

static void texture_write_8x8_visual(int b){
    for(int y = 0; y < TEXTURE_SIZE; y++){
        int sy = y >> 1;
        for(int x = 0; x < TEXTURE_SIZE; x++){
            int sx = x >> 1;
            texture_set_texel(b, y * TEXTURE_SIZE + x, texture_data_rom[b][sy * 8 + sx]);
        }
    }
}

static void texture_write_16x16_builtin(int b){
    for(int y = 0; y < TEXTURE_SIZE; y++){
        int sy = y >> 1;
        for(int x = 0; x < TEXTURE_SIZE; x++){
            int sx = x >> 1;
            int src = (int)texture_data_rom[b][sy * 8 + sx];
            static const int bayer[4] = {-1, +1, +1, -1};
            int jitter = (tex_hash(b, x, y) & 3u) ? 0 : bayer[((y & 1) << 1) | (x & 1)];
            int out = src + jitter;
            if(out < 0) out = 0;
            if(out > 7) out = 7;
            texture_set_texel(b, y * TEXTURE_SIZE + x, (u8)out);
        }
    }
}

static void texture_write_16x16_texels(int b, const u8* src){
    for(int i = 0; i < TEXTURE_PIXELS; i++) texture_set_texel(b, i, src[i]);
}

static void texture_build_builtin(void){
    for(int b = 0; b < BLK_COUNT; b++){
        if(g_use_16x16_textures) texture_write_16x16_builtin(b);
        else texture_write_8x8_visual(b);
    }
    if(!g_use_16x16_textures) return;
    texture_write_16x16_texels(BLK_GRASS,        TEX16_GRASS);
    texture_write_16x16_texels(BLK_GRASS_SIDE,   TEX16_GRASS_SIDE);
    texture_write_16x16_texels(BLK_DIRT,         TEX16_DIRT);
    texture_write_16x16_texels(BLK_STONE,        TEX16_STONE);
    texture_write_16x16_texels(BLK_SAND,         TEX16_SAND);
    texture_write_16x16_texels(BLK_WOOD,         TEX16_WOOD);
    texture_write_16x16_texels(BLK_WOOD_DARK,    TEX16_WOOD);
    texture_write_16x16_texels(BLK_WOOD_LIGHT,   TEX16_WOOD);
    texture_write_16x16_texels(BLK_LEAF,         TEX16_LEAF);
    texture_write_16x16_texels(BLK_LEAF_DARK,    TEX16_LEAF);
    texture_write_16x16_texels(BLK_LEAF_LIGHT,   TEX16_LEAF);
    texture_write_16x16_texels(BLK_PLANK,        TEX16_PLANK);
    texture_write_16x16_texels(BLK_COBBLE,       TEX16_COBBLE);
    texture_write_16x16_texels(BLK_MOSSY_COBBLE, TEX16_COBBLE);
    texture_write_16x16_texels(BLK_BRICK,        TEX16_BRICK);
}

static u8 texture_shaded_color(int b, int l, u8 texel){
    texel &= 7u;
    if(texture_pack_palette_valid[b]){
        if(b < BLOCK_PALETTE_BLOCKS) return (u8)((b << 3) | texel);
        if(b == BLK_TNT || b == BLK_TNT_LIT) return (u8)(TEXPACK_TNT_START_IDX + texel);
    } else if(b == BLK_TNT_LIT && texture_pack_palette_valid[BLK_TNT]){
        return (u8)(TEXPACK_TNT_START_IDX + texel);
    }
    if(b == BLK_GRASS_SIDE){
        u8 shade = tex_light_lut[l][texel];
        if(texel <= 4u && shade > 4u) shade = 4u;
        return (u8)((b << 3) | shade);
    }
    if(b == BLK_ORE_IRON || b == BLK_ORE_GOLD || b == BLK_ORE_DIAMOND){
        if(texel <= 4) return (u8)((BLK_STONE << 3) | tex_light_lut[l][texel]);
        if(b == BLK_ORE_IRON){
            if(texel == 5) return (u8)((BLK_SAND << 3) | tex_light_lut[l][2]);
            if(texel == 6) return (u8)((BLK_SAND << 3) | tex_light_lut[l][5]);
            return (u8)((BLK_SAND << 3) | tex_light_lut[l][7]);
        }
        if(b == BLK_ORE_GOLD){
            if(texel == 5) return (u8)((BLK_PLANK << 3) | tex_light_lut[l][2]);
            if(texel == 6) return (u8)((BLK_PLANK << 3) | tex_light_lut[l][5]);
            return (u8)((BLK_BRICK << 3) | tex_light_lut[l][4]);
        }
        if(texel == 5) return (u8)((BLK_GLASS << 3) | tex_light_lut[l][4]);
        if(texel == 6) return (u8)((BLK_WATER << 3) | tex_light_lut[l][5]);
        return (u8)((BLK_GLASS << 3) | tex_light_lut[l][7]);
    }
    if(b == BLK_TNT || b == BLK_TNT_LIT){
        if(texel == 0) return (u8)((BLK_BEDROCK << 3) | 0);
        if(texel == 1) return (u8)((BLK_SAND << 3) | ((b == BLK_TNT_LIT) ? 7 : 6));
        if(texel == 2) return (u8)((BLK_SAND << 3) | ((b == BLK_TNT_LIT) ? 5 : 4));
        if(texel == 3) return (u8)((BLK_NETHER_BRICK << 3) | tex_light_lut[l][1]);
        if(texel == 4) return (u8)((BLK_NETHER_BRICK << 3) | tex_light_lut[l][3]);
        if(texel == 5) return (u8)((BLK_NETHER_BRICK << 3) | tex_light_lut[l][5]);
        if(texel == 6) return (u8)((BLK_BEDROCK << 3) | tex_light_lut[l][2]);
        return (u8)((BLK_GLASS << 3) | 7);
    }
    if(b == BLK_TRACK){
        if(texel <= 3) return (u8)((BLK_PLANK << 3) | tex_light_lut[l][texel + 2]);
        if(texel <= 5) return (u8)((BLK_STONE << 3) | tex_light_lut[l][texel - 1]);
        return (u8)((BLK_SAND << 3) | tex_light_lut[l][texel]);
    }
    if(b == BLK_COBWEB){
        return (texel >= 5)
            ? (u8)((BLK_GLASS << 3) | tex_light_lut[l][7])
            : (u8)((BLK_STONE << 3) | tex_light_lut[l][texel]);
    }
    if(b == BLK_TRACK_SIDE) return (u8)((BLK_PLANK << 3) | tex_light_lut[l][texel]);
    return (u8)((b << 3) | tex_light_lut[l][texel]);
}

static void texture_cache_touch(int slot){
    g_texture_cache_tick++;
    if(g_texture_cache_tick == 0){
        g_texture_cache_tick = 1;
        for(int i = 0; i < TEXTURE_CACHE_SLOTS; i++) shaded_texture_cache_age[i] = 0;
    }
    shaded_texture_cache_age[slot] = g_texture_cache_tick;
}

static void texture_build_cache_slot(int slot, int b, int l){
    for(int t = 0; t < TEXTURE_PIXELS; t++){
        shaded_texture_cache[slot][t] = texture_shaded_color(b, l, texture_get_texel(b, t));
    }
    shaded_texture_cache_block[slot] = (u8)b;
    shaded_texture_cache_light[slot] = (u8)l;
    shaded_texture_lookup[b][l] = (u8)(slot + 1);
    texture_cache_touch(slot);
}

static const u8* __attribute__((noinline)) shaded_texture_for_miss(int b, int l){
    int best = 0;
    u16 best_age = 0xFFFFu;
    for(int i = 0; i < TEXTURE_CACHE_SLOTS; i++){
        if(shaded_texture_cache_block[i] == TEXTURE_CACHE_EMPTY){
            best = i;
            best_age = 0;
            break;
        }
        if(shaded_texture_cache_age[i] < best_age){
            best_age = shaded_texture_cache_age[i];
            best = i;
        }
    }

    if(shaded_texture_cache_block[best] != TEXTURE_CACHE_EMPTY){
        u8 old_b = shaded_texture_cache_block[best];
        u8 old_l = shaded_texture_cache_light[best] & 3u;
        if(old_b < BLK_COUNT) shaded_texture_lookup[old_b][old_l] = 0;
    }
    texture_build_cache_slot(best, b, l);
    return shaded_texture_cache[best];
}

static inline const u8* shaded_texture_for(int b, int l){
    if(b < 0) b = 0;
    else if(b >= BLK_COUNT) b = BLK_COUNT - 1;
    l &= 3;
    u8 hit = shaded_texture_lookup[b][l];
    if(hit){
        int slot = (int)hit - 1;
        texture_cache_touch(slot);
        return shaded_texture_cache[slot];
    }
    return shaded_texture_for_miss(b, l);
}

static bool texture_apply_sram_pack(void){
    SramTexturePackHeader hdr;
    texture_sram_read(TEXPACK_SRAM_OFF, &hdr, sizeof(hdr));
    g_sram_texture_pack_present = texture_pack_sram_present();
    g_sram_texture_pack_loaded = false;
    strncpy(g_sram_texture_pack_name, "NONE", sizeof(g_sram_texture_pack_name));
    g_sram_texture_pack_name[sizeof(g_sram_texture_pack_name) - 1] = '\0';
    if(!g_texture_pack_enabled || !g_sram_texture_pack_present) return false;

    const u32 record_size = (hdr.version == TEXPACK_VERSION_PALETTES) ? TEXPACK_RECORD_V2_SIZE : TEXPACK_RECORD_V1_SIZE;
    u8 data[TEXPACK_RECORD_V2_SIZE];
    u32 off = TEXPACK_SRAM_OFF + hdr.header_size;
    u32 end = TEXPACK_SRAM_OFF + hdr.header_size + hdr.data_size;
    u32 sum = 0;
    while(off < end){
        u32 chunk = end - off;
        if(chunk > record_size) chunk = record_size;
        texture_sram_read(off, data, chunk);
        sum ^= texture_checksum32(data, chunk);
        off += chunk;
    }
    if(sum != hdr.checksum) return false;

    off = TEXPACK_SRAM_OFF + hdr.header_size;
    for(int i = 0; i < (int)hdr.block_count; i++){
        if(off + record_size > end) break;
        texture_sram_read(off, data, record_size);
        off += record_size;
        u8 bid = data[0];
        if(bid >= BLK_COUNT) continue;
        const u8* texels = data + 2;
        if(hdr.version == TEXPACK_VERSION_PALETTES){
            const u8* pal = data + 2;
            for(int s = 0; s < SHADES_PER_BLOCK; s++){
                texture_pack_palettes[bid][s] = (u16)pal[s * 2] | ((u16)pal[s * 2 + 1] << 8);
            }
            texture_pack_palette_valid[bid] = 1;
            texels = data + 2 + SHADES_PER_BLOCK * 2;
        }
        if(g_use_16x16_textures){
            texture_write_16x16_texels(bid, texels);
        } else {
            for(int y = 0; y < TEXTURE_SIZE; y++){
                int sy = y & ~1;
                for(int x = 0; x < TEXTURE_SIZE; x++){
                    int sx = x & ~1;
                    texture_set_texel(bid, y * TEXTURE_SIZE + x, texels[sy * TEXTURE_SIZE + sx]);
                }
            }
        }
    }
    for(int i = 0; i < TEXPACK_NAME_LEN; i++) g_sram_texture_pack_name[i] = hdr.name[i];
    g_sram_texture_pack_name[TEXPACK_NAME_LEN] = '\0';
    if(!g_sram_texture_pack_name[0]) strncpy(g_sram_texture_pack_name, "SRAM PACK", sizeof(g_sram_texture_pack_name));
    g_sram_texture_pack_loaded = true;
    return true;
}

static void generate_textures(void){
    memset(texture_pack_palette_valid, 0, sizeof(texture_pack_palette_valid));
    texture_build_builtin();
    texture_apply_sram_pack();
    texture_cache_invalidate();
    buildPalette();
}

static inline u8 block_render_top_type(u8 blk){
    if(blk == BLK_GRASS) return BLK_GRASS;
    if(blk == BLK_TRACK) return BLK_TRACK;
    return blk;
}

static inline u8 block_render_side_type(u8 blk){
    if(blk == BLK_GRASS) return BLK_GRASS_SIDE;
    if(blk == BLK_TRACK) return BLK_TRACK_SIDE;
    return blk;
}

static inline u8 block_flat_palette_type(u8 blk){
    if(blk == BLK_GRASS_SIDE) return BLK_DIRT;
    if(blk == BLK_ORE_IRON || blk == BLK_ORE_GOLD || blk == BLK_ORE_DIAMOND) return BLK_STONE;
    if(blk == BLK_TRACK || blk == BLK_TRACK_SIDE) return BLK_PLANK;
    if(blk == BLK_COBWEB) return BLK_GLASS;
    if(blk == BLK_TNT || blk == BLK_TNT_LIT) return BLK_NETHER_BRICK;
    return blk;
}

static inline u8 block_id_from_buildable(u8 b){
    return (b == ITEM_FLINT_STEEL) ? ITEM_FLINT_STEEL : (u8)(b + 1);
}

static int buildable_index_from_block(u8 curBlk){
    for(int i=0; i<BUILDABLE_COUNT; i++){
        if(block_id_from_buildable(g_buildable_blocks[i]) == curBlk) return i;
    }
    return 0;
}

typedef struct {
    int saved_scale;
} ProgressScreenState;

typedef enum {
    RESPAWN_VISUAL_NONE = 0,
    RESPAWN_VISUAL_GENERATE,
    RESPAWN_VISUAL_RESPAWN
} RespawnVisualMode;

typedef enum {
    DEATH_RESPAWN = 0,
    DEATH_MAIN_MENU
} DeathMenuChoice;


