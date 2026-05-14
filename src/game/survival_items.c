typedef struct {
    u8 in1, count1;
    u8 in2, count2;
    u8 out, out_count;
} RecipeDef;

static const RecipeDef g_recipes[] = {
    {BLK_WOOD + 1, 1, ITEM_NONE, 0, BLK_PLANK + 1, 4},
    {BLK_WOOD_DARK + 1, 1, ITEM_NONE, 0, BLK_PLANK + 1, 4},
    {BLK_WOOD_LIGHT + 1, 1, ITEM_NONE, 0, BLK_PLANK + 1, 4},
    {BLK_PLANK + 1, 2, ITEM_NONE, 0, ITEM_STICK, 4},
    {BLK_PLANK + 1, 3, ITEM_STICK, 2, ITEM_PICK_WOOD, 1},
    {BLK_COBBLE + 1, 3, ITEM_STICK, 2, ITEM_PICK_STONE, 1},
    {BLK_GRAVEL + 1, 2, BLK_COBBLE + 1, 1, ITEM_FLINT_STEEL, 1}
};

static int recipe_count(void){
    return (int)(sizeof(g_recipes) / sizeof(g_recipes[0]));
}

static bool recipe_is_craftable(const RecipeDef* r){
    if(!r) return false;
    if(inventory_count_item(r->in1) < r->count1) return false;
    if(r->in2 != ITEM_NONE && inventory_count_item(r->in2) < r->count2) return false;
    return true;
}

static bool craft_recipe(int idx){
    if(idx < 0 || idx >= recipe_count()) return false;
    const RecipeDef* r = &g_recipes[idx];
    if(!recipe_is_craftable(r)) return false;
    if(r->in1 != ITEM_NONE && r->count1) inventory_remove_item(r->in1, r->count1);
    if(r->in2 != ITEM_NONE && r->count2) inventory_remove_item(r->in2, r->count2);
    if(!inventory_add_item(r->out, r->out_count)) return false;
    inventory_compact();
    return true;
}

static void set_player_health_units(int amount){
    if(amount < 0) amount = 0;
    if(amount > g_player_max_health) amount = g_player_max_health;
    if(amount < g_player_health){
        g_health_flash_unit = amount;
        g_health_flash_timer = 12;
    }
    g_player_health = (u8)amount;
}

static void set_player_air_units(int amount){
    if(amount < 0) amount = 0;
    if(amount > g_player_max_air) amount = g_player_max_air;
    if(amount < g_player_air){
        g_air_flash_unit = amount;
        g_air_flash_timer = 12;
    }
    g_player_air = (u8)amount;
}

static int survival_break_ticks(u8 blk, u8 held_item){
    int base = 18;
    bool stone_family = false;
    switch(blk){
        case BLK_GRASS:
        case BLK_DIRT:
        case BLK_SAND:
        case BLK_GRAVEL:
        case BLK_CLAY:
            base = 10;
            break;
        case BLK_WOOD:
        case BLK_WOOD_DARK:
        case BLK_WOOD_LIGHT:
        case BLK_PLANK:
        case BLK_BRICK:
        case BLK_CACTUS:
        case BLK_TRACK:
        case BLK_TNT:
            base = 18;
            break;
        case BLK_STONE:
        case BLK_ORE_IRON:
        case BLK_ORE_GOLD:
        case BLK_COBBLE:
        case BLK_MOSSY_COBBLE:
        case BLK_NETHER_BRICK:
        case BLK_BASALT:
            base = 26;
            stone_family = true;
            break;
        case BLK_ORE_DIAMOND:
            base = 30;
            stone_family = true;
            break;
        case BLK_OBSIDIAN:
            base = 80;
            break;
        case BLK_LEAF:
        case BLK_LEAF_DARK:
        case BLK_LEAF_LIGHT:
        case BLK_GLASS:
        case BLK_COBWEB:
        case BLK_TNT_LIT:
            base = 6;
            break;
        default:
            break;
    }
    if(stone_family && !item_is_pickaxe(held_item)) base *= 3;
    if(item_is_pickaxe(held_item)){
        int level = pickaxe_level(held_item);
        if(level == 1) base = (base * 3) / 4;
        else if(level == 2) base = base / 2;
        else if(level >= 3) base = base / 3;
    }
    if(base < 4) base = 4;
    return base;
}

static bool survival_can_harvest(u8 blk, u8 held_item){
    if(blk == BLK_BEDROCK || blk == BLK_PORTAL || blk == BLK_WATER || blk == BLK_LAVA) return false;
    if(blk == BLK_OBSIDIAN) return pickaxe_level(held_item) >= 2;
    if(blk == BLK_STONE || blk == BLK_ORE_IRON || blk == BLK_ORE_GOLD || blk == BLK_ORE_DIAMOND ||
       blk == BLK_COBBLE || blk == BLK_MOSSY_COBBLE || blk == BLK_BASALT || blk == BLK_NETHER_BRICK){
        return item_is_pickaxe(held_item);
    }
    return true;
}

static bool survival_can_break(u8 blk, u8 held_item){
    if(blk == BLK_BEDROCK || blk == BLK_PORTAL || blk == BLK_WATER || blk == BLK_LAVA) return false;
    if(blk == BLK_OBSIDIAN) return pickaxe_level(held_item) >= 2;
    if(blk == BLK_TNT_LIT) return false;
    return true;
}

static u8 survival_drop_item(u8 blk){
    switch(blk){
        case BLK_GRASS: return BLK_DIRT + 1;
        case BLK_STONE: return BLK_COBBLE + 1;
        case BLK_PORTAL:
        case BLK_WATER:
        case BLK_LAVA:
        case BLK_BEDROCK:
        case BLK_TNT_LIT:
            return ITEM_NONE;
        default:
            return (u8)(blk + 1);
    }
}

static void survival_damage(int amount){
    if(g_game_mode != MODE_SURVIVAL || amount <= 0) return;
    set_player_health_units((int)g_player_health - amount);
}

static bool find_overworld_spawn(int* out_x, int* out_y, int* out_z){
    int cx = CW / 2;
    int cz = CD / 2;
    for(int radius = 0; radius <= 14; radius++){
        for(int dz = -radius; dz <= radius; dz++){
            for(int dx = -radius; dx <= radius; dx++){
                if(radius != 0 && abs(dx) != radius && abs(dz) != radius) continue;
                int x = cx + dx;
                int z = cz + dz;
                if(x < 2 || x >= CW - 2 || z < 2 || z >= CD - 2) continue;
                for(int y = CH - 3; y >= 2; y--){
                    u8 floor_id = world[widx(x, y, z)];
                    if(!floor_id) continue;
                    u8 floor_blk = (u8)(floor_id - 1);
                    if(floor_blk == BLK_WATER || floor_blk == BLK_LAVA || floor_blk == BLK_LEAF ||
                       floor_blk == BLK_LEAF_DARK || floor_blk == BLK_LEAF_LIGHT || floor_blk == BLK_PORTAL) continue;
                    if(world[widx(x, y + 1, z)] != 0) continue;
                    if(world[widx(x, y + 2, z)] != 0) continue;
                    *out_x = x;
                    *out_y = y + 1;
                    *out_z = z;
                    return true;
                }
            }
        }
    }
    return false;
}

static volatile u32 g_frame;
static inline bool solid_at(int x,int y,int z);

static void respawn_player(RespawnVisualMode visual_mode){
    clear_dropped_items();
    stream_reset_state();
    stream_center_on_global(VW/2, VD/2);
    buildPalette();
    if(visual_mode == RESPAWN_VISUAL_GENERATE){
        gen_world_with_progress_screen("GENERATING WORLD");
    } else if(visual_mode == RESPAWN_VISUAL_RESPAWN){
        gen_world_with_progress_screen("RESPAWNING");
    } else {
        gen_world();
    }
    apply_dimension_edits(g_dimension);
    int spawn_x = CW/2, spawn_z = CD/2, spawn_y = CH - 1;
    if(g_world_type == WORLD_SUPERFLAT) spawn_y = 11;
    else if(g_dimension == 0 && find_overworld_spawn(&spawn_x, &spawn_y, &spawn_z)) {
        // Prefer a nearby safe surface instead of the center column, which may be a cave opening.
    } else {
        for(int y = CH - 1; y >= 0; y--){
            u8 id = world[widx(spawn_x, y, spawn_z)];
            if(id){
                u8 t = id - 1;
                if(t != BLK_WATER && t != BLK_LEAF && t != BLK_LEAF_DARK && t != BLK_LEAF_LIGHT && t != BLK_LAVA){
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
    pl.vel.x = pl.vel.y = pl.vel.z = 0;
    pl.yaw = LUT_N/2;
    pl.pitch = 0;
    pl.onGround = false;
    g_creative_flying = false;
    g_jump_tap_timer = 0;
    survival_reset_stats();
    g_mining_ticks = 0;
    g_mining_x = g_mining_y = g_mining_z = -1;
    store_current_dimension_state();
}

static void spawn_dropped_item(int x, int y, int z, u8 item){
    if(item == ITEM_NONE) return;
    int slot = -1;
    int oldest = 0;
    u16 oldest_age = 0;
    for(int i=0; i<MAX_DROPPED_ITEMS; i++){
        if(!g_dropped_items[i].active){
            slot = i;
            break;
        }
        if(i == 0 || g_dropped_items[i].age > oldest_age){
            oldest = i;
            oldest_age = g_dropped_items[i].age;
        }
    }
    if(slot < 0) slot = oldest;

    DroppedItem* d = &g_dropped_items[slot];
    d->active = true;
    d->item = item;
    d->age = 0;
    d->pos.x = I2F(x) + F(0.5f);
    d->pos.y = I2F(y) + F(0.35f);
    d->pos.z = I2F(z) + F(0.5f);
    d->vel.x = 0;
    d->vel.z = 0;
    switch((g_frame + x + (z << 1)) & 3){
        case 0: d->vel.x = F(0.05f); d->vel.z = F(0.02f); break;
        case 1: d->vel.x = F(-0.05f); d->vel.z = F(0.03f); break;
        case 2: d->vel.x = F(0.03f); d->vel.z = F(-0.05f); break;
        default:d->vel.x = F(-0.03f); d->vel.z = F(-0.04f); break;
    }
    d->vel.y = F(0.11f);
}

static void update_dropped_items(u8* io_curBlk){
    const fix gravity = F(0.012f);
    const fix item_half_height = F(0.18f);
    const fix pickup_xz = F(0.75f);
    const fix pickup_y = F(1.35f);

    for(int i=0; i<MAX_DROPPED_ITEMS; i++){
        DroppedItem* d = &g_dropped_items[i];
        if(!d->active) continue;

        d->age++;
        if(d->age > 60 * 45){
            d->active = false;
            continue;
        }

        if(g_game_mode == MODE_SURVIVAL && d->age > 8){
            if(fixabs(d->pos.x - pl.pos.x) < pickup_xz &&
               fixabs(d->pos.z - pl.pos.z) < pickup_xz &&
               fixabs(d->pos.y - (pl.pos.y + F(0.8f))) < pickup_y){
                if(inventory_add_item(d->item, 1)){
                    d->active = false;
                    inventory_compact();
                    refresh_selected_item(io_curBlk);
                    continue;
                }
            }
        }

        d->vel.y -= gravity;
        if(d->vel.y < -F(0.16f)) d->vel.y = -F(0.16f);

        fix next_x = d->pos.x + d->vel.x;
        if(!solid_at(F2I(next_x), F2I(d->pos.y), F2I(d->pos.z))) d->pos.x = next_x;
        else d->vel.x = 0;

        fix next_z = d->pos.z + d->vel.z;
        if(!solid_at(F2I(d->pos.x), F2I(d->pos.y), F2I(next_z))) d->pos.z = next_z;
        else d->vel.z = 0;

        fix next_y = d->pos.y + d->vel.y;
        if(d->vel.y <= 0){
            int below_y = F2I(next_y - item_half_height);
            if(solid_at(F2I(d->pos.x), below_y, F2I(d->pos.z))){
                d->pos.y = I2F(below_y + 1) + item_half_height;
                d->vel.y = 0;
                d->vel.x = fmuli(d->vel.x, F(0.78f));
                d->vel.z = fmuli(d->vel.z, F(0.78f));
            } else {
                d->pos.y = next_y;
            }
        } else {
            int above_y = F2I(next_y + item_half_height);
            if(solid_at(F2I(d->pos.x), above_y, F2I(d->pos.z))) d->vel.y = 0;
            else d->pos.y = next_y;
        }

        if(d->pos.x < I2F(1)) d->pos.x = I2F(1);
        if(d->pos.x > I2F(CW-2)) d->pos.x = I2F(CW-2);
        if(d->pos.z < I2F(1)) d->pos.z = I2F(1);
        if(d->pos.z > I2F(CD-2)) d->pos.z = I2F(CD-2);
    }
}

