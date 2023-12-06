package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.inventory.container.BackpackContainer;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.crafting.SimpleRecipeSerializer;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.WallBlock;
import net.minecraftforge.registries.ObjectHolder;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationContainers.ID_BACKPACK_CONTAINER;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;
import static mrtjp.projectred.exploration.init.ExplorationRecipeSerializers.ID_BACKPACK_DYE;

@ObjectHolder(MOD_ID)
@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationReferences {

    /* Blocks */

    // Ores
    @ObjectHolder(ID_RUBY_ORE)
    public static Block RUBY_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_RUBY_ORE)
    public static Block DEEPSLATE_RUBY_ORE_BLOCK;
    @ObjectHolder(ID_SAPPHIRE_ORE)
    public static Block SAPPHIRE_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_SAPPHIRE_ORE)
    public static Block DEEPSLATE_SAPPHIRE_ORE_BLOCK;
    @ObjectHolder(ID_PERIDOT_ORE)
    public static Block PERIDOT_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_PERIDOT_ORE)
    public static Block DEEPSLATE_PERIDOT_ORE_BLOCK;
    @ObjectHolder(ID_TIN_ORE)
    public static Block TIN_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_TIN_ORE)
    public static Block DEEPSLATE_TIN_ORE_BLOCK;
    @ObjectHolder(ID_SILVER_ORE)
    public static Block SILVER_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_SILVER_ORE)
    public static Block DEEPSLATE_SILVER_ORE_BLOCK;
    @ObjectHolder(ID_ELECTROTINE_ORE)
    public static Block ELECTROTINE_ORE_BLOCK;
    @ObjectHolder(ID_DEEPSLATE_ELECTROTINE_ORE)
    public static Block DEEPSLATE_ELECTROTINE_ORE_BLOCK;

    // Decorative blocks
    @ObjectHolder(ID_MARBLE)
    public static Block MARBLE_BLOCK;
    @ObjectHolder(ID_MARBLE_BRICK)
    public static Block MARBLE_BRICK_BLOCK;
    @ObjectHolder(ID_BASALT)
    public static Block BASALT_BLOCK;
    @ObjectHolder(ID_BASALT_COBBLE)
    public static Block BASALT_COBBLE_BLOCK;
    @ObjectHolder(ID_BASALT_BRICK)
    public static Block BASALT_BRICK_BLOCK;
    @ObjectHolder(ID_RUBY_BLOCK)
    public static Block RUBY_BLOCK;
    @ObjectHolder(ID_SAPPHIRE_BLOCK)
    public static Block SAPPHIRE_BLOCK;
    @ObjectHolder(ID_PERIDOT_BLOCK)
    public static Block PERIDOT_BLOCK;
    @ObjectHolder(ID_ELECTROTINE_BLOCK)
    public static Block ELECTROTINE_BLOCK;
    @ObjectHolder(ID_RAW_TIN_BLOCK)
    public static Block RAW_TIN_BLOCK;
    @ObjectHolder(ID_RAW_SILVER_BLOCK)
    public static Block RAW_SILVER_BLOCK;
    @ObjectHolder(ID_TIN_BLOCK)
    public static Block TIN_BLOCK;
    @ObjectHolder(ID_SILVER_BLOCK)
    public static Block SILVER_BLOCK;

    // Walls
    @ObjectHolder(ID_MARBLE_WALL)
    public static WallBlock MARBLE_WALL;
    @ObjectHolder(ID_MARBLE_BRICK_WALL)
    public static WallBlock MARBLE_BRICK_WALL;
    @ObjectHolder(ID_BASALT_WALL)
    public static WallBlock BASALT_WALL;
    @ObjectHolder(ID_BASALT_COBBLE_WALL)
    public static WallBlock BASALT_COBBLE_WALL;
    @ObjectHolder(ID_BASALT_BRICK_WALL)
    public static WallBlock BASALT_BRICK_WALL;
    @ObjectHolder(ID_RUBY_BLOCK_WALL)
    public static WallBlock RUBY_BLOCK_WALL;
    @ObjectHolder(ID_SAPPHIRE_BLOCK_WALL)
    public static WallBlock SAPPHIRE_BLOCK_WALL;
    @ObjectHolder(ID_PERIDOT_BLOCK_WALL)
    public static WallBlock PERIDOT_BLOCK_WALL;
    @ObjectHolder(ID_ELECTROTINE_BLOCK_WALL)
    public static WallBlock ELECTROTINE_BLOCK_WALL;

    /* Items */

    // Ingots / dusts / gems
    @ObjectHolder(ID_RAW_TIN)
    public static Item RAW_TIN_ITEM;
    @ObjectHolder(ID_TIN_INGOT)
    public static Item TIN_INGOT_ITEM;
    @ObjectHolder(ID_RAW_SILVER)
    public static Item RAW_SILVER_ITEM;
    @ObjectHolder(ID_SILVER_INGOT)
    public static Item SILVER_INGOT_ITEM;

    @ObjectHolder(ID_WOOL_GIN)
    public static Item WOOL_GIN;
    @ObjectHolder(ID_ATHAME)
    public static Item ATHAME;

    // Tools
    @ObjectHolder(ID_RUBY_AXE)
    public static Item RUBY_AXE;
    @ObjectHolder(ID_SAPPHIRE_AXE)
    public static Item SAPPHIRE_AXE;
    @ObjectHolder(ID_PERIDOT_AXE)
    public static Item PERIDOT_AXE;

    @ObjectHolder(ID_RUBY_PICKAXE)
    public static Item RUBY_PICKAXE;
    @ObjectHolder(ID_SAPPHIRE_PICKAXE)
    public static Item SAPPHIRE_PICKAXE;
    @ObjectHolder(ID_PERIDOT_PICKAXE)
    public static Item PERIDOT_PICKAXE;

    @ObjectHolder(ID_RUBY_SHOVEL)
    public static Item RUBY_SHOVEL;
    @ObjectHolder(ID_SAPPHIRE_SHOVEL)
    public static Item SAPPHIRE_SHOVEL;
    @ObjectHolder(ID_PERIDOT_SHOVEL)
    public static Item PERIDOT_SHOVEL;

    @ObjectHolder(ID_RUBY_HOE)
    public static Item RUBY_HOE;
    @ObjectHolder(ID_SAPPHIRE_HOE)
    public static Item SAPPHIRE_HOE;
    @ObjectHolder(ID_PERIDOT_HOE)
    public static Item PERIDOT_HOE;

    @ObjectHolder(ID_RUBY_SWORD)
    public static Item RUBY_SWORD;
    @ObjectHolder(ID_SAPPHIRE_SWORD)
    public static Item SAPPHIRE_SWORD;
    @ObjectHolder(ID_PERIDOT_SWORD)
    public static Item PERIDOT_SWORD;

    @ObjectHolder(ID_GOLD_SAW)
    public static Item GOLD_SAW;
    @ObjectHolder(ID_RUBY_SAW)
    public static Item RUBY_SAW;
    @ObjectHolder(ID_SAPPHIRE_SAW)
    public static Item SAPPHIRE_SAW;
    @ObjectHolder(ID_PERIDOT_SAW)
    public static Item PERIDOT_SAW;

    @ObjectHolder(ID_WOOD_SICKLE)
    public static Item WOOD_SICKLE;
    @ObjectHolder(ID_STONE_SICKLE)
    public static Item STONE_SICKLE;
    @ObjectHolder(ID_IRON_SICKLE)
    public static Item IRON_SICKLE;
    @ObjectHolder(ID_GOLD_SICKLE)
    public static Item GOLD_SICKLE;
    @ObjectHolder(ID_DIAMOND_SICKLE)
    public static Item DIAMOND_SICKLE;
    @ObjectHolder(ID_RUBY_SICKLE)
    public static Item RUBY_SICKLE;
    @ObjectHolder(ID_SAPPHIRE_SICKLE)
    public static Item SAPPHIRE_SICKLE;
    @ObjectHolder(ID_PERIDOT_SICKLE)
    public static Item PERIDOT_SICKLE;

    // Armor
    @ObjectHolder(ID_RUBY_HELMET)
    public static Item RUBY_HELMET;
    @ObjectHolder(ID_SAPPHIRE_HELMET)
    public static Item SAPPHIRE_HELMET;
    @ObjectHolder(ID_PERIDOT_HELMET)
    public static Item PERIDOT_HELMET;

    @ObjectHolder(ID_RUBY_CHESTPLATE)
    public static Item RUBY_CHESTPLATE;
    @ObjectHolder(ID_SAPPHIRE_CHESTPLATE)
    public static Item SAPPHIRE_CHESTPLATE;
    @ObjectHolder(ID_PERIDOT_CHESTPLATE)
    public static Item PERIDOT_CHESTPLATE;

    @ObjectHolder(ID_RUBY_LEGGINGS)
    public static Item RUBY_LEGGINGS;
    @ObjectHolder(ID_SAPPHIRE_LEGGINGS)
    public static Item SAPPHIRE_LEGGINGS;
    @ObjectHolder(ID_PERIDOT_LEGGINGS)
    public static Item PERIDOT_LEGGINGS;

    @ObjectHolder(ID_RUBY_BOOTS)
    public static Item RUBY_BOOTS;
    @ObjectHolder(ID_SAPPHIRE_BOOTS)
    public static Item SAPPHIRE_BOOTS;
    @ObjectHolder(ID_PERIDOT_BOOTS)
    public static Item PERIDOT_BOOTS;

    //Backpacks
    @ObjectHolder(ID_WHITE_BACKPACK)
    public static Item WHITE_BACKPACK;
    @ObjectHolder(ID_ORANGE_BACKPACK)
    public static Item ORANGE_BACKPACK;
    @ObjectHolder(ID_MAGENTA_BACKPACK)
    public static Item MAGENTA_BACKPACK;
    @ObjectHolder(ID_LIGHT_BLUE_BACKPACK)
    public static Item LIGHT_BLUE_BACKPACK;
    @ObjectHolder(ID_YELLOW_BACKPACK)
    public static Item YELLOW_BACKPACK;
    @ObjectHolder(ID_LIME_BACKPACK)
    public static Item LIME_BACKPACK;
    @ObjectHolder(ID_PINK_BACKPACK)
    public static Item PINK_BACKPACK;
    @ObjectHolder(ID_GRAY_BACKPACK)
    public static Item GRAY_BACKPACK;
    @ObjectHolder(ID_LIGHT_GRAY_BACKPACK)
    public static Item LIGHT_GRAY_BACKPACK;
    @ObjectHolder(ID_CYAN_BACKPACK)
    public static Item CYAN_BACKPACK;
    @ObjectHolder(ID_PURPLE_BACKPACK)
    public static Item PURPLE_BACKPACK;
    @ObjectHolder(ID_BLUE_BACKPACK)
    public static Item BLUE_BACKPACK;
    @ObjectHolder(ID_BROWN_BACKPACK)
    public static Item BROWN_BACKPACK;
    @ObjectHolder(ID_GREEN_BACKPACK)
    public static Item GREEN_BACKPACK;
    @ObjectHolder(ID_RED_BACKPACK)
    public static Item RED_BACKPACK;
    @ObjectHolder(ID_BLACK_BACKPACK)
    public static Item BLACK_BACKPACK;

    /* Containers */
    @ObjectHolder(ID_BACKPACK_CONTAINER)
    public static MenuType<BackpackContainer> BACKPACK_CONTAINER;

    /* Recipe Serializers */
    @ObjectHolder(ID_BACKPACK_DYE)
    public static SimpleRecipeSerializer<?> BACKPACK_DYE_RECIPE_SERIALIZER;

    public static Item getBackpackByColor(int color) {
        switch (color) {
            case 0: return WHITE_BACKPACK;
            case 1: return ORANGE_BACKPACK;
            case 2: return MAGENTA_BACKPACK;
            case 3: return LIGHT_BLUE_BACKPACK;
            case 4: return YELLOW_BACKPACK;
            case 5: return LIME_BACKPACK;
            case 6: return PINK_BACKPACK;
            case 7: return GRAY_BACKPACK;
            case 8: return LIGHT_GRAY_BACKPACK;
            case 9: return CYAN_BACKPACK;
            case 10: return PURPLE_BACKPACK;
            case 11: return BLUE_BACKPACK;
            case 12: return BROWN_BACKPACK;
            case 13: return GREEN_BACKPACK;
            case 14: return RED_BACKPACK;
            case 15: return BLACK_BACKPACK;
            default: throw new RuntimeException();
        }
    }
}
