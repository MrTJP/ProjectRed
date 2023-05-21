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
public class ExplorationReferences {

    /* Blocks */

    // Ores
    @ObjectHolder(ID_RUBY_ORE)
    public static Block RUBY_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_RUBY_ORE)
    public static Block DEEPSLATE_RUBY_ORE_BLOCK = null;
    @ObjectHolder(ID_SAPPHIRE_ORE)
    public static Block SAPPHIRE_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_SAPPHIRE_ORE)
    public static Block DEEPSLATE_SAPPHIRE_ORE_BLOCK = null;
    @ObjectHolder(ID_PERIDOT_ORE)
    public static Block PERIDOT_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_PERIDOT_ORE)
    public static Block DEEPSLATE_PERIDOT_ORE_BLOCK = null;
    @ObjectHolder(ID_TIN_ORE)
    public static Block TIN_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_TIN_ORE)
    public static Block DEEPSLATE_TIN_ORE_BLOCK = null;
    @ObjectHolder(ID_SILVER_ORE)
    public static Block SILVER_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_SILVER_ORE)
    public static Block DEEPSLATE_SILVER_ORE_BLOCK = null;
    @ObjectHolder(ID_ELECTROTINE_ORE)
    public static Block ELECTROTINE_ORE_BLOCK = null;
    @ObjectHolder(ID_DEEPSLATE_ELECTROTINE_ORE)
    public static Block DEEPSLATE_ELECTROTINE_ORE_BLOCK = null;

    // Decorative blocks
    @ObjectHolder(ID_MARBLE)
    public static Block MARBLE_BLOCK = null;
    @ObjectHolder(ID_MARBLE_BRICK)
    public static Block MARBLE_BRICK_BLOCK = null;
    @ObjectHolder(ID_BASALT)
    public static Block BASALT_BLOCK = null;
    @ObjectHolder(ID_BASALT_COBBLE)
    public static Block BASALT_COBBLE_BLOCK = null;
    @ObjectHolder(ID_BASALT_BRICK)
    public static Block BASALT_BRICK_BLOCK = null;
    @ObjectHolder(ID_RUBY_BLOCK)
    public static Block RUBY_BLOCK = null;
    @ObjectHolder(ID_SAPPHIRE_BLOCK)
    public static Block SAPPHIRE_BLOCK = null;
    @ObjectHolder(ID_PERIDOT_BLOCK)
    public static Block PERIDOT_BLOCK = null;
    @ObjectHolder(ID_ELECTROTINE_BLOCK)
    public static Block ELECTROTINE_BLOCK = null;
    @ObjectHolder(ID_RAW_TIN_BLOCK)
    public static Block RAW_TIN_BLOCK = null;
    @ObjectHolder(ID_RAW_SILVER_BLOCK)
    public static Block RAW_SILVER_BLOCK = null;
    @ObjectHolder(ID_TIN_BLOCK)
    public static Block TIN_BLOCK = null;
    @ObjectHolder(ID_SILVER_BLOCK)
    public static Block SILVER_BLOCK = null;

    // Walls
    @ObjectHolder(ID_MARBLE_WALL)
    public static WallBlock MARBLE_WALL = null;
    @ObjectHolder(ID_MARBLE_BRICK_WALL)
    public static WallBlock MARBLE_BRICK_WALL = null;
    @ObjectHolder(ID_BASALT_WALL)
    public static WallBlock BASALT_WALL = null;
    @ObjectHolder(ID_BASALT_COBBLE_WALL)
    public static WallBlock BASALT_COBBLE_WALL = null;
    @ObjectHolder(ID_BASALT_BRICK_WALL)
    public static WallBlock BASALT_BRICK_WALL = null;
    @ObjectHolder(ID_RUBY_BLOCK_WALL)
    public static WallBlock RUBY_BLOCK_WALL = null;
    @ObjectHolder(ID_SAPPHIRE_BLOCK_WALL)
    public static WallBlock SAPPHIRE_BLOCK_WALL = null;
    @ObjectHolder(ID_PERIDOT_BLOCK_WALL)
    public static WallBlock PERIDOT_BLOCK_WALL = null;
    @ObjectHolder(ID_ELECTROTINE_BLOCK_WALL)
    public static WallBlock ELECTROTINE_BLOCK_WALL = null;

    /* Items */

    // Ingots / dusts / gems
    @ObjectHolder(ID_RAW_TIN)
    public static Item RAW_TIN_ITEM = null;
    @ObjectHolder(ID_TIN_INGOT)
    public static Item TIN_INGOT_ITEM = null;
    @ObjectHolder(ID_RAW_SILVER)
    public static Item RAW_SILVER_ITEM = null;
    @ObjectHolder(ID_SILVER_INGOT)
    public static Item SILVER_INGOT_ITEM = null;

    @ObjectHolder(ID_WOOL_GIN)
    public static Item WOOL_GIN = null;
    @ObjectHolder(ID_ATHAME)
    public static Item ATHAME = null;

    // Tools
    @ObjectHolder(ID_RUBY_AXE)
    public static Item RUBY_AXE = null;
    @ObjectHolder(ID_SAPPHIRE_AXE)
    public static Item SAPPHIRE_AXE = null;
    @ObjectHolder(ID_PERIDOT_AXE)
    public static Item PERIDOT_AXE = null;

    @ObjectHolder(ID_RUBY_PICKAXE)
    public static Item RUBY_PICKAXE = null;
    @ObjectHolder(ID_SAPPHIRE_PICKAXE)
    public static Item SAPPHIRE_PICKAXE = null;
    @ObjectHolder(ID_PERIDOT_PICKAXE)
    public static Item PERIDOT_PICKAXE = null;

    @ObjectHolder(ID_RUBY_SHOVEL)
    public static Item RUBY_SHOVEL = null;
    @ObjectHolder(ID_SAPPHIRE_SHOVEL)
    public static Item SAPPHIRE_SHOVEL = null;
    @ObjectHolder(ID_PERIDOT_SHOVEL)
    public static Item PERIDOT_SHOVEL = null;

    @ObjectHolder(ID_RUBY_HOE)
    public static Item RUBY_HOE = null;
    @ObjectHolder(ID_SAPPHIRE_HOE)
    public static Item SAPPHIRE_HOE = null;
    @ObjectHolder(ID_PERIDOT_HOE)
    public static Item PERIDOT_HOE = null;

    @ObjectHolder(ID_RUBY_SWORD)
    public static Item RUBY_SWORD = null;
    @ObjectHolder(ID_SAPPHIRE_SWORD)
    public static Item SAPPHIRE_SWORD = null;
    @ObjectHolder(ID_PERIDOT_SWORD)
    public static Item PERIDOT_SWORD = null;

    @ObjectHolder(ID_GOLD_SAW)
    public static Item GOLD_SAW = null;
    @ObjectHolder(ID_RUBY_SAW)
    public static Item RUBY_SAW = null;
    @ObjectHolder(ID_SAPPHIRE_SAW)
    public static Item SAPPHIRE_SAW = null;
    @ObjectHolder(ID_PERIDOT_SAW)
    public static Item PERIDOT_SAW = null;

    @ObjectHolder(ID_WOOD_SICKLE)
    public static Item WOOD_SICKLE = null;
    @ObjectHolder(ID_STONE_SICKLE)
    public static Item STONE_SICKLE = null;
    @ObjectHolder(ID_IRON_SICKLE)
    public static Item IRON_SICKLE = null;
    @ObjectHolder(ID_GOLD_SICKLE)
    public static Item GOLD_SICKLE = null;
    @ObjectHolder(ID_DIAMOND_SICKLE)
    public static Item DIAMOND_SICKLE = null;
    @ObjectHolder(ID_RUBY_SICKLE)
    public static Item RUBY_SICKLE = null;
    @ObjectHolder(ID_SAPPHIRE_SICKLE)
    public static Item SAPPHIRE_SICKLE = null;
    @ObjectHolder(ID_PERIDOT_SICKLE)
    public static Item PERIDOT_SICKLE = null;

    // Armor
    @ObjectHolder(ID_RUBY_HELMET)
    public static Item RUBY_HELMET = null;
    @ObjectHolder(ID_SAPPHIRE_HELMET)
    public static Item SAPPHIRE_HELMET = null;
    @ObjectHolder(ID_PERIDOT_HELMET)
    public static Item PERIDOT_HELMET = null;

    @ObjectHolder(ID_RUBY_CHESTPLATE)
    public static Item RUBY_CHESTPLATE = null;
    @ObjectHolder(ID_SAPPHIRE_CHESTPLATE)
    public static Item SAPPHIRE_CHESTPLATE = null;
    @ObjectHolder(ID_PERIDOT_CHESTPLATE)
    public static Item PERIDOT_CHESTPLATE = null;

    @ObjectHolder(ID_RUBY_LEGGINGS)
    public static Item RUBY_LEGGINGS = null;
    @ObjectHolder(ID_SAPPHIRE_LEGGINGS)
    public static Item SAPPHIRE_LEGGINGS = null;
    @ObjectHolder(ID_PERIDOT_LEGGINGS)
    public static Item PERIDOT_LEGGINGS = null;

    @ObjectHolder(ID_RUBY_BOOTS)
    public static Item RUBY_BOOTS = null;
    @ObjectHolder(ID_SAPPHIRE_BOOTS)
    public static Item SAPPHIRE_BOOTS = null;
    @ObjectHolder(ID_PERIDOT_BOOTS)
    public static Item PERIDOT_BOOTS = null;

    //Backpacks
    @ObjectHolder(ID_WHITE_BACKPACK)
    public static Item WHITE_BACKPACK = null;
    @ObjectHolder(ID_ORANGE_BACKPACK)
    public static Item ORANGE_BACKPACK = null;
    @ObjectHolder(ID_MAGENTA_BACKPACK)
    public static Item MAGENTA_BACKPACK = null;
    @ObjectHolder(ID_LIGHT_BLUE_BACKPACK)
    public static Item LIGHT_BLUE_BACKPACK = null;
    @ObjectHolder(ID_YELLOW_BACKPACK)
    public static Item YELLOW_BACKPACK = null;
    @ObjectHolder(ID_LIME_BACKPACK)
    public static Item LIME_BACKPACK = null;
    @ObjectHolder(ID_PINK_BACKPACK)
    public static Item PINK_BACKPACK = null;
    @ObjectHolder(ID_GRAY_BACKPACK)
    public static Item GRAY_BACKPACK = null;
    @ObjectHolder(ID_LIGHT_GRAY_BACKPACK)
    public static Item LIGHT_GRAY_BACKPACK = null;
    @ObjectHolder(ID_CYAN_BACKPACK)
    public static Item CYAN_BACKPACK = null;
    @ObjectHolder(ID_PURPLE_BACKPACK)
    public static Item PURPLE_BACKPACK = null;
    @ObjectHolder(ID_BLUE_BACKPACK)
    public static Item BLUE_BACKPACK = null;
    @ObjectHolder(ID_BROWN_BACKPACK)
    public static Item BROWN_BACKPACK = null;
    @ObjectHolder(ID_GREEN_BACKPACK)
    public static Item GREEN_BACKPACK = null;
    @ObjectHolder(ID_RED_BACKPACK)
    public static Item RED_BACKPACK = null;
    @ObjectHolder(ID_BLACK_BACKPACK)
    public static Item BLACK_BACKPACK = null;

    /* Containers */
    @ObjectHolder(ID_BACKPACK_CONTAINER)
    public static MenuType<BackpackContainer> BACKPACK_CONTAINER = null;

    /* Recipe Serializers */
    @ObjectHolder(ID_BACKPACK_DYE)
    public static SimpleRecipeSerializer<?> BACKPACK_DYE_RECIPE_SERIALIZER = null;

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
