package mrtjp.projectred.exploration.init;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

public class ExplorationTags {

    /* Block Tags */

    // Ores
    public static final TagKey<Block> RUBY_ORES_BLOCK_TAG           = BlockTags.create(new ResourceLocation("forge:ores/ruby"));
    public static final TagKey<Block> SAPPHIRE_ORES_BLOCK_TAG       = BlockTags.create(new ResourceLocation("forge:ores/sapphire"));
    public static final TagKey<Block> PERIDOT_ORES_BLOCK_TAG        = BlockTags.create(new ResourceLocation("forge:ores/peridot"));
    public static final TagKey<Block> COPPER_ORES_BLOCK_TAG         = BlockTags.create(new ResourceLocation("forge:ores/copper"));
    public static final TagKey<Block> TIN_ORES_BLOCK_TAG            = BlockTags.create(new ResourceLocation("forge:ores/tin"));
    public static final TagKey<Block> SILVER_ORES_BLOCK_TAG         = BlockTags.create(new ResourceLocation("forge:ores/silver"));
    public static final TagKey<Block> ELECTROTINE_ORES_BLOCK_TAG    = BlockTags.create(new ResourceLocation("forge:ores/electrotine"));

    // Decorative blocks
    public static final TagKey<Block> MARBLE_BLOCK_TAG              = BlockTags.create(new ResourceLocation("forge:stone/marble"));
    public static final TagKey<Block> BASALT_BLOCK_TAG              = BlockTags.create(new ResourceLocation("forge:stone/basalt"));
    public static final TagKey<Block> RUBY_STORAGE_BLOCK_TAG        = BlockTags.create(new ResourceLocation("forge:storage_blocks/ruby"));
    public static final TagKey<Block> SAPPHIRE_STORAGE_BLOCK_TAG    = BlockTags.create(new ResourceLocation("forge:storage_blocks/sapphire"));
    public static final TagKey<Block> PERIDOT_STORAGE_BLOCK_TAG     = BlockTags.create(new ResourceLocation("forge:storage_blocks/peridot"));
    public static final TagKey<Block> COPPER_STORAGE_BLOCK_TAG      = BlockTags.create(new ResourceLocation("forge:storage_blocks/copper"));
    public static final TagKey<Block> ELECTROTINE_STORAGE_BLOCK_TAG = BlockTags.create(new ResourceLocation("forge:storage_blocks/electrotine"));
    public static final TagKey<Block> RAW_TIN_STORAGE_BLOCK_TAG     = BlockTags.create(new ResourceLocation("forge:storage_blocks/raw_tin"));
    public static final TagKey<Block> TIN_STORAGE_BLOCK_TAG         = BlockTags.create(new ResourceLocation("forge:storage_blocks/tin"));
    public static final TagKey<Block> RAW_SILVER_STORAGE_BLOCK_TAG  = BlockTags.create(new ResourceLocation("forge:storage_blocks/raw_silver"));
    public static final TagKey<Block> SILVER_STORAGE_BLOCK_TAG      = BlockTags.create(new ResourceLocation("forge:storage_blocks/silver"));

    /* Item Tags */

    // Ore Item Blocks
    public static final TagKey<Item> RUBY_ORES_BLOCK_ITEM_TAG           = ItemTags.create(new ResourceLocation("forge:ores/ruby"));
    public static final TagKey<Item> SAPPHIRE_ORES_BLOCK_ITEM_TAG       = ItemTags.create(new ResourceLocation("forge:ores/sapphire"));
    public static final TagKey<Item> PERIDOT_ORES_BLOCK_ITEM_TAG        = ItemTags.create(new ResourceLocation("forge:ores/peridot"));
    public static final TagKey<Item> COPPER_ORES_BLOCK_ITEM_TAG         = ItemTags.create(new ResourceLocation("forge:ores/copper"));
    public static final TagKey<Item> TIN_ORES_BLOCK_ITEM_TAG            = ItemTags.create(new ResourceLocation("forge:ores/tin"));
    public static final TagKey<Item> SILVER_ORES_BLOCK_ITEM_TAG         = ItemTags.create(new ResourceLocation("forge:ores/silver"));
    public static final TagKey<Item> ELECTROTINE_ORES_BLOCK_ITEM_TAG    = ItemTags.create(new ResourceLocation("forge:ores/electrotine"));

    // Decorative Item Blocks
    public static final TagKey<Item> MARBLE_BLOCK_ITEM_TAG              = ItemTags.create(new ResourceLocation("forge:stone/marble"));
    public static final TagKey<Item> BASALT_BLOCK_ITEM_TAG              = ItemTags.create(new ResourceLocation("forge:stone/basalt"));
    public static final TagKey<Item> RUBY_STORAGE_BLOCK_ITEM_TAG        = ItemTags.create(new ResourceLocation("forge:storage_blocks/ruby"));
    public static final TagKey<Item> SAPPHIRE_STORAGE_BLOCK_ITEM_TAG    = ItemTags.create(new ResourceLocation("forge:storage_blocks/sapphire"));
    public static final TagKey<Item> PERIDOT_STORAGE_BLOCK_ITEM_TAG     = ItemTags.create(new ResourceLocation("forge:storage_blocks/peridot"));
    public static final TagKey<Item> ELECTROTINE_STORAGE_BLOCK_ITEM_TAG = ItemTags.create(new ResourceLocation("forge:storage_blocks/electrotine"));
    public static final TagKey<Item> RAW_TIN_STORAGE_BLOCK_ITEM_TAG     = ItemTags.create(new ResourceLocation("forge:storage_blocks/tin"));
    public static final TagKey<Item> RAW_SILVER_STORAGE_BLOCK_ITEM_TAG  = ItemTags.create(new ResourceLocation("forge:storage_blocks/raw_silver"));
    public static final TagKey<Item> TIN_STORAGE_BLOCK_ITEM_TAG         = ItemTags.create(new ResourceLocation("forge:storage_blocks/tin"));
    public static final TagKey<Item> SILVER_STORAGE_BLOCK_ITEM_TAG      = ItemTags.create(new ResourceLocation("forge:storage_blocks/silver"));

    // Items
    public static final TagKey<Item> RAW_MATERIALS_TIN_TAG      = ItemTags.create(new ResourceLocation("forge:raw_materials/tin"));
    public static final TagKey<Item> RAW_MATERIALS_SILVER_TAG   = ItemTags.create(new ResourceLocation("forge:raw_materials/silver"));

    // Backpacks
    public static final TagKey<Item> BACKPACKS_TAG             = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks"));
    public static final TagKey<Item> BACKPACKS_DISALLOWED_TAG  = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/disallowed"));

    public static final TagKey<Item> WHITE_BACKPACK_TAG      = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/white"));
    public static final TagKey<Item> ORANGE_BACKPACK_TAG     = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/orange"));
    public static final TagKey<Item> MAGENTA_BACKPACK_TAG    = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/magenta"));
    public static final TagKey<Item> LIGHT_BLUE_BACKPACK_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/light_blue"));
    public static final TagKey<Item> YELLOW_BACKPACK_TAG     = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/yellow"));
    public static final TagKey<Item> LIME_BACKPACK_TAG       = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/lime"));
    public static final TagKey<Item> PINK_BACKPACK_TAG       = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/pink"));
    public static final TagKey<Item> GRAY_BACKPACK_TAG       = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/gray"));
    public static final TagKey<Item> LIGHT_GRAY_BACKPACK_TAG = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/light_gray"));
    public static final TagKey<Item> CYAN_BACKPACK_TAG       = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/cyan"));
    public static final TagKey<Item> PURPLE_BACKPACK_TAG     = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/purple"));
    public static final TagKey<Item> BLUE_BACKPACK_TAG       = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/blue"));
    public static final TagKey<Item> BROWN_BACKPACK_TAG      = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/brown"));
    public static final TagKey<Item> GREEN_BACKPACK_TAG      = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/green"));
    public static final TagKey<Item> RED_BACKPACK_TAG        = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/red"));
    public static final TagKey<Item> BLACK_BACKPACK_TAG      = ItemTags.create(new ResourceLocation(MOD_ID, "backpacks/black"));
}
