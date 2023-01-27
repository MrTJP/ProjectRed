package mrtjp.projectred.exploration.init;

import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.tags.BlockTags;
import net.minecraft.tags.ITag;
import net.minecraft.tags.ItemTags;
import net.minecraft.util.ResourceLocation;

import static mrtjp.projectred.ProjectRedExploration.MOD_ID;

public class ExplorationTags {

    /* Block Tags */

    // Ores
    public static final ITag.INamedTag<Block> RUBY_ORE_BLOCK_TAG           = BlockTags.bind("forge:ores/ruby");
    public static final ITag.INamedTag<Block> SAPPHIRE_ORE_BLOCK_TAG       = BlockTags.bind("forge:ores/sapphire");
    public static final ITag.INamedTag<Block> PERIDOT_ORE_BLOCK_TAG        = BlockTags.bind("forge:ores/peridot");
    public static final ITag.INamedTag<Block> COPPER_ORE_BLOCK_TAG         = BlockTags.bind("forge:ores/copper");
    public static final ITag.INamedTag<Block> TIN_ORE_BLOCK_TAG            = BlockTags.bind("forge:ores/tin");
    public static final ITag.INamedTag<Block> SILVER_ORE_BLOCK_TAG         = BlockTags.bind("forge:ores/silver");
    public static final ITag.INamedTag<Block> ELECTROTINE_ORE_BLOCK_TAG    = BlockTags.bind("forge:ores/electrotine");

    // Decorative blocks
    public static final ITag.INamedTag<Block> MARBLE_BLOCK_TAG              = BlockTags.bind("forge:stone/marble");
    public static final ITag.INamedTag<Block> BASALT_BLOCK_TAG              = BlockTags.bind("forge:stone/basalt");
    public static final ITag.INamedTag<Block> RUBY_STORAGE_BLOCK_TAG        = BlockTags.bind("forge:storage_blocks/ruby");
    public static final ITag.INamedTag<Block> SAPPHIRE_STORAGE_BLOCK_TAG    = BlockTags.bind("forge:storage_blocks/sapphire");
    public static final ITag.INamedTag<Block> PERIDOT_STORAGE_BLOCK_TAG     = BlockTags.bind("forge:storage_blocks/peridot");
    public static final ITag.INamedTag<Block> COPPER_STORAGE_BLOCK_TAG      = BlockTags.bind("forge:storage_blocks/copper");
    public static final ITag.INamedTag<Block> TIN_STORAGE_BLOCK_TAG         = BlockTags.bind("forge:storage_blocks/tin");
    public static final ITag.INamedTag<Block> SILVER_STORAGE_BLOCK_TAG      = BlockTags.bind("forge:storage_blocks/silver");
    public static final ITag.INamedTag<Block> ELECTROTINE_STORAGE_BLOCK_TAG = BlockTags.bind("forge:storage_blocks/electrotine");

    /* Item Tags */

    // Ore Item Blocks
    public static final ITag.INamedTag<Item> RUBY_ORE_BLOCK_ITEM_TAG           = ItemTags.bind("forge:ores/ruby");
    public static final ITag.INamedTag<Item> SAPPHIRE_ORE_BLOCK_ITEM_TAG       = ItemTags.bind("forge:ores/sapphire");
    public static final ITag.INamedTag<Item> PERIDOT_ORE_BLOCK_ITEM_TAG        = ItemTags.bind("forge:ores/peridot");
    public static final ITag.INamedTag<Item> COPPER_ORE_BLOCK_ITEM_TAG         = ItemTags.bind("forge:ores/copper");
    public static final ITag.INamedTag<Item> TIN_ORE_BLOCK_ITEM_TAG            = ItemTags.bind("forge:ores/tin");
    public static final ITag.INamedTag<Item> SILVER_ORE_BLOCK_ITEM_TAG         = ItemTags.bind("forge:ores/silver");
    public static final ITag.INamedTag<Item> ELECTROTINE_ORE_BLOCK_ITEM_TAG    = ItemTags.bind("forge:ores/electrotine");

    // Decorative Item Blocks
    public static final ITag.INamedTag<Item> MARBLE_BLOCK_ITEM_TAG              = ItemTags.bind("forge:stone/marble");
    public static final ITag.INamedTag<Item> BASALT_BLOCK_ITEM_TAG              = ItemTags.bind("forge:stone/basalt");
    public static final ITag.INamedTag<Item> RUBY_STORAGE_BLOCK_ITEM_TAG        = ItemTags.bind("forge:storage_blocks/ruby");
    public static final ITag.INamedTag<Item> SAPPHIRE_STORAGE_BLOCK_ITEM_TAG    = ItemTags.bind("forge:storage_blocks/sapphire");
    public static final ITag.INamedTag<Item> PERIDOT_STORAGE_BLOCK_ITEM_TAG     = ItemTags.bind("forge:storage_blocks/peridot");
    public static final ITag.INamedTag<Item> COPPER_STORAGE_BLOCK_ITEM_TAG      = ItemTags.bind("forge:storage_blocks/copper");
    public static final ITag.INamedTag<Item> TIN_STORAGE_BLOCK_ITEM_TAG         = ItemTags.bind("forge:storage_blocks/tin");
    public static final ITag.INamedTag<Item> SILVER_STORAGE_BLOCK_ITEM_TAG      = ItemTags.bind("forge:storage_blocks/silver");
    public static final ITag.INamedTag<Item> ELECTROTINE_STORAGE_BLOCK_ITEM_TAG = ItemTags.bind("forge:storage_blocks/electrotine");

    // Backpacks
    public static final ITag.INamedTag<Item> BACKPACKS_TAG             = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks").toString());
    public static final ITag.INamedTag<Item> BACKPACKS_DISALLOWED_TAG  = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/disallowed").toString());

    public static final ITag.INamedTag<Item> WHITE_BACKPACK_TAG      = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/white").toString());
    public static final ITag.INamedTag<Item> ORANGE_BACKPACK_TAG     = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/orange").toString());
    public static final ITag.INamedTag<Item> MAGENTA_BACKPACK_TAG    = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/magenta").toString());
    public static final ITag.INamedTag<Item> LIGHT_BLUE_BACKPACK_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/light_blue").toString());
    public static final ITag.INamedTag<Item> YELLOW_BACKPACK_TAG     = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/yellow").toString());
    public static final ITag.INamedTag<Item> LIME_BACKPACK_TAG       = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/lime").toString());
    public static final ITag.INamedTag<Item> PINK_BACKPACK_TAG       = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/pink").toString());
    public static final ITag.INamedTag<Item> GRAY_BACKPACK_TAG       = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/gray").toString());
    public static final ITag.INamedTag<Item> LIGHT_GRAY_BACKPACK_TAG = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/light_gray").toString());
    public static final ITag.INamedTag<Item> CYAN_BACKPACK_TAG       = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/cyan").toString());
    public static final ITag.INamedTag<Item> PURPLE_BACKPACK_TAG     = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/purple").toString());
    public static final ITag.INamedTag<Item> BLUE_BACKPACK_TAG       = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/blue").toString());
    public static final ITag.INamedTag<Item> BROWN_BACKPACK_TAG      = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/brown").toString());
    public static final ITag.INamedTag<Item> GREEN_BACKPACK_TAG      = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/green").toString());
    public static final ITag.INamedTag<Item> RED_BACKPACK_TAG        = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/red").toString());
    public static final ITag.INamedTag<Item> BLACK_BACKPACK_TAG      = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/black").toString());
}
