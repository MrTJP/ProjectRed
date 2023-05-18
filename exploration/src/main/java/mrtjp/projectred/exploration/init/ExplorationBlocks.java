package mrtjp.projectred.exploration.init;

import mrtjp.projectred.exploration.block.ElectrotineOreBlock;
import mrtjp.projectred.exploration.block.OreBlock;
import net.minecraft.block.AbstractBlock;
import net.minecraft.block.Block;
import net.minecraft.block.SoundType;
import net.minecraft.block.WallBlock;
import net.minecraft.block.material.Material;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.fml.RegistryObject;

import static mrtjp.projectred.exploration.ProjectRedExploration.*;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

public class ExplorationBlocks {

    public static final String ID_RUBY_ORE = "ruby_ore";
    public static final String ID_SAPPHIRE_ORE = "sapphire_ore";
    public static final String ID_PERIDOT_ORE = "peridot_ore";
    public static final String ID_COPPER_ORE = "copper_ore";
    public static final String ID_TIN_ORE = "tin_ore";
    public static final String ID_SILVER_ORE = "silver_ore";
    public static final String ID_ELECTROTINE_ORE = "electrotine_ore";

    public static final String ID_MARBLE = "marble";
    public static final String ID_MARBLE_BRICK = "marble_brick";
    public static final String ID_BASALT = "basalt";
    public static final String ID_BASALT_COBBLE = "basalt_cobble";
    public static final String ID_BASALT_BRICK = "basalt_brick";
    public static final String ID_RUBY_BLOCK = "ruby_block";
    public static final String ID_SAPPHIRE_BLOCK = "sapphire_block";
    public static final String ID_PERIDOT_BLOCK = "peridot_block";
    public static final String ID_COPPER_BLOCK = "copper_block";
    public static final String ID_TIN_BLOCK = "tin_block";
    public static final String ID_SILVER_BLOCK = "silver_block";
    public static final String ID_ELECTROTINE_BLOCK = "electrotine_block";

    public static final String ID_MARBLE_WALL = "marble_wall";
    public static final String ID_MARBLE_BRICK_WALL = "marble_brick_wall";
    public static final String ID_BASALT_WALL = "basalt_wall";
    public static final String ID_BASALT_COBBLE_WALL = "basalt_cobble_wall";
    public static final String ID_BASALT_BRICK_WALL = "basalt_brick_wall";
    public static final String ID_RUBY_BLOCK_WALL = "ruby_block_wall";
    public static final String ID_SAPPHIRE_BLOCK_WALL = "sapphire_block_wall";
    public static final String ID_PERIDOT_BLOCK_WALL = "peridot_block_wall";
    public static final String ID_COPPER_BLOCK_WALL = "copper_block_wall";
    public static final String ID_TIN_BLOCK_WALL = "tin_block_wall";
    public static final String ID_SILVER_BLOCK_WALL = "silver_block_wall";
    public static final String ID_ELECTROTINE_BLOCK_WALL = "electrotine_block_wall";

    public static void register() {

        /* Blocks */

        // Ores
        BLOCKS.register(ID_RUBY_ORE,        () -> new OreBlock(2, 2, 7));
        BLOCKS.register(ID_SAPPHIRE_ORE,    () -> new OreBlock(2, 2, 7));
        BLOCKS.register(ID_PERIDOT_ORE,     () -> new OreBlock(2, 2, 7));
        BLOCKS.register(ID_COPPER_ORE,      () -> new OreBlock(1, 0, 0));
        BLOCKS.register(ID_TIN_ORE,         () -> new OreBlock(1, 0, 0));
        BLOCKS.register(ID_SILVER_ORE,      () -> new OreBlock(2, 0, 0));
        BLOCKS.register(ID_ELECTROTINE_ORE, () -> new ElectrotineOreBlock(2, 1, 5));

        // Decorative blocks
        RegistryObject<Block> marbleBlock       = BLOCKS.register(ID_MARBLE,          () -> createDecorativeStoneBlock(2, 1F, 14F));
        RegistryObject<Block> marbleBrickBlock  = BLOCKS.register(ID_MARBLE_BRICK,    () -> createDecorativeStoneBlock(2, 1F, 14F));
        RegistryObject<Block> basaltBlock       = BLOCKS.register(ID_BASALT,          () -> createDecorativeStoneBlock(2, 2.5F, 16F));
        RegistryObject<Block> basaltCobbleBlock = BLOCKS.register(ID_BASALT_COBBLE,   () -> createDecorativeStoneBlock(2, 2.5F, 14F));
        RegistryObject<Block> basaltBrickBlock  = BLOCKS.register(ID_BASALT_BRICK,    () -> createDecorativeStoneBlock(2, 2.5F, 20F));
        RegistryObject<Block> rubyBlock         = BLOCKS.register(ID_RUBY_BLOCK,      () -> createDecorativeGemBlock(2, 5F, 10F));
        RegistryObject<Block> sapphireBlock     = BLOCKS.register(ID_SAPPHIRE_BLOCK,  () -> createDecorativeGemBlock(2, 5F, 10F));
        RegistryObject<Block> peridotBlock      = BLOCKS.register(ID_PERIDOT_BLOCK,   () -> createDecorativeGemBlock(2, 5F, 10F));
        RegistryObject<Block> copperBlock       = BLOCKS.register(ID_COPPER_BLOCK,    () -> createDecorativeMetalBlock(2, 5F, 10F));
        RegistryObject<Block> tinBlock          = BLOCKS.register(ID_TIN_BLOCK,       () -> createDecorativeMetalBlock(2, 5F, 10F));
        RegistryObject<Block> silverBlock       = BLOCKS.register(ID_SILVER_BLOCK,    () -> createDecorativeMetalBlock(2, 5F, 10F));
        RegistryObject<Block> electrotineBlock  = BLOCKS.register(ID_ELECTROTINE_BLOCK, () -> createDecorativeMetalBlock(2, 5F, 10F));

        // Walls
        //Note: Direct refs used here bc ExplorationReferences isnt filled out when wall block suppliers are run
        BLOCKS.register(ID_MARBLE_WALL,         () -> createWallBlock(marbleBlock));
        BLOCKS.register(ID_MARBLE_BRICK_WALL,   () -> createWallBlock(marbleBrickBlock));
        BLOCKS.register(ID_BASALT_WALL,         () -> createWallBlock(basaltBlock));
        BLOCKS.register(ID_BASALT_COBBLE_WALL,  () -> createWallBlock(basaltCobbleBlock));
        BLOCKS.register(ID_BASALT_BRICK_WALL,   () -> createWallBlock(basaltBrickBlock));
        BLOCKS.register(ID_RUBY_BLOCK_WALL,     () -> createWallBlock(rubyBlock));
        BLOCKS.register(ID_SAPPHIRE_BLOCK_WALL, () -> createWallBlock(sapphireBlock));
        BLOCKS.register(ID_PERIDOT_BLOCK_WALL,  () -> createWallBlock(peridotBlock));
        BLOCKS.register(ID_COPPER_BLOCK_WALL,   () -> createWallBlock(copperBlock));
        BLOCKS.register(ID_TIN_BLOCK_WALL,      () -> createWallBlock(tinBlock));
        BLOCKS.register(ID_SILVER_BLOCK_WALL,   () -> createWallBlock(silverBlock));
        BLOCKS.register(ID_ELECTROTINE_BLOCK_WALL, () -> createWallBlock(electrotineBlock));

        /* Block Items */

        // Ores
        ITEMS.register(ID_RUBY_ORE,     () -> createBlockItem(RUBY_ORE_BLOCK));
        ITEMS.register(ID_SAPPHIRE_ORE, () -> createBlockItem(SAPPHIRE_ORE_BLOCK));
        ITEMS.register(ID_PERIDOT_ORE,  () -> createBlockItem(PERIDOT_ORE_BLOCK));
        ITEMS.register(ID_COPPER_ORE,   () -> createBlockItem(COPPER_ORE_BLOCK));
        ITEMS.register(ID_TIN_ORE,      () -> createBlockItem(TIN_ORE_BLOCK));
        ITEMS.register(ID_SILVER_ORE,   () -> createBlockItem(SILVER_ORE_BLOCK));
        ITEMS.register(ID_ELECTROTINE_ORE, () -> createBlockItem(ELECTROTINE_ORE_BLOCK));

        // Decorative blocks
        ITEMS.register(ID_MARBLE,           () -> createBlockItem(MARBLE_BLOCK));
        ITEMS.register(ID_MARBLE_BRICK,     () -> createBlockItem(MARBLE_BRICK_BLOCK));
        ITEMS.register(ID_BASALT,           () -> createBlockItem(BASALT_BLOCK));
        ITEMS.register(ID_BASALT_COBBLE,    () -> createBlockItem(BASALT_COBBLE_BLOCK));
        ITEMS.register(ID_BASALT_BRICK,     () -> createBlockItem(BASALT_BRICK_BLOCK));
        ITEMS.register(ID_RUBY_BLOCK,       () -> createBlockItem(RUBY_BLOCK));
        ITEMS.register(ID_SAPPHIRE_BLOCK,   () -> createBlockItem(SAPPHIRE_BLOCK));
        ITEMS.register(ID_PERIDOT_BLOCK,    () -> createBlockItem(PERIDOT_BLOCK));
        ITEMS.register(ID_COPPER_BLOCK,     () -> createBlockItem(COPPER_BLOCK));
        ITEMS.register(ID_TIN_BLOCK,        () -> createBlockItem(TIN_BLOCK));
        ITEMS.register(ID_SILVER_BLOCK,     () -> createBlockItem(SILVER_BLOCK));
        ITEMS.register(ID_ELECTROTINE_BLOCK,() -> createBlockItem(ELECTROTINE_BLOCK));

        // Walls
        ITEMS.register(ID_MARBLE_WALL,          () -> createBlockItem(MARBLE_WALL));
        ITEMS.register(ID_MARBLE_BRICK_WALL,    () -> createBlockItem(MARBLE_BRICK_WALL));
        ITEMS.register(ID_BASALT_WALL,          () -> createBlockItem(BASALT_WALL));
        ITEMS.register(ID_BASALT_COBBLE_WALL,   () -> createBlockItem(BASALT_COBBLE_WALL));
        ITEMS.register(ID_BASALT_BRICK_WALL,    () -> createBlockItem(BASALT_BRICK_WALL));
        ITEMS.register(ID_RUBY_BLOCK_WALL,      () -> createBlockItem(RUBY_BLOCK_WALL));
        ITEMS.register(ID_SAPPHIRE_BLOCK_WALL,  () -> createBlockItem(SAPPHIRE_BLOCK_WALL));
        ITEMS.register(ID_PERIDOT_BLOCK_WALL,   () -> createBlockItem(PERIDOT_BLOCK_WALL));
        ITEMS.register(ID_COPPER_BLOCK_WALL,    () -> createBlockItem(COPPER_BLOCK_WALL));
        ITEMS.register(ID_TIN_BLOCK_WALL,       () -> createBlockItem(TIN_BLOCK_WALL));
        ITEMS.register(ID_SILVER_BLOCK_WALL,    () -> createBlockItem(SILVER_BLOCK_WALL));
        ITEMS.register(ID_ELECTROTINE_BLOCK_WALL, () -> createBlockItem(ELECTROTINE_BLOCK_WALL));
    }

    private static Block createWallBlock(RegistryObject<Block> block) {
        return new WallBlock(AbstractBlock.Properties.copy(block.get()));
    }

    private static Block createWallBlock(Block block) {
        return new WallBlock(AbstractBlock.Properties.copy(block));
    }

    private static Block createDecorativeStoneBlock(int harvestLevel, float hardness, float resistance) {
        return new Block(AbstractBlock.Properties.of(Material.STONE)
                .strength(hardness, resistance)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE)
                .requiresCorrectToolForDrops()
                .sound(SoundType.STONE));
    }

    private static Block createDecorativeGemBlock(int harvestLevel, float hardness, float resistance) {
        return new Block(AbstractBlock.Properties.of(Material.METAL)
                .strength(hardness, resistance)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE)
                .requiresCorrectToolForDrops()
                .sound(SoundType.METAL));
    }

    private static Block createDecorativeMetalBlock(int harvestLevel, float hardness, float resistance) {
        return new Block(AbstractBlock.Properties.of(Material.METAL)
                .strength(hardness, resistance)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE)
                .requiresCorrectToolForDrops()
                .sound(SoundType.METAL));
    }

    private static Item createBlockItem(Block block) {
        return new BlockItem(block, new Item.Properties().tab(EXPLORATION_GROUP));
    }
}
