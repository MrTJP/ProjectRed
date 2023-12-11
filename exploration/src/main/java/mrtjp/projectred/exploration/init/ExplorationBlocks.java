package mrtjp.projectred.exploration.init;

import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.microblock.api.MicroMaterial;
import codechicken.microblock.util.MicroMaterialRegistry;
import mrtjp.projectred.exploration.block.ElectrotineOreBlock;
import net.minecraft.util.valueproviders.UniformInt;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.DropExperienceBlock;
import net.minecraft.world.level.block.SoundType;
import net.minecraft.world.level.block.WallBlock;
import net.minecraft.world.level.block.state.BlockBehaviour;
import net.minecraft.world.level.material.Material;
import net.minecraft.world.level.material.MaterialColor;
import net.minecraftforge.registries.RegisterEvent;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.exploration.ProjectRedExploration.*;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationBlocks {

    public static final String ID_RUBY_ORE = "ruby_ore";
    public static final String ID_DEEPSLATE_RUBY_ORE = "deepslate_ruby_ore";
    public static final String ID_SAPPHIRE_ORE = "sapphire_ore";
    public static final String ID_DEEPSLATE_SAPPHIRE_ORE = "deepslate_sapphire_ore";
    public static final String ID_PERIDOT_ORE = "peridot_ore";
    public static final String ID_DEEPSLATE_PERIDOT_ORE = "deepslate_peridot_ore";
    public static final String ID_TIN_ORE = "tin_ore";
    public static final String ID_DEEPSLATE_TIN_ORE = "deepslate_tin_ore";
    public static final String ID_SILVER_ORE = "silver_ore";
    public static final String ID_DEEPSLATE_ELECTROTINE_ORE = "deepslate_electrotine_ore";
    public static final String ID_ELECTROTINE_ORE = "electrotine_ore";
    public static final String ID_DEEPSLATE_SILVER_ORE = "deepslate_silver_ore";

    public static final String ID_MARBLE = "marble";
    public static final String ID_MARBLE_BRICK = "marble_brick";
    public static final String ID_BASALT = "basalt";
    public static final String ID_BASALT_COBBLE = "basalt_cobble";
    public static final String ID_BASALT_BRICK = "basalt_brick";
    public static final String ID_RUBY_BLOCK = "ruby_block";
    public static final String ID_SAPPHIRE_BLOCK = "sapphire_block";
    public static final String ID_PERIDOT_BLOCK = "peridot_block";
    public static final String ID_RAW_TIN_BLOCK = "raw_tin_block";
    public static final String ID_RAW_SILVER_BLOCK = "raw_silver_block";
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
    public static final String ID_ELECTROTINE_BLOCK_WALL = "electrotine_block_wall";

    // Ores
    public static RegistryObject<Block> RUBY_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_RUBY_ORE_BLOCK;
    public static RegistryObject<Block> SAPPHIRE_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_SAPPHIRE_ORE_BLOCK;
    public static RegistryObject<Block> PERIDOT_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_PERIDOT_ORE_BLOCK;
    public static RegistryObject<Block> TIN_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_TIN_ORE_BLOCK;
    public static RegistryObject<Block> SILVER_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_SILVER_ORE_BLOCK;
    public static RegistryObject<Block> ELECTROTINE_ORE_BLOCK;
    public static RegistryObject<Block> DEEPSLATE_ELECTROTINE_ORE_BLOCK;

    // Decorative blocks
    public static RegistryObject<Block> MARBLE_BLOCK;
    public static RegistryObject<Block> MARBLE_BRICK_BLOCK;
    public static RegistryObject<Block> BASALT_BLOCK;
    public static RegistryObject<Block> BASALT_COBBLE_BLOCK;
    public static RegistryObject<Block> BASALT_BRICK_BLOCK;
    public static RegistryObject<Block> RUBY_BLOCK;
    public static RegistryObject<Block> SAPPHIRE_BLOCK;
    public static RegistryObject<Block> PERIDOT_BLOCK;
    public static RegistryObject<Block> ELECTROTINE_BLOCK;
    public static RegistryObject<Block> RAW_TIN_BLOCK;
    public static RegistryObject<Block> RAW_SILVER_BLOCK;
    public static RegistryObject<Block> TIN_BLOCK;
    public static RegistryObject<Block> SILVER_BLOCK;

    // Walls
    public static RegistryObject<WallBlock> MARBLE_WALL;
    public static RegistryObject<WallBlock> MARBLE_BRICK_WALL;
    public static RegistryObject<WallBlock> BASALT_WALL;
    public static RegistryObject<WallBlock> BASALT_COBBLE_WALL;
    public static RegistryObject<WallBlock> BASALT_BRICK_WALL;
    public static RegistryObject<WallBlock> RUBY_BLOCK_WALL;
    public static RegistryObject<WallBlock> SAPPHIRE_BLOCK_WALL;
    public static RegistryObject<WallBlock> PERIDOT_BLOCK_WALL;
    public static RegistryObject<WallBlock> ELECTROTINE_BLOCK_WALL;


    public static void register() {

        // Ores
        RUBY_ORE_BLOCK                  = BLOCKS.register(ID_RUBY_ORE,                    () -> createOreBlock(false, 2, 7));
        DEEPSLATE_RUBY_ORE_BLOCK        = BLOCKS.register(ID_DEEPSLATE_RUBY_ORE,          () -> createOreBlock(true,  2, 7));
        SAPPHIRE_ORE_BLOCK              = BLOCKS.register(ID_SAPPHIRE_ORE,                () -> createOreBlock(false, 2, 7));
        DEEPSLATE_SAPPHIRE_ORE_BLOCK    = BLOCKS.register(ID_DEEPSLATE_SAPPHIRE_ORE,      () -> createOreBlock(true,  2, 7));
        PERIDOT_ORE_BLOCK               = BLOCKS.register(ID_PERIDOT_ORE,                 () -> createOreBlock(false, 2, 7));
        DEEPSLATE_PERIDOT_ORE_BLOCK     = BLOCKS.register(ID_DEEPSLATE_PERIDOT_ORE,       () -> createOreBlock(true,  2, 7));
        TIN_ORE_BLOCK                   = BLOCKS.register(ID_TIN_ORE,                     () -> createOreBlock(false, 0, 0));
        DEEPSLATE_TIN_ORE_BLOCK         = BLOCKS.register(ID_DEEPSLATE_TIN_ORE,           () -> createOreBlock(true,  0, 0));
        SILVER_ORE_BLOCK                = BLOCKS.register(ID_SILVER_ORE,                  () -> createOreBlock(false, 0, 0));
        DEEPSLATE_SILVER_ORE_BLOCK      = BLOCKS.register(ID_DEEPSLATE_SILVER_ORE,        () -> createOreBlock(true,  0, 0));
        ELECTROTINE_ORE_BLOCK           = BLOCKS.register(ID_ELECTROTINE_ORE,             () -> createElectrotineOreBlock(false, 1, 5));
        DEEPSLATE_ELECTROTINE_ORE_BLOCK = BLOCKS.register(ID_DEEPSLATE_ELECTROTINE_ORE,   () -> createElectrotineOreBlock(true,  1, 5));

        // Decorative blocks
        MARBLE_BLOCK        = BLOCKS.register(ID_MARBLE,            () -> createDecorativeStoneBlock(1F, 14F));
        MARBLE_BRICK_BLOCK  = BLOCKS.register(ID_MARBLE_BRICK,      () -> createDecorativeStoneBlock(1F, 14F));
        BASALT_BLOCK        = BLOCKS.register(ID_BASALT,            () -> createDecorativeStoneBlock(2.5F, 16F));
        BASALT_COBBLE_BLOCK = BLOCKS.register(ID_BASALT_COBBLE,     () -> createDecorativeStoneBlock(2.5F, 14F));
        BASALT_BRICK_BLOCK  = BLOCKS.register(ID_BASALT_BRICK,      () -> createDecorativeStoneBlock(2.5F, 20F));
        RUBY_BLOCK          = BLOCKS.register(ID_RUBY_BLOCK,        () -> createDecorativeGemBlock(5F, 10F));
        SAPPHIRE_BLOCK      = BLOCKS.register(ID_SAPPHIRE_BLOCK,    () -> createDecorativeGemBlock(5F, 10F));
        PERIDOT_BLOCK       = BLOCKS.register(ID_PERIDOT_BLOCK,     () -> createDecorativeGemBlock(5F, 10F));
        ELECTROTINE_BLOCK   = BLOCKS.register(ID_ELECTROTINE_BLOCK, () -> createDecorativeMetalBlock(5F, 10F));
        RAW_TIN_BLOCK       = BLOCKS.register(ID_RAW_TIN_BLOCK,     () -> createDecorativeMetalBlock(2F, 6F));
        RAW_SILVER_BLOCK    = BLOCKS.register(ID_TIN_BLOCK,         () -> createDecorativeMetalBlock(2F, 6F));
        TIN_BLOCK           = BLOCKS.register(ID_RAW_SILVER_BLOCK,  () -> createDecorativeMetalBlock(3F, 6F));
        SILVER_BLOCK        = BLOCKS.register(ID_SILVER_BLOCK,      () -> createDecorativeMetalBlock(3F, 6F));

        // Walls
        MARBLE_WALL            = BLOCKS.register(ID_MARBLE_WALL,            () -> createWallBlock(MARBLE_BLOCK));
        MARBLE_BRICK_WALL      = BLOCKS.register(ID_MARBLE_BRICK_WALL,      () -> createWallBlock(MARBLE_BRICK_BLOCK));
        BASALT_WALL            = BLOCKS.register(ID_BASALT_WALL,            () -> createWallBlock(BASALT_BLOCK));
        BASALT_COBBLE_WALL     = BLOCKS.register(ID_BASALT_COBBLE_WALL,     () -> createWallBlock(BASALT_COBBLE_BLOCK));
        BASALT_BRICK_WALL      = BLOCKS.register(ID_BASALT_BRICK_WALL,      () -> createWallBlock(BASALT_BRICK_BLOCK));
        RUBY_BLOCK_WALL        = BLOCKS.register(ID_RUBY_BLOCK_WALL,        () -> createWallBlock(RUBY_BLOCK));
        SAPPHIRE_BLOCK_WALL    = BLOCKS.register(ID_SAPPHIRE_BLOCK_WALL,    () -> createWallBlock(SAPPHIRE_BLOCK));
        PERIDOT_BLOCK_WALL     = BLOCKS.register(ID_PERIDOT_BLOCK_WALL,     () -> createWallBlock(PERIDOT_BLOCK));
        ELECTROTINE_BLOCK_WALL = BLOCKS.register(ID_ELECTROTINE_BLOCK_WALL, () -> createWallBlock(ELECTROTINE_BLOCK));

        /* Block Items */

        // Ores
        ITEMS.register(ID_RUBY_ORE,                     () -> createBlockItem(RUBY_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_RUBY_ORE,           () -> createBlockItem(DEEPSLATE_RUBY_ORE_BLOCK));
        ITEMS.register(ID_SAPPHIRE_ORE,                 () -> createBlockItem(SAPPHIRE_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_SAPPHIRE_ORE,       () -> createBlockItem(DEEPSLATE_SAPPHIRE_ORE_BLOCK));
        ITEMS.register(ID_PERIDOT_ORE,                  () -> createBlockItem(PERIDOT_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_PERIDOT_ORE,        () -> createBlockItem(DEEPSLATE_PERIDOT_ORE_BLOCK));
        ITEMS.register(ID_TIN_ORE,                      () -> createBlockItem(TIN_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_TIN_ORE,            () -> createBlockItem(DEEPSLATE_TIN_ORE_BLOCK));
        ITEMS.register(ID_SILVER_ORE,                   () -> createBlockItem(SILVER_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_SILVER_ORE,         () -> createBlockItem(DEEPSLATE_SILVER_ORE_BLOCK));
        ITEMS.register(ID_ELECTROTINE_ORE,              () -> createBlockItem(ELECTROTINE_ORE_BLOCK));
        ITEMS.register(ID_DEEPSLATE_ELECTROTINE_ORE,    () -> createBlockItem(DEEPSLATE_ELECTROTINE_ORE_BLOCK));

        // Decorative blocks
        ITEMS.register(ID_MARBLE,           () -> createBlockItem(MARBLE_BLOCK));
        ITEMS.register(ID_MARBLE_BRICK,     () -> createBlockItem(MARBLE_BRICK_BLOCK));
        ITEMS.register(ID_BASALT,           () -> createBlockItem(BASALT_BLOCK));
        ITEMS.register(ID_BASALT_COBBLE,    () -> createBlockItem(BASALT_COBBLE_BLOCK));
        ITEMS.register(ID_BASALT_BRICK,     () -> createBlockItem(BASALT_BRICK_BLOCK));
        ITEMS.register(ID_RUBY_BLOCK,       () -> createBlockItem(RUBY_BLOCK));
        ITEMS.register(ID_SAPPHIRE_BLOCK,   () -> createBlockItem(SAPPHIRE_BLOCK));
        ITEMS.register(ID_PERIDOT_BLOCK,    () -> createBlockItem(PERIDOT_BLOCK));
        ITEMS.register(ID_ELECTROTINE_BLOCK,() -> createBlockItem(ELECTROTINE_BLOCK));
        ITEMS.register(ID_RAW_TIN_BLOCK,    () -> createBlockItem(RAW_TIN_BLOCK));
        ITEMS.register(ID_TIN_BLOCK,        () -> createBlockItem(TIN_BLOCK));
        ITEMS.register(ID_RAW_SILVER_BLOCK, () -> createBlockItem(RAW_SILVER_BLOCK));
        ITEMS.register(ID_SILVER_BLOCK,     () -> createBlockItem(SILVER_BLOCK));

        // Walls
        ITEMS.register(ID_MARBLE_WALL,          () -> createBlockItem(MARBLE_WALL));
        ITEMS.register(ID_MARBLE_BRICK_WALL,    () -> createBlockItem(MARBLE_BRICK_WALL));
        ITEMS.register(ID_BASALT_WALL,          () -> createBlockItem(BASALT_WALL));
        ITEMS.register(ID_BASALT_COBBLE_WALL,   () -> createBlockItem(BASALT_COBBLE_WALL));
        ITEMS.register(ID_BASALT_BRICK_WALL,    () -> createBlockItem(BASALT_BRICK_WALL));
        ITEMS.register(ID_RUBY_BLOCK_WALL,      () -> createBlockItem(RUBY_BLOCK_WALL));
        ITEMS.register(ID_SAPPHIRE_BLOCK_WALL,  () -> createBlockItem(SAPPHIRE_BLOCK_WALL));
        ITEMS.register(ID_PERIDOT_BLOCK_WALL,   () -> createBlockItem(PERIDOT_BLOCK_WALL));
        ITEMS.register(ID_ELECTROTINE_BLOCK_WALL, () -> createBlockItem(ELECTROTINE_BLOCK_WALL));
    }

    public static void onRegisterMicroMaterials(RegisterEvent event) {

        event.register(MicroMaterialRegistry.MICRO_MATERIALS.getRegistryKey(), r -> {
            registerBlockMicro(r, new BlockMicroMaterial(MARBLE_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(MARBLE_BRICK_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(BASALT_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(BASALT_COBBLE_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(BASALT_BRICK_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(RUBY_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(SAPPHIRE_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(PERIDOT_BLOCK.get()));
            registerBlockMicro(r, new BlockMicroMaterial(ELECTROTINE_BLOCK.get()));
        });
    }

    private static void registerBlockMicro(RegisterEvent.RegisterHelper<MicroMaterial> r, BlockMicroMaterial material) {
        r.register(BlockMicroMaterial.makeMaterialKey(material.state), material);
    }

    private static Block createOreBlock(boolean isDeepslate, int minxp, int maxxp) {
        return new DropExperienceBlock(BlockBehaviour.Properties.of(Material.STONE)
                .strength(isDeepslate ? 4.5F : 3.0F, 3.0F)
                .requiresCorrectToolForDrops()
                .color(isDeepslate ? MaterialColor.DEEPSLATE : MaterialColor.STONE)
                .sound(isDeepslate ? SoundType.DEEPSLATE : SoundType.STONE), UniformInt.of(minxp, maxxp));
    }

    private static Block createElectrotineOreBlock(boolean isDeepslate, int minxp, int maxxp) {

        return new ElectrotineOreBlock(BlockBehaviour.Properties.of(Material.STONE)
                .strength(isDeepslate ? 4.5F : 3.0F, 3.0F)
                .requiresCorrectToolForDrops()
                .color(isDeepslate ? MaterialColor.DEEPSLATE : MaterialColor.STONE)
                .sound(isDeepslate ? SoundType.DEEPSLATE : SoundType.STONE), UniformInt.of(minxp, maxxp));
    }

    private static WallBlock createWallBlock(RegistryObject<Block> block) {
        return new WallBlock(BlockBehaviour.Properties.copy(block.get()));
    }

    private static Block createDecorativeStoneBlock(float hardness, float resistance) {
        return new Block(BlockBehaviour.Properties.of(Material.STONE)
                .strength(hardness, resistance)
                .requiresCorrectToolForDrops()
                .sound(SoundType.STONE));
    }

    private static Block createDecorativeGemBlock(float hardness, float resistance) {
        return new Block(BlockBehaviour.Properties.of(Material.METAL)
                .strength(hardness, resistance)
                .requiresCorrectToolForDrops()
                .sound(SoundType.METAL));
    }

    private static Block createDecorativeMetalBlock(float hardness, float resistance) {
        return new Block(BlockBehaviour.Properties.of(Material.METAL)
                .strength(hardness, resistance)
                .requiresCorrectToolForDrops()
                .sound(SoundType.METAL));
    }

    private static Item createBlockItem(RegistryObject<? extends Block> block) {
        return new BlockItem(block.get(), new Item.Properties().tab(EXPLORATION_CREATIVE_TAB));
    }

    private static Item createBlockItem(Block block) {
        return new BlockItem(block, new Item.Properties().tab(EXPLORATION_CREATIVE_TAB));
    }
}
