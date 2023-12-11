package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.block.*;
import mrtjp.projectred.expansion.tile.*;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.expansion.ProjectRedExpansion.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class ExpansionBlocks {

    public static final String ID_PROJECT_BENCH = "project_bench";
    public static final String ID_BATTERY_BOX = "battery_box";
    public static final String ID_AUTO_CRAFTER = "auto_crafter";
    public static final String ID_CHARGING_BENCH = "charging_bench";
    public static final String ID_FIRE_STARTER = "fire_starter";
    public static final String ID_FRAME = "frame";
    public static final String ID_FRAME_MOTOR = "frame_motor";
    public static final String ID_FRAME_ACTUATOR = "frame_actuator";

//    public static final String ID_INDUCTION_FURNACE = "induction_furnace";
//    public static final String ID_TELEPOSER = "teleposer";

    // Blocks
    public static RegistryObject<Block> FRAME_BLOCK;

    public static RegistryObject<Block> PROJECT_BENCH_BLOCK;
    public static RegistryObject<Block> BATTERY_BOX_BLOCK;
    public static RegistryObject<Block> CHARGING_BENCH_BLOCK;
    public static RegistryObject<Block> AUTO_CRAFTER_BLOCK;
    public static RegistryObject<Block> FIRE_STARTER_BLOCK;
    public static RegistryObject<Block> FRAME_MOTOR_BLOCK;
    public static RegistryObject<Block> FRAME_ACTUATOR_BLOCK;
//    //    public static Block INDUCTION_FURNACE_BLOCK;
//    //    public static Block TELEPOSER_BLOCK;

    // Tiles
    public static RegistryObject<BlockEntityType<?>> PROJECT_BENCH_TILE;
    public static RegistryObject<BlockEntityType<?>> BATTERY_BOX_TILE;
    public static RegistryObject<BlockEntityType<?>> CHARGING_BENCH_TILE;
    public static RegistryObject<BlockEntityType<?>> AUTO_CRAFTER_TILE;
    public static RegistryObject<BlockEntityType<?>> FIRE_STARTER_TILE;
    public static RegistryObject<BlockEntityType<?>> FRAME_MOTOR_TILE;
    public static RegistryObject<BlockEntityType<?>> FRAME_ACTUATOR_TILE;


    public static void register() {

        /* Entity Blocks */

        // Blocks
        PROJECT_BENCH_BLOCK = BLOCKS.register(ID_PROJECT_BENCH, ProjectBenchBlock::new);
        BATTERY_BOX_BLOCK = BLOCKS.register(ID_BATTERY_BOX, BatteryBoxBlock::new);
        CHARGING_BENCH_BLOCK = BLOCKS.register(ID_CHARGING_BENCH, ChargingBenchBlock::new);
        AUTO_CRAFTER_BLOCK = BLOCKS.register(ID_AUTO_CRAFTER, AutoCrafterBlock::new);
        FIRE_STARTER_BLOCK = BLOCKS.register(ID_FIRE_STARTER, FireStarterBlock::new);
        FRAME_MOTOR_BLOCK = BLOCKS.register(ID_FRAME_MOTOR, FrameMotorBlock::new);
        FRAME_ACTUATOR_BLOCK = BLOCKS.register(ID_FRAME_ACTUATOR, FrameActuatorBlock::new);
//        BLOCKS.register(ID_INDUCTION_FURNACE, InductionFurnaceBlock::new);
//        BLOCKS.register(ID_TELEPOSER, TeleposerBlock::new);

        // Block Items
        ITEMS.register(ID_PROJECT_BENCH, () -> new BlockItem(PROJECT_BENCH_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_BATTERY_BOX, () -> new BlockItem(BATTERY_BOX_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_AUTO_CRAFTER, () -> new BlockItem(AUTO_CRAFTER_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_CHARGING_BENCH, () -> new BlockItem(CHARGING_BENCH_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FIRE_STARTER, () -> new BlockItem(FIRE_STARTER_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FRAME_MOTOR, () -> new BlockItem(FRAME_MOTOR_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FRAME_ACTUATOR, () -> new BlockItem(FRAME_ACTUATOR_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_INDUCTION_FURNACE, () -> new BlockItem(INDUCTION_FURNACE_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_TELEPOSER, () -> new BlockItem(TELEPOSER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));

        // Tiles
        PROJECT_BENCH_TILE = BLOCK_ENTITY_TYPES.register(ID_PROJECT_BENCH, () -> BlockEntityType.Builder.of(ProjectBenchTile::new, PROJECT_BENCH_BLOCK.get()).build(null));
        BATTERY_BOX_TILE = BLOCK_ENTITY_TYPES.register(ID_BATTERY_BOX, () -> BlockEntityType.Builder.of(BatteryBoxTile::new, BATTERY_BOX_BLOCK.get()).build(null));
        CHARGING_BENCH_TILE = BLOCK_ENTITY_TYPES.register(ID_CHARGING_BENCH, () -> BlockEntityType.Builder.of(ChargingBenchTile::new, CHARGING_BENCH_BLOCK.get()).build(null));
        AUTO_CRAFTER_TILE = BLOCK_ENTITY_TYPES.register(ID_AUTO_CRAFTER, () -> BlockEntityType.Builder.of(AutoCrafterTile::new, AUTO_CRAFTER_BLOCK.get()).build(null));
        FIRE_STARTER_TILE = BLOCK_ENTITY_TYPES.register(ID_FIRE_STARTER, () -> BlockEntityType.Builder.of(FireStarterTile::new, FIRE_STARTER_BLOCK.get()).build(null));
        FRAME_MOTOR_TILE = BLOCK_ENTITY_TYPES.register(ID_FRAME_MOTOR, () -> BlockEntityType.Builder.of(FrameMotorTile::new, FRAME_MOTOR_BLOCK.get()).build(null));
        FRAME_ACTUATOR_TILE = BLOCK_ENTITY_TYPES.register(ID_FRAME_ACTUATOR, () -> BlockEntityType.Builder.of(FrameActuatorTile::new, FRAME_ACTUATOR_BLOCK.get()).build(null));
//        TILE_ENTITIES.register(ID_INDUCTION_FURNACE, () -> TileEntityType.Builder.of(InductionFurnaceTile::new, INDUCTION_FURNACE_BLOCK).build(null));
//        TILE_ENTITIES.register(ID_TELEPOSER, () -> TileEntityType.Builder.of(TeleposerTile::new, TELEPOSER_BLOCK).build(null));

        /* Blocks */
        FRAME_BLOCK = BLOCKS.register(ID_FRAME, FrameBlock::new);

        ITEMS.register(ID_FRAME, () -> new BlockItem(FRAME_BLOCK.get(), new Item.Properties().tab(EXPANSION_GROUP)));
    }

}