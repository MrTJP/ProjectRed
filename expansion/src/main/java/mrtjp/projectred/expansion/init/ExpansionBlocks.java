package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.block.*;
import mrtjp.projectred.expansion.tile.*;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.entity.BlockEntityType;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;

import java.util.function.Supplier;

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
    public static final String ID_TRANSPOSER = "transposer";
    public static final String ID_BLOCK_BREAKER = "block_breaker";
    public static final String ID_DEPLOYER = "deployer";

    // Blocks
    public static Supplier<Block> FRAME_BLOCK;

    public static Supplier<Block> PROJECT_BENCH_BLOCK;
    public static Supplier<Block> BATTERY_BOX_BLOCK;
    public static Supplier<Block> CHARGING_BENCH_BLOCK;
    public static Supplier<Block> AUTO_CRAFTER_BLOCK;
    public static Supplier<Block> FIRE_STARTER_BLOCK;
    public static Supplier<Block> FRAME_MOTOR_BLOCK;
    public static Supplier<Block> FRAME_ACTUATOR_BLOCK;
    public static Supplier<Block> TRANSPOSER_BLOCK;
    public static Supplier<Block> BLOCK_BREAKER_BLOCK;
    public static Supplier<Block> DEPLOYER_BLOCK;

    // Tiles
    public static Supplier<BlockEntityType<ProjectBenchBlockEntity>> PROJECT_BENCH_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<BatteryBoxBlockEntity>> BATTERY_BOX_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<ChargingBenchBlockEntity>> CHARGING_BENCH_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<AutoCrafterBlockEntity>> AUTO_CRAFTER_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<FireStarterBlockEntity>> FIRE_STARTER_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<FrameMotorBlockEntity>> FRAME_MOTOR_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<FrameActuatorBlockEntity>> FRAME_ACTUATOR_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<TransposerBlockEntity>> TRANSPOSER_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<BlockBreakerBlockEntity>> BLOCK_BREAKER_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<DeployerBlockEntity>> DEPLOYER_BLOCK_ENTITY;


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
        TRANSPOSER_BLOCK = BLOCKS.register(ID_TRANSPOSER, TransposerBlock::new);
        BLOCK_BREAKER_BLOCK = BLOCKS.register(ID_BLOCK_BREAKER, BlockBreakerBlock::new);
        DEPLOYER_BLOCK = BLOCKS.register(ID_DEPLOYER, DeployerBlock::new);

        // Block Items
        ITEMS.register(ID_PROJECT_BENCH, () -> new BlockItem(PROJECT_BENCH_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_BATTERY_BOX, () -> new BlockItem(BATTERY_BOX_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_AUTO_CRAFTER, () -> new BlockItem(AUTO_CRAFTER_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_CHARGING_BENCH, () -> new BlockItem(CHARGING_BENCH_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_FIRE_STARTER, () -> new BlockItem(FIRE_STARTER_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_FRAME_MOTOR, () -> new BlockItem(FRAME_MOTOR_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_FRAME_ACTUATOR, () -> new BlockItem(FRAME_ACTUATOR_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_TRANSPOSER, () -> new BlockItem(TRANSPOSER_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_BLOCK_BREAKER, () -> new BlockItem(BLOCK_BREAKER_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_DEPLOYER, () -> new BlockItem(DEPLOYER_BLOCK.get(), new Item.Properties()));

        // Tiles
        PROJECT_BENCH_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_PROJECT_BENCH, () -> BlockEntityType.Builder.of(ProjectBenchBlockEntity::new, PROJECT_BENCH_BLOCK.get()).build(null));
        BATTERY_BOX_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_BATTERY_BOX, () -> BlockEntityType.Builder.of(BatteryBoxBlockEntity::new, BATTERY_BOX_BLOCK.get()).build(null));
        CHARGING_BENCH_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_CHARGING_BENCH, () -> BlockEntityType.Builder.of(ChargingBenchBlockEntity::new, CHARGING_BENCH_BLOCK.get()).build(null));
        AUTO_CRAFTER_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_AUTO_CRAFTER, () -> BlockEntityType.Builder.of(AutoCrafterBlockEntity::new, AUTO_CRAFTER_BLOCK.get()).build(null));
        FIRE_STARTER_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_FIRE_STARTER, () -> BlockEntityType.Builder.of(FireStarterBlockEntity::new, FIRE_STARTER_BLOCK.get()).build(null));
        FRAME_MOTOR_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_FRAME_MOTOR, () -> BlockEntityType.Builder.of(FrameMotorBlockEntity::new, FRAME_MOTOR_BLOCK.get()).build(null));
        FRAME_ACTUATOR_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_FRAME_ACTUATOR, () -> BlockEntityType.Builder.of(FrameActuatorBlockEntity::new, FRAME_ACTUATOR_BLOCK.get()).build(null));
        TRANSPOSER_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_TRANSPOSER, () -> BlockEntityType.Builder.of(TransposerBlockEntity::new, TRANSPOSER_BLOCK.get()).build(null));
        BLOCK_BREAKER_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_BLOCK_BREAKER, () -> BlockEntityType.Builder.of(BlockBreakerBlockEntity::new, BLOCK_BREAKER_BLOCK.get()).build(null));
        DEPLOYER_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_DEPLOYER, () -> BlockEntityType.Builder.of(DeployerBlockEntity::new, DEPLOYER_BLOCK.get()).build(null));

        /* Blocks */
        FRAME_BLOCK = BLOCKS.register(ID_FRAME, FrameBlock::new);

        ITEMS.register(ID_FRAME, () -> new BlockItem(FRAME_BLOCK.get(), new Item.Properties()));
    }

    public static void registerCaps(RegisterCapabilitiesEvent event) {
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, PROJECT_BENCH_BLOCK_ENTITY.get(), (tile, ctx) -> tile.getHandler());
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, BATTERY_BOX_BLOCK_ENTITY.get(), BatteryBoxBlockEntity::getHandler);
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, CHARGING_BENCH_BLOCK_ENTITY.get(), ChargingBenchBlockEntity::getHandler);
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, AUTO_CRAFTER_BLOCK_ENTITY.get(), (tile, ctx) -> tile.getHandler());
        event.registerBlockEntity(Capabilities.ItemHandler.BLOCK, DEPLOYER_BLOCK_ENTITY.get(), (tile, ctx) -> tile.getHandler());
    }
}