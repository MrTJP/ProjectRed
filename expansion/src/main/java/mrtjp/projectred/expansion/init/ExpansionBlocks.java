package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.block.*;
import mrtjp.projectred.expansion.tile.*;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.entity.BlockEntityType;

import static mrtjp.projectred.expansion.ProjectRedExpansion.*;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

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

    public static void register() {

        /* Entity Blocks */

        // Blocks
        BLOCKS.register(ID_PROJECT_BENCH, ProjectBenchBlock::new);
        BLOCKS.register(ID_BATTERY_BOX, BatteryBoxBlock::new);
        BLOCKS.register(ID_AUTO_CRAFTER, AutoCrafterBlock::new);
        BLOCKS.register(ID_CHARGING_BENCH, ChargingBenchBlock::new);
        BLOCKS.register(ID_FIRE_STARTER, FireStarterBlock::new);
        BLOCKS.register(ID_FRAME_MOTOR, FrameMotorBlock::new);
        BLOCKS.register(ID_FRAME_ACTUATOR, FrameActuatorBlock::new);
//        BLOCKS.register(ID_INDUCTION_FURNACE, InductionFurnaceBlock::new);
//        BLOCKS.register(ID_TELEPOSER, TeleposerBlock::new);

        // Block Items
        ITEMS.register(ID_PROJECT_BENCH, () -> new BlockItem(PROJECT_BENCH_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_BATTERY_BOX, () -> new BlockItem(BATTERY_BOX_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_AUTO_CRAFTER, () -> new BlockItem(AUTO_CRAFTER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_CHARGING_BENCH, () -> new BlockItem(CHARGING_BENCH_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FIRE_STARTER, () -> new BlockItem(FIRE_STARTER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FRAME_MOTOR, () -> new BlockItem(FRAME_MOTOR_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FRAME_ACTUATOR, () -> new BlockItem(FRAME_ACTUATOR_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_INDUCTION_FURNACE, () -> new BlockItem(INDUCTION_FURNACE_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_TELEPOSER, () -> new BlockItem(TELEPOSER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));

        // Tiles
        BLOCK_ENTITIES.register(ID_PROJECT_BENCH, () -> BlockEntityType.Builder.of(ProjectBenchTile::new, PROJECT_BENCH_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_BATTERY_BOX, () -> BlockEntityType.Builder.of(BatteryBoxTile::new, BATTERY_BOX_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_AUTO_CRAFTER, () -> BlockEntityType.Builder.of(AutoCrafterTile::new, AUTO_CRAFTER_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_CHARGING_BENCH, () -> BlockEntityType.Builder.of(ChargingBenchTile::new, CHARGING_BENCH_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_FIRE_STARTER, () -> BlockEntityType.Builder.of(FireStarterTile::new, FIRE_STARTER_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_FRAME_MOTOR, () -> BlockEntityType.Builder.of(FrameMotorTile::new, FRAME_MOTOR_BLOCK).build(null));
        BLOCK_ENTITIES.register(ID_FRAME_ACTUATOR, () -> BlockEntityType.Builder.of(FrameActuatorTile::new, FRAME_ACTUATOR_BLOCK).build(null));
//        TILE_ENTITIES.register(ID_INDUCTION_FURNACE, () -> TileEntityType.Builder.of(InductionFurnaceTile::new, INDUCTION_FURNACE_BLOCK).build(null));
//        TILE_ENTITIES.register(ID_TELEPOSER, () -> TileEntityType.Builder.of(TeleposerTile::new, TELEPOSER_BLOCK).build(null));

        /* Blocks */
        BLOCKS.register(ID_FRAME, FrameBlock::new);

        ITEMS.register(ID_FRAME, () -> new BlockItem(FRAME_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
    }

}