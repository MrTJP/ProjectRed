package mrtjp.projectred.expansion.init;

import mrtjp.projectred.expansion.block.*;
import mrtjp.projectred.expansion.tile.*;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraft.tileentity.TileEntityType;

import static mrtjp.projectred.expansion.ProjectRedExpansion.*;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

public class ExpansionBlocks {

    public static final String ID_PROJECT_BENCH = "project_bench";
    public static final String ID_BATTERY_BOX = "battery_box";
    public static final String ID_AUTO_CRAFTER = "auto_crafter";
    public static final String ID_CHARGING_BENCH = "charging_bench";
    public static final String ID_FIRE_STARTER = "fire_starter";

//    public static final String ID_INDUCTION_FURNACE = "induction_furnace";
//    public static final String ID_TELEPOSER = "teleposer";

    public static void register() {

        // Blocks
        BLOCKS.register(ID_PROJECT_BENCH, ProjectBenchBlock::new);
        BLOCKS.register(ID_BATTERY_BOX, BatteryBoxBlock::new);
        BLOCKS.register(ID_AUTO_CRAFTER, AutoCrafterBlock::new);
        BLOCKS.register(ID_CHARGING_BENCH, ChargingBenchBlock::new);
        BLOCKS.register(ID_FIRE_STARTER, FireStarterBlock::new);
//        BLOCKS.register(ID_INDUCTION_FURNACE, InductionFurnaceBlock::new);
//        BLOCKS.register(ID_TELEPOSER, TeleposerBlock::new);

        // Block Items
        ITEMS.register(ID_PROJECT_BENCH, () -> new BlockItem(PROJECT_BENCH_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_BATTERY_BOX, () -> new BlockItem(BATTERY_BOX_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_AUTO_CRAFTER, () -> new BlockItem(AUTO_CRAFTER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_CHARGING_BENCH, () -> new BlockItem(CHARGING_BENCH_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
        ITEMS.register(ID_FIRE_STARTER, () -> new BlockItem(FIRE_STARTER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_INDUCTION_FURNACE, () -> new BlockItem(INDUCTION_FURNACE_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));
//        ITEMS.register(ID_TELEPOSER, () -> new BlockItem(TELEPOSER_BLOCK, new Item.Properties().tab(EXPANSION_GROUP)));

        // Tiles
        TILE_ENTITIES.register(ID_PROJECT_BENCH, () -> TileEntityType.Builder.of(ProjectBenchTile::new, PROJECT_BENCH_BLOCK).build(null));
        TILE_ENTITIES.register(ID_BATTERY_BOX, () -> TileEntityType.Builder.of(BatteryBoxTile::new, BATTERY_BOX_BLOCK).build(null));
        TILE_ENTITIES.register(ID_AUTO_CRAFTER, () -> TileEntityType.Builder.of(AutoCrafterTile::new, AUTO_CRAFTER_BLOCK).build(null));
        TILE_ENTITIES.register(ID_CHARGING_BENCH, () -> TileEntityType.Builder.of(ChargingBenchTile::new, CHARGING_BENCH_BLOCK).build(null));
        TILE_ENTITIES.register(ID_FIRE_STARTER, () -> TileEntityType.Builder.of(FireStarterTile::new, FIRE_STARTER_BLOCK).build(null));
//        TILE_ENTITIES.register(ID_INDUCTION_FURNACE, () -> TileEntityType.Builder.of(InductionFurnaceTile::new, INDUCTION_FURNACE_BLOCK).build(null));
//        TILE_ENTITIES.register(ID_TELEPOSER, () -> TileEntityType.Builder.of(TeleposerTile::new, TELEPOSER_BLOCK).build(null));
    }
}
