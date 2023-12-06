package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.block.ICWorkbenchBlock;
import mrtjp.projectred.fabrication.block.LithographyTableBlock;
import mrtjp.projectred.fabrication.block.PackagingTableBlock;
import mrtjp.projectred.fabrication.block.PlottingTableBlock;
import mrtjp.projectred.fabrication.tile.ICWorkbenchTile;
import mrtjp.projectred.fabrication.tile.LithographyTableTile;
import mrtjp.projectred.fabrication.tile.PackagingTableTile;
import mrtjp.projectred.fabrication.tile.PlottingTableTile;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.entity.BlockEntityType;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.*;
import static mrtjp.projectred.fabrication.init.FabricationReferences.*;

@SuppressWarnings("DataFlowIssue")
public class FabricationBlocks {

    public static final String ID_IC_WORKBENCH = "ic_workbench";
    public static final String ID_PLOTTING_TABLE = "plotting_table";
    public static final String ID_LITHOGRAPHY_TABLE = "lithography_table";
    public static final String ID_PACKAGING_TABLE = "packaging_table";

    public static void register() {

        // Blocks
        BLOCKS.register(ID_IC_WORKBENCH, ICWorkbenchBlock::new);
        BLOCKS.register(ID_PLOTTING_TABLE, PlottingTableBlock::new);
        BLOCKS.register(ID_LITHOGRAPHY_TABLE, LithographyTableBlock::new);
        BLOCKS.register(ID_PACKAGING_TABLE, PackagingTableBlock::new);

        // Block Items
        ITEMS.register(ID_IC_WORKBENCH, () -> new BlockItem(IC_WORKBENCH_BLOCK, new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_PLOTTING_TABLE, () -> new BlockItem(PLOTTING_TABLE_BLOCK, new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_LITHOGRAPHY_TABLE, () -> new BlockItem(LITHOGRAPHY_TABLE_BLOCK, new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_PACKAGING_TABLE, () -> new BlockItem(PACKAGING_TABLE_BLOCK, new Item.Properties().tab(FABRICATION_GROUP)));

        // Tiles
        TILE_ENTITIES.register(ID_IC_WORKBENCH, () -> BlockEntityType.Builder.of(ICWorkbenchTile::new, IC_WORKBENCH_BLOCK).build(null));
        TILE_ENTITIES.register(ID_PLOTTING_TABLE, () -> BlockEntityType.Builder.of(PlottingTableTile::new, PLOTTING_TABLE_BLOCK).build(null));
        TILE_ENTITIES.register(ID_LITHOGRAPHY_TABLE, () -> BlockEntityType.Builder.of(LithographyTableTile::new, LITHOGRAPHY_TABLE_BLOCK).build(null));
        TILE_ENTITIES.register(ID_PACKAGING_TABLE, () -> BlockEntityType.Builder.of(PackagingTableTile::new, PACKAGING_TABLE_BLOCK).build(null));
    }
}
