package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.block.ICWorkbenchBlock;
import mrtjp.projectred.fabrication.block.LithographyTableBlock;
import mrtjp.projectred.fabrication.block.PackagingTableBlock;
import mrtjp.projectred.fabrication.block.PlottingTableBlock;
import mrtjp.projectred.fabrication.tile.ICWorkbenchBlockEntity;
import mrtjp.projectred.fabrication.tile.LithographyTableBlockEntity;
import mrtjp.projectred.fabrication.tile.PackagingTableBlockEntity;
import mrtjp.projectred.fabrication.tile.PlottingTableBlockEntity;
import net.minecraft.world.item.BlockItem;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.entity.BlockEntityType;

import java.util.function.Supplier;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class FabricationBlocks {

    public static final String ID_IC_WORKBENCH = "ic_workbench";
    public static final String ID_PLOTTING_TABLE = "plotting_table";
    public static final String ID_LITHOGRAPHY_TABLE = "lithography_table";
    public static final String ID_PACKAGING_TABLE = "packaging_table";

    // Blocks
    public static Supplier<ICWorkbenchBlock> IC_WORKBENCH_BLOCK;
    public static Supplier<PlottingTableBlock> PLOTTING_TABLE_BLOCK;
    public static Supplier<LithographyTableBlock> LITHOGRAPHY_TABLE_BLOCK;
    public static Supplier<PackagingTableBlock> PACKAGING_TABLE_BLOCK;

    // Block Entities
    public static Supplier<BlockEntityType<ICWorkbenchBlockEntity>> IC_WORKBENCH_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<PlottingTableBlockEntity>> PLOTTING_TABLE_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<LithographyTableBlockEntity>> LITHOGRAPHY_TABLE_BLOCK_ENTITY;
    public static Supplier<BlockEntityType<PackagingTableBlockEntity>> PACKAGING_TABLE_BLOCK_ENTITY;

    public static void register() {

        // Blocks
        IC_WORKBENCH_BLOCK = BLOCKS.register(ID_IC_WORKBENCH, ICWorkbenchBlock::new);
        PLOTTING_TABLE_BLOCK = BLOCKS.register(ID_PLOTTING_TABLE, PlottingTableBlock::new);
        LITHOGRAPHY_TABLE_BLOCK = BLOCKS.register(ID_LITHOGRAPHY_TABLE, LithographyTableBlock::new);
        PACKAGING_TABLE_BLOCK = BLOCKS.register(ID_PACKAGING_TABLE, PackagingTableBlock::new);

        // Block Items
        ITEMS.register(ID_IC_WORKBENCH, () -> new BlockItem(IC_WORKBENCH_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_PLOTTING_TABLE, () -> new BlockItem(PLOTTING_TABLE_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_LITHOGRAPHY_TABLE, () -> new BlockItem(LITHOGRAPHY_TABLE_BLOCK.get(), new Item.Properties()));
        ITEMS.register(ID_PACKAGING_TABLE, () -> new BlockItem(PACKAGING_TABLE_BLOCK.get(), new Item.Properties()));

        // Tiles
        IC_WORKBENCH_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_IC_WORKBENCH, () -> BlockEntityType.Builder.of(ICWorkbenchBlockEntity::new, IC_WORKBENCH_BLOCK.get()).build(null));
        PLOTTING_TABLE_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_PLOTTING_TABLE, () -> BlockEntityType.Builder.of(PlottingTableBlockEntity::new, PLOTTING_TABLE_BLOCK.get()).build(null));
        LITHOGRAPHY_TABLE_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_LITHOGRAPHY_TABLE, () -> BlockEntityType.Builder.of(LithographyTableBlockEntity::new, LITHOGRAPHY_TABLE_BLOCK.get()).build(null));
        PACKAGING_TABLE_BLOCK_ENTITY = BLOCK_ENTITY_TYPES.register(ID_PACKAGING_TABLE, () -> BlockEntityType.Builder.of(PackagingTableBlockEntity::new, PACKAGING_TABLE_BLOCK.get()).build(null));
    }
}
