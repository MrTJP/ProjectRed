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
import net.minecraftforge.registries.RegistryObject;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.*;

@SuppressWarnings({ "DataFlowIssue", "NotNullFieldNotInitialized" })
public class FabricationBlocks {

    public static final String ID_IC_WORKBENCH = "ic_workbench";
    public static final String ID_PLOTTING_TABLE = "plotting_table";
    public static final String ID_LITHOGRAPHY_TABLE = "lithography_table";
    public static final String ID_PACKAGING_TABLE = "packaging_table";

    // Blocks
    public static RegistryObject<ICWorkbenchBlock> IC_WORKBENCH_BLOCK;
    public static RegistryObject<PlottingTableBlock> PLOTTING_TABLE_BLOCK;
    public static RegistryObject<LithographyTableBlock> LITHOGRAPHY_TABLE_BLOCK;
    public static RegistryObject<PackagingTableBlock> PACKAGING_TABLE_BLOCK;

    // Block Entities
    public static RegistryObject<BlockEntityType<ICWorkbenchTile>> IC_WORKBENCH_TILE;
    public static RegistryObject<BlockEntityType<PlottingTableTile>> PLOTTING_TABLE_TILE;
    public static RegistryObject<BlockEntityType<LithographyTableTile>> LITHOGRAPHY_TABLE_TILE;
    public static RegistryObject<BlockEntityType<PackagingTableTile>> PACKAGING_TABLE_TILE;


    public static void register() {

        // Blocks
        IC_WORKBENCH_BLOCK = BLOCKS.register(ID_IC_WORKBENCH, ICWorkbenchBlock::new);
        PLOTTING_TABLE_BLOCK = BLOCKS.register(ID_PLOTTING_TABLE, PlottingTableBlock::new);
        LITHOGRAPHY_TABLE_BLOCK = BLOCKS.register(ID_LITHOGRAPHY_TABLE, LithographyTableBlock::new);
        PACKAGING_TABLE_BLOCK = BLOCKS.register(ID_PACKAGING_TABLE, PackagingTableBlock::new);

        // Block Items
        ITEMS.register(ID_IC_WORKBENCH, () -> new BlockItem(IC_WORKBENCH_BLOCK.get(), new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_PLOTTING_TABLE, () -> new BlockItem(PLOTTING_TABLE_BLOCK.get(), new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_LITHOGRAPHY_TABLE, () -> new BlockItem(LITHOGRAPHY_TABLE_BLOCK.get(), new Item.Properties().tab(FABRICATION_GROUP)));
        ITEMS.register(ID_PACKAGING_TABLE, () -> new BlockItem(PACKAGING_TABLE_BLOCK.get(), new Item.Properties().tab(FABRICATION_GROUP)));

        // Tiles
        IC_WORKBENCH_TILE = BLOCK_ENTITY_TYPES.register(ID_IC_WORKBENCH, () -> BlockEntityType.Builder.of(ICWorkbenchTile::new, IC_WORKBENCH_BLOCK.get()).build(null));
        PLOTTING_TABLE_TILE = BLOCK_ENTITY_TYPES.register(ID_PLOTTING_TABLE, () -> BlockEntityType.Builder.of(PlottingTableTile::new, PLOTTING_TABLE_BLOCK.get()).build(null));
        LITHOGRAPHY_TABLE_TILE = BLOCK_ENTITY_TYPES.register(ID_LITHOGRAPHY_TABLE, () -> BlockEntityType.Builder.of(LithographyTableTile::new, LITHOGRAPHY_TABLE_BLOCK.get()).build(null));
        PACKAGING_TABLE_TILE = BLOCK_ENTITY_TYPES.register(ID_PACKAGING_TABLE, () -> BlockEntityType.Builder.of(PackagingTableTile::new, PACKAGING_TABLE_BLOCK.get()).build(null));
    }
}
