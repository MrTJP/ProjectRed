package mrtjp.projectred.fabrication.init;

import mrtjp.projectred.fabrication.block.FabricationBaseBlock;
import mrtjp.projectred.fabrication.tile.ICWorkbenchTile;
import net.minecraft.item.BlockItem;
import net.minecraft.item.Item;
import net.minecraft.tileentity.TileEntityType;

import static mrtjp.projectred.ProjectRedFabrication.*;
import static mrtjp.projectred.fabrication.init.FabricationReferences.IC_WORKBENCH_BLOCK;

public class FabricationBlocks {

    public static final String ID_IC_WORKBENCH = "ic_workbench";

    public static void register() {

        // Blocks
        BLOCKS.register(ID_IC_WORKBENCH, () -> new FabricationBaseBlock(ICWorkbenchTile::new));

        // Block Items
        ITEMS.register(ID_IC_WORKBENCH, () -> new BlockItem(IC_WORKBENCH_BLOCK, new Item.Properties().tab(FABRICATION_GROUP)));

        // Tiles
        TILE_ENTITIES.register(ID_IC_WORKBENCH, () -> TileEntityType.Builder.of(ICWorkbenchTile::new, IC_WORKBENCH_BLOCK).build(null));
    }
}
