package mrtjp.projectred.fabrication.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.PackOutput;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

public class FabricationLootTableProvider extends LootTableProvider.BlockLootProvider {

    public FabricationLootTableProvider(PackOutput output) {
        super(output, MOD_ID);
    }

    @Override
    protected void registerTables() {
        register(IC_WORKBENCH_BLOCK.get(), singleItem(IC_WORKBENCH_BLOCK.get()));
        register(PLOTTING_TABLE_BLOCK.get(), singleItem(PLOTTING_TABLE_BLOCK.get()));
        register(LITHOGRAPHY_TABLE_BLOCK.get(), singleItem(LITHOGRAPHY_TABLE_BLOCK.get()));
        register(PACKAGING_TABLE_BLOCK.get(), singleItem(PACKAGING_TABLE_BLOCK.get()));
    }
}
