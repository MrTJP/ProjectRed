package mrtjp.projectred.core.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.PackOutput;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;

public class CoreLootTableProvider extends LootTableProvider.BlockLootProvider {

    public CoreLootTableProvider(PackOutput output) {
        super(output, MOD_ID);
    }

    @Override
    protected void registerTables() {
        register(ELECTROTINE_GENERATOR_BLOCK.get(), singleItem(ELECTROTINE_GENERATOR_BLOCK.get()));
    }
}
