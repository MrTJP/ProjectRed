package mrtjp.projectred.core.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.core.init.CoreReferences.ELECTROTINE_GENERATOR_BLOCK;

public class CoreLootTableProvider extends LootTableProvider.BlockLootProvider {

    public CoreLootTableProvider(DataGenerator gen) {
        super(gen);
    }

    @Override
    public String getName() {
        return "ProjectRed-Core Block Loot Tables";
    }

    @Override
    protected void registerTables() {
        register(ELECTROTINE_GENERATOR_BLOCK, singleItem(ELECTROTINE_GENERATOR_BLOCK));
    }
}
