package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.core.CoreContent.*;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

public class ExplorationLootTableProvider extends LootTableProvider.BlockLootProvider {

    public ExplorationLootTableProvider(DataGenerator dataGenerator) {
        super(dataGenerator);
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Loot Tables";
    }

    @Override
    protected void registerTables() {

        // Ores
        register(RUBY_ORE_BLOCK, valueRangeOrSilkWithFortune(RUBY_ORE_BLOCK, itemRuby().get(), 1, 4));
        register(SAPPHIRE_ORE_BLOCK, valueRangeOrSilkWithFortune(SAPPHIRE_ORE_BLOCK, itemSapphire().get(), 1, 4));
        register(PERIDOT_ORE_BLOCK, valueRangeOrSilkWithFortune(PERIDOT_ORE_BLOCK, itemPeridot().get(), 1, 4));
        register(COPPER_ORE_BLOCK, singleItem(COPPER_ORE_BLOCK));
        register(TIN_ORE_BLOCK, singleItem(TIN_ORE_BLOCK));
        register(SILVER_ORE_BLOCK, singleItem(SILVER_ORE_BLOCK));
        register(ELECTROTINE_ORE_BLOCK, valueRangeOrSilkWithFortune(ELECTROTINE_ORE_BLOCK, itemElectrotineDust().get(), 1, 8));

        // Decorative Blocks
        register(MARBLE_BLOCK, singleItem(MARBLE_BLOCK));
        register(MARBLE_BRICK_BLOCK, singleItem(MARBLE_BRICK_BLOCK));
        register(BASALT_BLOCK, singleItem(BASALT_BLOCK));
        register(BASALT_COBBLE_BLOCK, singleItem(BASALT_COBBLE_BLOCK));
        register(BASALT_BRICK_BLOCK, singleItem(BASALT_BRICK_BLOCK));
        register(RUBY_BLOCK, singleItem(RUBY_BLOCK));
        register(SAPPHIRE_BLOCK, singleItem(SAPPHIRE_BLOCK));
        register(PERIDOT_BLOCK, singleItem(PERIDOT_BLOCK));
        register(COPPER_BLOCK, singleItem(COPPER_BLOCK));
        register(TIN_BLOCK, singleItem(TIN_BLOCK));
        register(SILVER_BLOCK, singleItem(SILVER_BLOCK));
        register(ELECTROTINE_BLOCK, singleItem(ELECTROTINE_BLOCK));

        // Walls
        register(MARBLE_WALL, singleItem(MARBLE_WALL));
        register(MARBLE_BRICK_WALL, singleItem(MARBLE_BRICK_WALL));
        register(BASALT_WALL, singleItem(BASALT_WALL));
        register(BASALT_COBBLE_WALL, singleItem(BASALT_COBBLE_WALL));
        register(BASALT_BRICK_WALL, singleItem(BASALT_BRICK_WALL));
        register(RUBY_BLOCK_WALL, singleItem(RUBY_BLOCK_WALL));
        register(SAPPHIRE_BLOCK_WALL, singleItem(SAPPHIRE_BLOCK_WALL));
        register(PERIDOT_BLOCK_WALL, singleItem(PERIDOT_BLOCK_WALL));
        register(COPPER_BLOCK_WALL, singleItem(COPPER_BLOCK_WALL));
        register(TIN_BLOCK_WALL, singleItem(TIN_BLOCK_WALL));
        register(SILVER_BLOCK_WALL, singleItem(SILVER_BLOCK_WALL));
        register(ELECTROTINE_BLOCK_WALL, singleItem(ELECTROTINE_BLOCK_WALL));
    }
}
