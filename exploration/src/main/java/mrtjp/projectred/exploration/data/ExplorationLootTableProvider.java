package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.core.init.CoreReferences.*;
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
        register(RUBY_ORE_BLOCK, valueRangeOrSilkWithFortune(RUBY_ORE_BLOCK, RUBY_ITEM, 1, 2));
        register(DEEPSLATE_RUBY_ORE_BLOCK, valueRangeOrSilkWithFortune(DEEPSLATE_RUBY_ORE_BLOCK, RUBY_ITEM, 1, 2));
        register(SAPPHIRE_ORE_BLOCK, valueRangeOrSilkWithFortune(SAPPHIRE_ORE_BLOCK, SAPPHIRE_ITEM, 1, 2));
        register(DEEPSLATE_SAPPHIRE_ORE_BLOCK, valueRangeOrSilkWithFortune(DEEPSLATE_SAPPHIRE_ORE_BLOCK, SAPPHIRE_ITEM, 1, 2));
        register(PERIDOT_ORE_BLOCK, valueRangeOrSilkWithFortune(PERIDOT_ORE_BLOCK, PERIDOT_ITEM, 1, 2));
        register(DEEPSLATE_PERIDOT_ORE_BLOCK, valueRangeOrSilkWithFortune(DEEPSLATE_PERIDOT_ORE_BLOCK, PERIDOT_ITEM, 1, 2));
        register(ELECTROTINE_ORE_BLOCK, valueRangeOrSilkWithFortune(ELECTROTINE_ORE_BLOCK, ELECTROTINE_DUST_ITEM, 1, 8));
        register(DEEPSLATE_ELECTROTINE_ORE_BLOCK, valueRangeOrSilkWithFortune(DEEPSLATE_ELECTROTINE_ORE_BLOCK, ELECTROTINE_DUST_ITEM, 1, 8));
        register(TIN_ORE_BLOCK, singleItem(RAW_TIN_ITEM));
        register(DEEPSLATE_TIN_ORE_BLOCK, singleItem(RAW_TIN_ITEM));
        register(SILVER_ORE_BLOCK, singleItem(RAW_SILVER_ITEM));
        register(DEEPSLATE_SILVER_ORE_BLOCK, singleItem(RAW_SILVER_ITEM));

        // Decorative Blocks
        register(MARBLE_BLOCK, singleItem(MARBLE_BLOCK));
        register(MARBLE_BRICK_BLOCK, singleItem(MARBLE_BRICK_BLOCK));
        register(BASALT_BLOCK, singleItem(BASALT_BLOCK));
        register(BASALT_COBBLE_BLOCK, singleItem(BASALT_COBBLE_BLOCK));
        register(BASALT_BRICK_BLOCK, singleItem(BASALT_BRICK_BLOCK));
        register(RUBY_BLOCK, singleItem(RUBY_BLOCK));
        register(SAPPHIRE_BLOCK, singleItem(SAPPHIRE_BLOCK));
        register(PERIDOT_BLOCK, singleItem(PERIDOT_BLOCK));
        register(ELECTROTINE_BLOCK, singleItem(ELECTROTINE_BLOCK));
        register(RAW_TIN_BLOCK, singleItem(RAW_TIN_BLOCK));
        register(RAW_SILVER_BLOCK, singleItem(RAW_SILVER_BLOCK));
        register(TIN_BLOCK, singleItem(TIN_BLOCK));
        register(SILVER_BLOCK, singleItem(SILVER_BLOCK));

        // Walls
        register(MARBLE_WALL, singleItem(MARBLE_WALL));
        register(MARBLE_BRICK_WALL, singleItem(MARBLE_BRICK_WALL));
        register(BASALT_WALL, singleItem(BASALT_WALL));
        register(BASALT_COBBLE_WALL, singleItem(BASALT_COBBLE_WALL));
        register(BASALT_BRICK_WALL, singleItem(BASALT_BRICK_WALL));
        register(RUBY_BLOCK_WALL, singleItem(RUBY_BLOCK_WALL));
        register(SAPPHIRE_BLOCK_WALL, singleItem(SAPPHIRE_BLOCK_WALL));
        register(PERIDOT_BLOCK_WALL, singleItem(PERIDOT_BLOCK_WALL));
        register(ELECTROTINE_BLOCK_WALL, singleItem(ELECTROTINE_BLOCK_WALL));
    }
}
