package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.PackOutput;

import static mrtjp.projectred.core.init.CoreItems.*;
import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.RAW_SILVER_ITEM;
import static mrtjp.projectred.exploration.init.ExplorationItems.RAW_TIN_ITEM;

public class ExplorationLootTableProvider extends LootTableProvider.BlockLootProvider {

    public ExplorationLootTableProvider(PackOutput output) {
        super(output, MOD_ID);
    }

    @Override
    protected void registerTables() {

        // Ores
        register(RUBY_ORE_BLOCK.get(),                  valueRangeOrSilkWithFortune(RUBY_ORE_BLOCK.get(),                   RUBY_ITEM.get(), 1, 2));
        register(DEEPSLATE_RUBY_ORE_BLOCK.get(),        valueRangeOrSilkWithFortune(DEEPSLATE_RUBY_ORE_BLOCK.get(),         RUBY_ITEM.get(), 1, 2));
        register(SAPPHIRE_ORE_BLOCK.get(),              valueRangeOrSilkWithFortune(SAPPHIRE_ORE_BLOCK.get(),               SAPPHIRE_ITEM.get(), 1, 2));
        register(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get(),    valueRangeOrSilkWithFortune(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get(),     SAPPHIRE_ITEM.get(), 1, 2));
        register(PERIDOT_ORE_BLOCK.get(),               valueRangeOrSilkWithFortune(PERIDOT_ORE_BLOCK.get(),                PERIDOT_ITEM.get(), 1, 2));
        register(DEEPSLATE_PERIDOT_ORE_BLOCK.get(),     valueRangeOrSilkWithFortune(DEEPSLATE_PERIDOT_ORE_BLOCK.get(),      PERIDOT_ITEM.get(), 1, 2));
        register(ELECTROTINE_ORE_BLOCK.get(),           valueRangeOrSilkWithFortune(ELECTROTINE_ORE_BLOCK.get(),            ELECTROTINE_DUST_ITEM.get(), 1, 8));
        register(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get(), valueRangeOrSilkWithFortune(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get(),  ELECTROTINE_DUST_ITEM.get(), 1, 8));
        register(TIN_ORE_BLOCK.get(),                   singleItem(RAW_TIN_ITEM.get()));
        register(DEEPSLATE_TIN_ORE_BLOCK.get(),         singleItem(RAW_TIN_ITEM.get()));
        register(SILVER_ORE_BLOCK.get(),                singleItem(RAW_SILVER_ITEM.get()));
        register(DEEPSLATE_SILVER_ORE_BLOCK.get(),      singleItem(RAW_SILVER_ITEM.get()));

        // Decorative Blocks
        register(MARBLE_BLOCK.get(),         singleItem(MARBLE_BLOCK.get()));
        register(MARBLE_BRICK_BLOCK.get(),   singleItem(MARBLE_BRICK_BLOCK.get()));
        register(BASALT_BLOCK.get(),         singleItem(BASALT_BLOCK.get()));
        register(BASALT_COBBLE_BLOCK.get(),  singleItem(BASALT_COBBLE_BLOCK.get()));
        register(BASALT_BRICK_BLOCK.get(),   singleItem(BASALT_BRICK_BLOCK.get()));
        register(RUBY_BLOCK.get(),           singleItem(RUBY_BLOCK.get()));
        register(SAPPHIRE_BLOCK.get(),       singleItem(SAPPHIRE_BLOCK.get()));
        register(PERIDOT_BLOCK.get(),        singleItem(PERIDOT_BLOCK.get()));
        register(ELECTROTINE_BLOCK.get(),    singleItem(ELECTROTINE_BLOCK.get()));
        register(RAW_TIN_BLOCK.get(),        singleItem(RAW_TIN_BLOCK.get()));
        register(RAW_SILVER_BLOCK.get(),     singleItem(RAW_SILVER_BLOCK.get()));
        register(TIN_BLOCK.get(),            singleItem(TIN_BLOCK.get()));
        register(SILVER_BLOCK.get(),         singleItem(SILVER_BLOCK.get()));

        // Walls
        register(MARBLE_WALL.get(),             singleItem(MARBLE_WALL.get()));
        register(MARBLE_BRICK_WALL.get(),       singleItem(MARBLE_BRICK_WALL.get()));
        register(BASALT_WALL.get(),             singleItem(BASALT_WALL.get()));
        register(BASALT_COBBLE_WALL.get(),      singleItem(BASALT_COBBLE_WALL.get()));
        register(BASALT_BRICK_WALL.get(),       singleItem(BASALT_BRICK_WALL.get()));
        register(RUBY_BLOCK_WALL.get(),         singleItem(RUBY_BLOCK_WALL.get()));
        register(SAPPHIRE_BLOCK_WALL.get(),     singleItem(SAPPHIRE_BLOCK_WALL.get()));
        register(PERIDOT_BLOCK_WALL.get(),      singleItem(PERIDOT_BLOCK_WALL.get()));
        register(ELECTROTINE_BLOCK_WALL.get(),  singleItem(ELECTROTINE_BLOCK_WALL.get()));
    }
}
