package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;

public class ExpansionLootTableProvider extends LootTableProvider.BlockLootProvider {

    public ExpansionLootTableProvider(DataGenerator dataGenerator) {
        super(dataGenerator);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Block Loot Tables";
    }

    @Override
    protected void registerTables() {

        register(PROJECT_BENCH_BLOCK.get(), singleItem(PROJECT_BENCH_BLOCK.get()));
        register(BATTERY_BOX_BLOCK.get(), singleItem(BATTERY_BOX_BLOCK.get()));
        register(AUTO_CRAFTER_BLOCK.get(), singleItem(AUTO_CRAFTER_BLOCK.get()));
        register(CHARGING_BENCH_BLOCK.get(), singleItem(CHARGING_BENCH_BLOCK.get()));
        register(FIRE_STARTER_BLOCK.get(), singleItem(FIRE_STARTER_BLOCK.get()));
        register(FRAME_BLOCK.get(), singleItem(FRAME_BLOCK.get()));
        register(FRAME_MOTOR_BLOCK.get(), singleItem(FRAME_MOTOR_BLOCK.get()));
        register(FRAME_ACTUATOR_BLOCK.get(), singleItem(FRAME_ACTUATOR_BLOCK.get()));
        register(TRANSPOSER_BLOCK.get(), singleItem(TRANSPOSER_BLOCK.get()));
        register(BLOCK_BREAKER_BLOCK.get(), singleItem(BLOCK_BREAKER_BLOCK.get()));
        register(DEPLOYER_BLOCK.get(), singleItem(DEPLOYER_BLOCK.get()));
    }
}
