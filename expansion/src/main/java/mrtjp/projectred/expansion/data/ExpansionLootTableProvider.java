package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.LootTableProvider;
import net.minecraft.data.DataGenerator;

import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

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

        register(PROJECT_BENCH_BLOCK, singleItem(PROJECT_BENCH_BLOCK));
        register(BATTERY_BOX_BLOCK, singleItem(BATTERY_BOX_BLOCK));
        register(AUTO_CRAFTER_BLOCK, singleItem(AUTO_CRAFTER_BLOCK));
        register(CHARGING_BENCH_BLOCK, singleItem(CHARGING_BENCH_BLOCK));
        register(FIRE_STARTER_BLOCK, singleItem(FIRE_STARTER_BLOCK));
        register(FRAME_BLOCK, singleItem(FRAME_BLOCK));
        register(FRAME_MOTOR_BLOCK, singleItem(FRAME_MOTOR_BLOCK));
        register(FRAME_ACTUATOR_BLOCK, singleItem(FRAME_ACTUATOR_BLOCK));
    }
}
