package mrtjp.projectred.expansion.data;

import mrtjp.projectred.expansion.TubeType;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.*;
import static mrtjp.projectred.expansion.init.ExpansionUnlocal.*;

public class ExpansionLanguageProvider extends LanguageProvider {

    public ExpansionLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Languages: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Expansion");

        add(PROJECT_BENCH_BLOCK.get(), "Project Bench");
        add(BATTERY_BOX_BLOCK.get(), "Battery Box");
        add(AUTO_CRAFTER_BLOCK.get(), "Auto Crafter");
        add(CHARGING_BENCH_BLOCK.get(), "Charging Bench");
        add(FIRE_STARTER_BLOCK.get(), "Fire Starter");
        add(FRAME_BLOCK.get(), "Frame");
        add(FRAME_MOTOR_BLOCK.get(), "Frame Motor");
        add(FRAME_ACTUATOR_BLOCK.get(), "Frame Actuator");
        add(TRANSPOSER_BLOCK.get(), "Transposer");
        add(BLOCK_BREAKER_BLOCK.get(), "Block Breaker");
        add(DEPLOYER_BLOCK.get(), "Deployer");

        add(TubeType.PNEUMATIC_TUBE.getItem(), "Pneumatic Tube");

        add(RECIPE_PLAN_ITEM.get(), "Recipe Plan");
        add(EMPTY_BATTERY_ITEM.get(), "Empty Battery");
        add(BATTERY_ITEM.get(), "Battery");
        add(ELECTRIC_SCREWDRIVER_ITEM.get(), "Electric Screwdriver");

        // Strings
        add(UL_STORED_POWER_TOOLTIP, "Stored power");
        add(UL_PLAN_RESULT, "Result");

        add(UL_SUBTITLE_PRESSURIZE, "Air pressurizes");
        add(UL_SUBTITLE_DEPRESSURIZE, "Air depressurizes");
    }
}
