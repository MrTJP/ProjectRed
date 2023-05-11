package mrtjp.projectred.core.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreReferences.*;

public class CoreLanguageProvider extends LanguageProvider {

    public CoreLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Core Languages: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Core");

        // Blocks
        add(ELECTROTINE_GENERATOR_BLOCK, "Electrotine Generator");

        // Items
        add(PLATE_ITEM, "Circuit Plate");
        add(CONDUCTIVE_PLATE_ITEM, "Conductive Plate");
        add(WIRED_PLATE_ITEM, "Wired Plate");
        add(BUNDLED_PLATE_ITEM, "Bundled Plate");
        add(PLATFORMED_PLATE_ITEM, "Platformed Plate");
        add(ANODE_ITEM, "Anode");
        add(CATHODE_ITEM, "Cathode");
        add(POINTER_ITEM, "Pointer");
        add(SILICON_CHIP_ITEM, "Silicon Chip");
        add(ENERGIZED_SILICON_CHIP_ITEM, "Energized Silicon Chip");
        add(COPPER_INGOT_ITEM, "Copper Ingot");
        add(TIN_INGOT_ITEM, "Tin Ingot");
        add(SILVER_INGOT_ITEM, "Silver Ingot");
        add(RED_ALLOY_INGOT_ITEM, "Red Alloy Ingot");
        add(ELECTROTINE_ALLOY_INGOT_ITEM, "Electrotine Alloy Ingot");
        add(ELECTROTINE_DUST_ITEM, "Electrotine Dust");
        add(RUBY_ITEM, "Ruby");
        add(SAPPHIRE_ITEM, "Sapphire");
        add(PERIDOT_ITEM, "Peridot");
        add(SAND_COAL_COMP_ITEM, "Sand Coal Compound");
        add(RED_IRON_COMP_ITEM, "Red Iron Compound");
        add(ELECTROTINE_IRON_COMP_ITEM, "Electrotine Iron Compound");
        add(SILICON_BOULE_ITEM, "Silicon Boule");
        add(SILICON_ITEM, "Silicon");
        add(RED_SILICON_COMP_ITEM, "Red Silicon Compound");
        add(GLOW_SILICON_COMP_ITEM, "Glowing Silicon Compound");
        add(ELECTROTINE_SILICON_COMP_ITEM, "Electrotine Silicon Compound");
        add(INFUSED_SILICON_ITEM, "Infused Silicon");
        add(ENERGIZED_SILICON_ITEM, "Energized Silicon");
        add(ELECTROTINE_SILICON_ITEM, "Electrotine Silicon");
        add(COPPER_COIL_ITEM, "Copper Coil");
        add(IRON_COIL_ITEM, "Iron Coil");
        add(GOLD_COIL_ITEM, "Gold Coil");
        add(MOTOR_ITEM, "Motor");
        add(WOVEN_CLOTH_ITEM, "Woven Cloth");
        add(SAIL_ITEM, "Sail");
        add(WHITE_ILLUMAR_ITEM, "White Illumar");
        add(ORANGE_ILLUMAR_ITEM, "Orange Illumar");
        add(MAGENTA_ILLUMAR_ITEM, "Magenta Illumar");
        add(LIGHT_BLUE_ILLUMAR_ITEM, "Light Blue Illumar");
        add(YELLOW_ILLUMAR_ITEM, "Yellow Illumar");
        add(LIME_ILLUMAR_ITEM, "Lime Illumar");
        add(PINK_ILLUMAR_ITEM, "Pink Illumar");
        add(GRAY_ILLUMAR_ITEM, "Gray Illumar");
        add(LIGHT_GRAY_ILLUMAR_ITEM, "Light Gray Illumar");
        add(CYAN_ILLUMAR_ITEM, "Cyan Illumar");
        add(PURPLE_ILLUMAR_ITEM, "Purple Illumar");
        add(BLUE_ILLUMAR_ITEM, "Blue Illumar");
        add(BROWN_ILLUMAR_ITEM, "Brown Illumar");
        add(GREEN_ILLUMAR_ITEM, "Green Illumar");
        add(RED_ILLUMAR_ITEM, "Red Illumar");
        add(BLACK_ILLUMAR_ITEM, "Black Illumar");
        add(DRAW_PLATE_ITEM, "Draw Plate");
        add(SCREWDRIVER_ITEM, "Screwdriver");
        add(MULTIMETER_ITEM, "Multimeter");
    }
}
