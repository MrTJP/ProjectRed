package mrtjp.projectred.core.data;

import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.common.data.LanguageProvider;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;
import static mrtjp.projectred.core.init.CoreItems.*;

public class CoreLanguageProvider extends LanguageProvider {

    public CoreLanguageProvider(PackOutput output) {
        super(output, MOD_ID, "en_us");
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Core");

        // Blocks
        add(ELECTROTINE_GENERATOR_BLOCK.get(), "Electrotine Generator");

        // Items
        add(PLATE_ITEM.get(), "Circuit Plate");
        add(CONDUCTIVE_PLATE_ITEM.get(), "Conductive Plate");
        add(WIRED_PLATE_ITEM.get(), "Wired Plate");
        add(BUNDLED_PLATE_ITEM.get(), "Bundled Plate");
        add(PLATFORMED_PLATE_ITEM.get(), "Platformed Plate");
        add(ANODE_ITEM.get(), "Anode");
        add(CATHODE_ITEM.get(), "Cathode");
        add(POINTER_ITEM.get(), "Pointer");
        add(SILICON_CHIP_ITEM.get(), "Silicon Chip");
        add(ENERGIZED_SILICON_CHIP_ITEM.get(), "Energized Silicon Chip");
        add(RED_ALLOY_INGOT_ITEM.get(), "Red Alloy Ingot");
        add(ELECTROTINE_ALLOY_INGOT_ITEM.get(), "Electrotine Alloy Ingot");
        add(ELECTROTINE_DUST_ITEM.get(), "Electrotine Dust");
        add(RUBY_ITEM.get(), "Ruby");
        add(SAPPHIRE_ITEM.get(), "Sapphire");
        add(PERIDOT_ITEM.get(), "Peridot");
        add(SAND_COAL_COMP_ITEM.get(), "Sand Coal Compound");
        add(RED_IRON_COMP_ITEM.get(), "Red Iron Compound");
        add(ELECTROTINE_IRON_COMP_ITEM.get(), "Electrotine Iron Compound");
        add(SILICON_BOULE_ITEM.get(), "Silicon Boule");
        add(SILICON_ITEM.get(), "Silicon");
        add(RED_SILICON_COMP_ITEM.get(), "Red Silicon Compound");
        add(GLOW_SILICON_COMP_ITEM.get(), "Glowing Silicon Compound");
        add(ELECTROTINE_SILICON_COMP_ITEM.get(), "Electrotine Silicon Compound");
        add(INFUSED_SILICON_ITEM.get(), "Infused Silicon");
        add(ENERGIZED_SILICON_ITEM.get(), "Energized Silicon");
        add(ELECTROTINE_SILICON_ITEM.get(), "Electrotine Silicon");
        add(COPPER_COIL_ITEM.get(), "Copper Coil");
        add(IRON_COIL_ITEM.get(), "Iron Coil");
        add(GOLD_COIL_ITEM.get(), "Gold Coil");
        add(MOTOR_ITEM.get(), "Motor");
        add(WOVEN_CLOTH_ITEM.get(), "Woven Cloth");
        add(SAIL_ITEM.get(), "Sail");
        add(WHITE_ILLUMAR_ITEM.get(), "White Illumar");
        add(ORANGE_ILLUMAR_ITEM.get(), "Orange Illumar");
        add(MAGENTA_ILLUMAR_ITEM.get(), "Magenta Illumar");
        add(LIGHT_BLUE_ILLUMAR_ITEM.get(), "Light Blue Illumar");
        add(YELLOW_ILLUMAR_ITEM.get(), "Yellow Illumar");
        add(LIME_ILLUMAR_ITEM.get(), "Lime Illumar");
        add(PINK_ILLUMAR_ITEM.get(), "Pink Illumar");
        add(GRAY_ILLUMAR_ITEM.get(), "Gray Illumar");
        add(LIGHT_GRAY_ILLUMAR_ITEM.get(), "Light Gray Illumar");
        add(CYAN_ILLUMAR_ITEM.get(), "Cyan Illumar");
        add(PURPLE_ILLUMAR_ITEM.get(), "Purple Illumar");
        add(BLUE_ILLUMAR_ITEM.get(), "Blue Illumar");
        add(BROWN_ILLUMAR_ITEM.get(), "Brown Illumar");
        add(GREEN_ILLUMAR_ITEM.get(), "Green Illumar");
        add(RED_ILLUMAR_ITEM.get(), "Red Illumar");
        add(BLACK_ILLUMAR_ITEM.get(), "Black Illumar");
        add(DRAW_PLATE_ITEM.get(), "Draw Plate");
        add(SCREWDRIVER_ITEM.get(), "Screwdriver");
        add(MULTIMETER_ITEM.get(), "Multimeter");
    }
}
