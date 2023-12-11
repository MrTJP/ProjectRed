package mrtjp.projectred.core.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;
import static mrtjp.projectred.core.init.CoreItems.*;

public class CoreItemModelProvider extends ItemModelProvider {

    public CoreItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Core Item Models";
    }

    @Override
    protected void registerModels() {
        simpleItemBlock(ELECTROTINE_GENERATOR_BLOCK.get());

        generated(PLATE_ITEM);
        generated(CONDUCTIVE_PLATE_ITEM);
        generated(WIRED_PLATE_ITEM);
        generated(BUNDLED_PLATE_ITEM);
        generated(PLATFORMED_PLATE_ITEM);
        generated(ANODE_ITEM);
        generated(CATHODE_ITEM);
        generated(POINTER_ITEM);
        generated(SILICON_CHIP_ITEM);
        generated(ENERGIZED_SILICON_CHIP_ITEM);
        generated(RED_ALLOY_INGOT_ITEM);
        generated(ELECTROTINE_ALLOY_INGOT_ITEM);
        generated(ELECTROTINE_DUST_ITEM);
        generated(RUBY_ITEM);
        generated(SAPPHIRE_ITEM);
        generated(PERIDOT_ITEM);
        generated(SAND_COAL_COMP_ITEM);
        generated(RED_IRON_COMP_ITEM);
        generated(ELECTROTINE_IRON_COMP_ITEM);
        generated(SILICON_BOULE_ITEM);
        generated(SILICON_ITEM);
        generated(RED_SILICON_COMP_ITEM);
        generated(GLOW_SILICON_COMP_ITEM);
        generated(ELECTROTINE_SILICON_COMP_ITEM);
        generated(INFUSED_SILICON_ITEM);
        generated(ENERGIZED_SILICON_ITEM);
        generated(ELECTROTINE_SILICON_ITEM);
        generated(COPPER_COIL_ITEM);
        generated(IRON_COIL_ITEM);
        generated(GOLD_COIL_ITEM);
        generated(MOTOR_ITEM);
        generated(WOVEN_CLOTH_ITEM);
        generated(SAIL_ITEM);
        generated(WHITE_ILLUMAR_ITEM);
        generated(ORANGE_ILLUMAR_ITEM);
        generated(MAGENTA_ILLUMAR_ITEM);
        generated(LIGHT_BLUE_ILLUMAR_ITEM);
        generated(YELLOW_ILLUMAR_ITEM);
        generated(LIME_ILLUMAR_ITEM);
        generated(PINK_ILLUMAR_ITEM);
        generated(GRAY_ILLUMAR_ITEM);
        generated(LIGHT_GRAY_ILLUMAR_ITEM);
        generated(CYAN_ILLUMAR_ITEM);
        generated(PURPLE_ILLUMAR_ITEM);
        generated(BLUE_ILLUMAR_ITEM);
        generated(BROWN_ILLUMAR_ITEM);
        generated(GREEN_ILLUMAR_ITEM);
        generated(RED_ILLUMAR_ITEM);
        generated(BLACK_ILLUMAR_ITEM);

        generated(DRAW_PLATE_ITEM);
        generated(SCREWDRIVER_ITEM);
        generated(MULTIMETER_ITEM);
    }
}
