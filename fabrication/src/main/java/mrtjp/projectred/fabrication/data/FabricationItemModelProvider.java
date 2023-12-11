package mrtjp.projectred.fabrication.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;
import static mrtjp.projectred.fabrication.init.FabricationItems.*;
import static mrtjp.projectred.fabrication.init.FabricationParts.FABRICATED_GATE_ITEM;

public class FabricationItemModelProvider extends ItemModelProvider {

    public FabricationItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Fabrication Item Models";
    }

    @Override
    protected void registerModels() {

        simpleItemBlock(IC_WORKBENCH_BLOCK.get());

        simpleItemBlock(PLOTTING_TABLE_BLOCK.get());
        simpleItemBlock(LITHOGRAPHY_TABLE_BLOCK.get());
        simpleItemBlock(PACKAGING_TABLE_BLOCK.get());

        generated(IC_BLUEPRINT_ITEM);
        generated(BLANK_PHOTOMASK_ITEM);
        generated(PHOTOMASK_SET_ITEM);
        generated(ROUGH_SILICON_WAFER_ITEM);
        generated(ETCHED_SILICON_WAFER_ITEM);
        generated(VALID_DIE_ITEM);
        generated(INVALID_DIE_ITEM);

        generated(FABRICATED_GATE_ITEM).noTexture(); // Dummy model to suppress warnings (actually rendered runtime via IItemRenderer)
   }
}
