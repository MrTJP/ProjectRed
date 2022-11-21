package mrtjp.projectred.fabrication.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationReferences.*;

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

        getSimple(IC_WORKBENCH_BLOCK)
                .parent(new ModelFile.UncheckedModelFile(modLoc("block/" + name(IC_WORKBENCH_BLOCK) + "_blueprint")))
                .noTexture();

        domedMachine(PLOTTING_TABLE_BLOCK);
        domedMachine(LITHOGRAPHY_TABLE_BLOCK);
        domedMachine(PACKAGING_TABLE_BLOCK);

        generated(IC_BLUEPRINT_ITEM);
        generated(BLANK_PHOTOMASK_ITEM);
        generated(PHOTOMASK_SET_ITEM);
        generated(ROUGH_SILICON_WAFER_ITEM);
        generated(ETCHED_SILICON_WAFER_ITEM);
        generated(VALID_DIE_ITEM);
        generated(INVALID_DIE_ITEM);

        generated(FABRICATED_GATE_ITEM).noTexture(); // Dummy model to suppress warnings (actually rendered runtime via IItemRenderer)
   }

   private void domedMachine(Block block) {
       getSimple(block)
               .parent(new ModelFile.UncheckedModelFile(modLoc("block/" + name(block) + "_c0")))
               .noTexture();
   }
}
