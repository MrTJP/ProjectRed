package mrtjp.projectred.fabrication.data;

import codechicken.lib.datagen.ItemModelProvider;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.ProjectRedFabrication.MOD_ID;
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
                .texture(null);

        generated(IC_BLUEPRINT_ITEM);

        generated(FABRICATED_GATE_ITEM).texture(null); // Dummy model to suppress warnings (actually rendered runtime via IItemRenderer)
   }
}
