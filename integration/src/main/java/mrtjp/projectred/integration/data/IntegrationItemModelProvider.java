package mrtjp.projectred.integration.data;

import codechicken.lib.datagen.ItemModelProvider;
import mrtjp.projectred.integration.GateType;
import net.minecraft.data.DataGenerator;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import javax.annotation.Nonnull;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class IntegrationItemModelProvider extends ItemModelProvider {

    public IntegrationItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Nonnull
    @Override
    public String getName() {
        return "ProjectRed-Integration Item Models";
    }

    @Override
    protected void registerModels() {

        ModelFile.ExistingModelFile gate = getExistingFile(new ResourceLocation(MOD_ID, "item/gate"));
        for (GateType type : GateType.values()) {
            getSimple(type.getItem()).texture(null).parent(gate);
        }
    }
}
