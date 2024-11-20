package mrtjp.projectred.integration.data;

import codechicken.lib.datagen.ItemModelProvider;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.client.GatePartItemRenderer;
import net.minecraft.data.PackOutput;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class IntegrationItemModelProvider extends ItemModelProvider {

    public IntegrationItemModelProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerModels() {

        for (GateType type : GateType.values()) {
            if (type.isExternalGate()) continue;
            clazz(type.getItem(), GatePartItemRenderer.class);
        }
    }
}
