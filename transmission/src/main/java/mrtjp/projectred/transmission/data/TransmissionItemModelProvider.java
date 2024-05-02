package mrtjp.projectred.transmission.data;

import codechicken.lib.datagen.ItemModelProvider;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.data.PackOutput;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.transmission.ProjectRedTransmission.MOD_ID;

public class TransmissionItemModelProvider extends ItemModelProvider {

    public TransmissionItemModelProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerModels() {

        // Wires and framed wires
        for (WireType type : WireType.values()) {
            generated(type.getItem()).noTexture();
        }
    }
}
