package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.ItemModelProvider;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.client.model.generators.ModelFile;
import net.neoforged.neoforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;
import static mrtjp.projectred.illumination.init.IlluminationBlocks.ILLUMAR_SMART_LAMP;

public class IlluminationItemModelProvider extends ItemModelProvider {

    public IlluminationItemModelProvider(PackOutput output, ExistingFileHelper existingFileHelper) {
        super(output, MOD_ID, existingFileHelper);
    }

    @Override
    protected void registerModels() {

        for (BlockLightType type : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                ModelFile offModel = new ModelFile.UncheckedModelFile(modLoc("block/" + type.getRegistryID(color, false)));
                ModelFile onModel = new ModelFile.UncheckedModelFile(modLoc("block/" + type.getRegistryID(color, false) + "_on"));

                generated(type.getBlock(color, false)).noTexture().parent(offModel);
                generated(type.getBlock(color, true)).noTexture().parent(onModel);
            }
        }

        simpleItemBlock(ILLUMAR_SMART_LAMP.get());

        for (MultipartLightType type : MultipartLightType.values()) {
            for (int color = 0; color < 16; color++) {
                clazz(type.getItem(color, false), type.getProperties().getItemRendererClass());
                clazz(type.getItem(color, true), type.getProperties().getItemRendererClass());
            }
        }
    }
}
