package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.ItemModelProvider;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.illumination.ProjectRedIllumination.MOD_ID;

public class IlluminationItemModelProvider extends ItemModelProvider {

    public IlluminationItemModelProvider(DataGenerator generator, ExistingFileHelper existingFileHelper) {
        super(generator, MOD_ID, existingFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Illumination Item Models";
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

        for (MultipartLightType type : MultipartLightType.values()) {
            ModelFile modelFile = getExistingFile(modLoc("item/" + type.getUnlocalBaseName()));
            for (int color = 0; color < 16; color++) {
                getSimple(type.getItem(color, false)).noTexture().parent(modelFile);
                getSimple(type.getItem(color, true)).noTexture().parent(modelFile);
            }
        }
    }
}
