package mrtjp.projectred.illumination.data;

import mrtjp.projectred.illumination.ProjectRedIllumination;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.common.data.ExistingFileHelper;

public class IlluminationBlockStateModelProvider extends BlockStateProvider {

    public IlluminationBlockStateModelProvider(DataGenerator gen, ExistingFileHelper exFileHelper) {
        super(gen, ProjectRedIllumination.MOD_ID, exFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Illumination Block Models";
    }

    @Override
    protected void registerStatesAndModels() {
        for (BlockLightType lampType : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                addIllumarLampVariants(lampType.getBlock(color, true));
                addIllumarLampVariants(lampType.getBlock(color, false));
            }
        }
    }

    private void addIllumarLampVariants(Block block) {
        getVariantBuilder(block).forAllStates(state -> ConfiguredModel.builder()
                .modelFile(createLampModel(block, state.getValue(IllumarLampBlock.LIT)))
                .build());
    }

    private BlockModelBuilder createLampModel(Block block, boolean lit) {
        String textureName = BlockLightType.ILLUMAR_LAMP.getRegistryID(((IllumarLampBlock) block).getColor(), false) + (lit ? "_on" : ""); // Always use non-inverted unlocal name for textures
        return models().cubeAll(textureName, modLoc("block/" + textureName));
    }
}
