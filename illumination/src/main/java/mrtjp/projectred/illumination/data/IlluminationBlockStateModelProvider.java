package mrtjp.projectred.illumination.data;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.ProjectRedIllumination;
import mrtjp.projectred.illumination.block.IllumarLampBlock;
import mrtjp.projectred.illumination.block.IllumarSmartLampBlock;
import net.minecraft.data.PackOutput;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.registries.ForgeRegistries;

import static mrtjp.projectred.illumination.init.IlluminationBlocks.ILLUMAR_SMART_LAMP;

public class IlluminationBlockStateModelProvider extends BlockStateProvider {

    // XY rotations for non-rotatable sided devices
    private static final int[][] DEVICE_SIDED_ROTATIONS = { {0, 0}, {2, 2}, {1, 2}, {1, 0}, {1, 1}, {1, 3} };

    public IlluminationBlockStateModelProvider(PackOutput output, ExistingFileHelper exFileHelper) {
        super(output, ProjectRedIllumination.MOD_ID, exFileHelper);
    }

    @Override
    protected void registerStatesAndModels() {
        for (BlockLightType lampType : BlockLightType.values()) {
            for (int color = 0; color < 16; color++) {
                addIllumarLampVariants(lampType.getBlock(color, true));
                addIllumarLampVariants(lampType.getBlock(color, false));
            }
        }

        addSidedBlockVariants(ILLUMAR_SMART_LAMP.get(),
                createSmartLampModel(ILLUMAR_SMART_LAMP.get(), false),
                createSmartLampModel(ILLUMAR_SMART_LAMP.get(), true));
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

    private void addSidedBlockVariants(Block block, ModelFile offModel, ModelFile onModel) {
        getVariantBuilder(block).forAllStates(state -> {
            int s = state.getValue(ProjectRedBlock.SIDE);
            boolean lit = state.getValue(IllumarSmartLampBlock.LEVEL) > 0;

            return ConfiguredModel.builder()
                    .modelFile(lit ? onModel : offModel)
                    .rotationX(DEVICE_SIDED_ROTATIONS[s][0] * 90)
                    .rotationY(DEVICE_SIDED_ROTATIONS[s][1] * 90)
                    .build();
        });
    }

    private BlockModelBuilder createSmartLampModel(Block block, boolean lit) {
        String texture = ForgeRegistries.BLOCKS.getKey(block).getPath();
        String litKey = lit ? "_on" : "";
        String modelName = texture + litKey;
        return models().cubeBottomTop(modelName,
                modLoc("block/" + texture + "_side" + litKey),
                modLoc("block/" + texture + "_bottom"),
                modLoc("block/" + texture + "_top" + litKey));
    }
}
