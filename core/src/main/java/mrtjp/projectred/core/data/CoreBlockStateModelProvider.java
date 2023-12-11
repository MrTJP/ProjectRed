package mrtjp.projectred.core.data;

import mrtjp.projectred.core.block.ProjectRedBlock;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.registries.ForgeRegistries;

import javax.annotation.Nonnull;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;
import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;

@SuppressWarnings("DataFlowIssue")
public class CoreBlockStateModelProvider extends BlockStateProvider {

    public CoreBlockStateModelProvider(DataGenerator gen, ExistingFileHelper exFileHelper) {
        super(gen, MOD_ID, exFileHelper);
    }

    @Nonnull
    @Override
    public String getName() {
        return "ProjectRed-Core Block State Models";
    }

    @Override
    protected void registerStatesAndModels() {
        quadStateFrontFacedPoweredMachineModel(ELECTROTINE_GENERATOR_BLOCK.get());
    }

    private void quadStateFrontFacedPoweredMachineModel(Block block) {
        addRotatablePoweredMachineVariants(block,
                createFrontFacedPoweredMachineModel(block, 0),
                createFrontFacedPoweredMachineModel(block, 1),
                createFrontFacedPoweredMachineModel(block, 2),
                createFrontFacedPoweredMachineModel(block, 3));
    }

    private void addRotatablePoweredMachineVariants(Block block, ModelFile idleModel, ModelFile chargedModel, ModelFile workingModel, ModelFile chargedWorkingModel) {
        getVariantBuilder(block).forAllStates(state -> {
            int r = state.getValue(ProjectRedBlock.ROTATION);
            boolean isWorking = state.getValue(ProjectRedBlock.WORKING);
            boolean isCharged = state.getValue(ProjectRedBlock.CHARGED);

            ModelFile modelFile = !isWorking && !isCharged ? idleModel :
                    isWorking && !isCharged ? workingModel :
                    !isWorking && isCharged ? chargedModel :
                    chargedWorkingModel;

            return ConfiguredModel.builder()
                    .modelFile(modelFile)
                    .rotationY(r * 90)
                    .build();
        });
    }

    private BlockModelBuilder createFrontFacedPoweredMachineModel(Block block, int state) {
        String texture = ForgeRegistries.BLOCKS.getKey(block).getPath();
        String modelName = texture + (state > 0 ? "_state" + state : "");

        return models().orientableWithBottom(modelName,
                modLoc("block/" + texture + "_side"),
                modLoc("block/" + texture + "_front_" + state),
                modLoc("block/" + texture + "_bottom"),
                modLoc("block/" + texture + "_top"));
    }
}
