package mrtjp.projectred.fabrication.data;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.block.FabricationMachineBlock;
import mrtjp.projectred.fabrication.block.ICWorkbenchBlock;
import net.minecraft.data.PackOutput;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.minecraftforge.registries.ForgeRegistries;

import javax.annotation.Nonnull;

import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;

@SuppressWarnings("DataFlowIssue")
public class FabricationBlockStateModelProvider extends BlockStateProvider {

    public FabricationBlockStateModelProvider(PackOutput output, ExistingFileHelper exFileHelper) {
        super(output, ProjectRedFabrication.MOD_ID, exFileHelper);
    }

    @Override
    public @Nonnull String getName() {
        return "ProjectRed-Fabrication Block Models";
    }

    @Override
    protected void registerStatesAndModels() {

        addICWorkbenchVariants(IC_WORKBENCH_BLOCK.get());

        addFabricationMachineVariants(PLOTTING_TABLE_BLOCK.get());
        addFabricationMachineVariants(LITHOGRAPHY_TABLE_BLOCK.get());
        addFabricationMachineVariants(PACKAGING_TABLE_BLOCK.get());
    }

    private void addICWorkbenchVariants(Block block) {
        getVariantBuilder(block)
                .partialState().with(ICWorkbenchBlock.BLUEPRINT_PROPERTY, false)
                .modelForState().modelFile(createICWorkbenchModel(block, false)).addModel();

        getVariantBuilder(block)
                .partialState().with(ICWorkbenchBlock.BLUEPRINT_PROPERTY, true)
                .modelForState().modelFile(createICWorkbenchModel(block, true)).addModel();
    }

    private void addFabricationMachineVariants(Block block) {
        addRotatableDomedMachineVariants(block,
                createDomedMachineModelFileForBlock(block, 2),
                createDomedMachineModelFileForBlock(block, 1),
                createDomedMachineModelFileForBlock(block, 0));
    }

    private void addRotatableDomedMachineVariants(Block block, ModelFile workingModel, ModelFile chargedModel, ModelFile idleModel) {
        getVariantBuilder(block)
                .forAllStates(state -> {
                    int r = state.getValue(ProjectRedBlock.ROTATION);
                    boolean isWorking = state.getValue(FabricationMachineBlock.WORKING);
                    boolean isCharged = state.getValue(FabricationMachineBlock.CHARGED);

                    ModelFile modelFile = isWorking && isCharged ? workingModel : isCharged ? chargedModel : idleModel;

                    return ConfiguredModel.builder()
                            .modelFile(modelFile)
                            .rotationY(r * 90)
                            .build();
                });
    }

    private BlockModelBuilder createDomedMachineModelFileForBlock(Block block, int chargeState) {
        String textureName = ForgeRegistries.BLOCKS.getKey(block).getPath();
        String modelName = textureName + (chargeState > 0 ? "_state" + chargeState : "");
        return models()
                .withExistingParent(modelName, modLoc("block/domed_machine"))
                .texture("down",  modLoc("block/" + textureName + "_bottom"))
                .texture("up",    modLoc("block/" + textureName + "_top"))
                .texture("north", modLoc("block/" + textureName + "_front_" + chargeState))
                .texture("south", modLoc("block/" + textureName + "_side"))
                .texture("west",  modLoc("block/" + textureName + "_side"))
                .texture("east",  modLoc("block/" + textureName + "_side"));
    }

    private BlockModelBuilder createICWorkbenchModel(Block block, boolean hasBlueprint) {
        String textureName = ForgeRegistries.BLOCKS.getKey(block).getPath();
        String suffix = hasBlueprint ? "" : "_empty";
        String modelName = textureName + suffix;
        return models().cube(modelName,
                modLoc("block/" + textureName + "_bottom"),
                modLoc("block/" + textureName + "_top" + suffix),
                modLoc("block/" + textureName + "_front" + suffix),
                modLoc("block/" + textureName + "_front" + suffix),
                modLoc("block/" + textureName + "_side" + suffix),
                modLoc("block/" + textureName + "_side" + suffix))
                .texture("particle", modLoc("block/" + textureName + "_front" + suffix));
    }
}
