package mrtjp.projectred.fabrication.data;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.block.FabricationMachineBlock;
import mrtjp.projectred.fabrication.block.ICWorkbenchBlock;
import net.minecraft.data.DataGenerator;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.Block;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationReferences.*;

public class FabricationBlockStateModelProvider extends BlockStateProvider {

    public FabricationBlockStateModelProvider(DataGenerator gen, ExistingFileHelper exFileHelper) {
        super(gen, ProjectRedFabrication.MOD_ID, exFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Fabrication Block Models";
    }

    @Override
    protected void registerStatesAndModels() {

        BlockModelBuilder noBP = models().cube(IC_WORKBENCH_BLOCK.getRegistryName().getPath() + "_no_blueprint",
                new ResourceLocation(MOD_ID, "block/ic_workbench_bottom"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_top"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_front"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_front"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_side"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_side"));

        BlockModelBuilder withBP = models().cube(IC_WORKBENCH_BLOCK.getRegistryName().getPath() + "_blueprint",
                new ResourceLocation(MOD_ID, "block/ic_workbench_bottom"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_top_bp"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_front_bp"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_front_bp"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_side_bp"),
                new ResourceLocation(MOD_ID, "block/ic_workbench_side_bp"));

        getVariantBuilder(IC_WORKBENCH_BLOCK)
                .partialState().with(ICWorkbenchBlock.BLUEPRINT_PROPERTY, false).modelForState().modelFile(noBP).addModel();

        getVariantBuilder(IC_WORKBENCH_BLOCK)
                .partialState().with(ICWorkbenchBlock.BLUEPRINT_PROPERTY, true).modelForState().modelFile(withBP).addModel();

        addFabricationMachineVariants(PLOTTING_TABLE_BLOCK);
        addFabricationMachineVariants(LITHOGRAPHY_TABLE_BLOCK);
        addFabricationMachineVariants(PACKAGING_TABLE_BLOCK);
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
        String textureName = block.getRegistryName().getPath();
        return models()
                .withExistingParent(textureName + "_c" + chargeState, modLoc("block/domed_machine"))
                .texture("down",  modLoc("block/" + textureName + "_bottom"))
                .texture("up",    modLoc("block/" + textureName + "_top"))
                .texture("north", modLoc("block/" + textureName + "_front_" + chargeState))
                .texture("south", modLoc("block/" + textureName + "_side"))
                .texture("west",  modLoc("block/" + textureName + "_side"))
                .texture("east",  modLoc("block/" + textureName + "_side"));
    }
}
