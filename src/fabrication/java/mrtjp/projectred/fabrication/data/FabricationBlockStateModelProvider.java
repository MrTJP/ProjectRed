package mrtjp.projectred.fabrication.data;

import mrtjp.projectred.ProjectRedFabrication;
import mrtjp.projectred.fabrication.block.FabricationBaseBlock;
import net.minecraft.data.DataGenerator;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationReferences.IC_WORKBENCH_BLOCK;

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
                .partialState().with(FabricationBaseBlock.HAS_BLUEPRINT_PROPERTY, false).modelForState().modelFile(noBP).addModel();

        getVariantBuilder(IC_WORKBENCH_BLOCK)
                .partialState().with(FabricationBaseBlock.HAS_BLUEPRINT_PROPERTY, true).modelForState().modelFile(withBP).addModel();

    }
}
