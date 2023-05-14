package mrtjp.projectred.expansion.data;

import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.expansion.block.BatteryBoxBlock;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.client.model.generators.BlockModelBuilder;
import net.minecraftforge.client.model.generators.BlockStateProvider;
import net.minecraftforge.client.model.generators.ConfiguredModel;
import net.minecraftforge.client.model.generators.ModelFile;
import net.minecraftforge.common.data.ExistingFileHelper;

import static mrtjp.projectred.ProjectRedExpansion.MOD_ID;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

public class ExpansionBlockStateModelProvider extends BlockStateProvider {

    // XY rotations for non-rotatable sided devices
    private static final int[][] DEVICE_SIDED_ROTATIONS = { {0, 0}, {2, 2}, {1, 2}, {1, 0}, {1, 1}, {1, 3} };

    public ExpansionBlockStateModelProvider(DataGenerator gen, ExistingFileHelper exFileHelper) {
        super(gen, MOD_ID, exFileHelper);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Block State Models";
    }

    @Override
    protected void registerStatesAndModels() {
        addRotatableOppositeMatchingFacesBlock(PROJECT_BENCH_BLOCK);
        addBatteryBoxBlock(BATTERY_BOX_BLOCK);
        addTriStateFrontFacedPoweredMachineBlock(AUTO_CRAFTER_BLOCK);
        addBiStateSideAndTopMachineBlock(CHARGING_BENCH_BLOCK);
        addSidedOppositeMatchingFacesDeviceBlock(FIRE_STARTER_BLOCK);
    }

    private void addRotatableOppositeMatchingFacesBlock(Block block) {
        addRotatableVariants(block, createOppositeMatchingFaceModel(block));
    }

    private void addBatteryBoxBlock(Block block) {
        getVariantBuilder(block).forAllStates(state -> {
            int charge = state.getValue(BatteryBoxBlock.CHARGE_LEVEL);
            return ConfiguredModel.builder()
                    .modelFile(createBatteryModel(block, charge))
                    .build();
        });
    }

    private void addTriStateFrontFacedPoweredMachineBlock(Block block) {

        ModelFile m0 = createFrontFacedMachineModel(block, 0);
        ModelFile m1 = createFrontFacedMachineModel(block, 1);
        ModelFile m3 = createFrontFacedMachineModel(block, 2);

        // m2 not possible here (cannot be working and not charged)
        addRotatablePoweredMachineVariants(block, m0, m1, m1, m3);
    }

    private void addBiStateSideAndTopMachineBlock(Block block) {

        ModelFile m0 = createSideAndTopStateModel(block, 0);
        ModelFile m1 = createSideAndTopStateModel(block, 1);

        addChargedMachineVariants(block, m0, m1);
    }

    private void addSidedOppositeMatchingFacesDeviceBlock(Block block) {
        ModelFile inactive = createOppositeMatchingFaceDeviceModel(block, false);
        ModelFile active = createOppositeMatchingFaceDeviceModel(block, true);

        addSidedDeviceVariants(block, inactive, active);
    }

    private void addRotatableVariants(Block block, ModelFile model) {
        for (int r = 0; r < 4; r++) {
            getVariantBuilder(block)
                    .partialState().with(ProjectRedBlock.ROTATION, r)
                    .addModels(ConfiguredModel.builder().modelFile(model).rotationY(r * 90).build());
        }
    }

    private void addRotatablePoweredMachineVariants(Block block, ModelFile idle, ModelFile charged, ModelFile working, ModelFile chargedAndWorking) {
        getVariantBuilder(block).forAllStates(state -> {
            int r = state.getValue(ProjectRedBlock.ROTATION);
            boolean isWorking = state.getValue(ProjectRedBlock.WORKING);
            boolean isCharged = state.getValue(ProjectRedBlock.CHARGED);

            ModelFile model = isWorking && isCharged ? chargedAndWorking
                    : isWorking ? working
                    : isCharged ? charged
                    : idle;

            return ConfiguredModel.builder()
                    .modelFile(model)
                    .rotationY(r * 90)
                    .build();
        });
    }

    private void addChargedMachineVariants(Block block, ModelFile idle, ModelFile charged) {
        getVariantBuilder(block).forAllStates(state -> {
            boolean isCharged = state.getValue(ProjectRedBlock.CHARGED);

            ModelFile model = isCharged ? charged : idle;
            return ConfiguredModel.builder()
                    .modelFile(model)
                    .build();
        });
    }

    private void addSidedDeviceVariants(Block block, ModelFile inactive, ModelFile active) {
        getVariantBuilder(block).forAllStates(state -> {
            int s = state.getValue(ProjectRedBlock.SIDE);
            boolean isActive = state.getValue(ProjectRedBlock.ACTIVE);

            return ConfiguredModel.builder()
                    .modelFile(isActive ? active : inactive)
                    .rotationX(DEVICE_SIDED_ROTATIONS[s][0] * 90)
                    .rotationY(DEVICE_SIDED_ROTATIONS[s][1] * 90)
                    .build();
        });
    }

    private BlockModelBuilder createOppositeMatchingFaceModel(Block block) {
        String texture = block.getRegistryName().getPath();
        return models().cube(texture,
                        modLoc("block/" + texture + "_bottom"),
                        modLoc("block/" + texture + "_top"),
                        modLoc("block/" + texture + "_front_back"),
                        modLoc("block/" + texture + "_front_back"),
                        modLoc("block/" + texture + "_left_right"),
                        modLoc("block/" + texture + "_left_right"))
                .texture("particle", modLoc("block/" + texture + "_front_back"));
    }

    private BlockModelBuilder createBatteryModel(Block block, int charge) {
        String texture = block.getRegistryName().getPath();
        String modelName = texture + (charge > 0 ? "_charge" + charge : "");
        return models().cubeBottomTop(modelName,
                modLoc("block/" + texture + "_side_" + charge),
                modLoc("block/" + texture + "_bottom"),
                modLoc("block/" + texture + "_top"));
    }

    private BlockModelBuilder createFrontFacedMachineModel(Block block, int state) {
        String texture = block.getRegistryName().getPath();
        String modelName = texture + (state > 0 ? "_state" + state : "");
        return models().orientableWithBottom(modelName,
                modLoc("block/" + texture + "_side"),
                modLoc("block/" + texture + "_front_" + state),
                modLoc("block/" + texture + "_bottom"),
                modLoc("block/" + texture + "_top"));
    }

    private BlockModelBuilder createSideAndTopStateModel(Block block, int state) {
        String texture = block.getRegistryName().getPath();
        String modelName = texture + (state > 0 ? "_state" + state : "");
        return models().cubeBottomTop(modelName,
                modLoc("block/" + texture + "_side_" + state),
                modLoc("block/" + texture + "_bottom"),
                modLoc("block/" + texture + "_top_" + state));
    }

    private BlockModelBuilder createOppositeMatchingFaceDeviceModel(Block block, boolean active) {
        String texture = block.getRegistryName().getPath();
        String activeKey = active ? "_active" : "";
        String modelName = texture + activeKey;
        return models().cube(modelName,
                        modLoc("block/" + texture + "_bottom" + activeKey),
                        modLoc("block/" + texture + "_top" + activeKey),
                        modLoc("block/" + texture + "_front_back" + activeKey),
                        modLoc("block/" + texture + "_front_back" + activeKey),
                        modLoc("block/" + texture + "_left_right" + activeKey),
                        modLoc("block/" + texture + "_left_right" + activeKey))
                .texture("particle", modLoc("block/" + texture + "_front_back" + activeKey));
    }
}
