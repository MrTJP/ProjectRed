package mrtjp.projectred.expansion.client;

import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.uv.MultiIconTransformation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.client.FullyOrientableBlockRenderer;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class FrameMotorBlockRenderer extends FullyOrientableBlockRenderer {

    public static final FrameMotorBlockRenderer INSTANCE = new FrameMotorBlockRenderer();

    private static TextureAtlasSprite topIcon;
    private static TextureAtlasSprite frontBack0Icon;
    private static TextureAtlasSprite frontBack1Icon;
    private static TextureAtlasSprite frontBack2Icon;
    private static TextureAtlasSprite leftIcon;
    private static TextureAtlasSprite rightIcon;
    private static TextureAtlasSprite bottomIcon;

    private static MultiIconTransformation iconT1;
    private static MultiIconTransformation iconT2;
    private static MultiIconTransformation iconT3;

    private FrameMotorBlockRenderer() {
    }

    @Override
    public boolean canHandleBlock(BlockAndTintGetter world, BlockPos pos, BlockState blockState) {
        return blockState.getBlock() == ExpansionReferences.FRAME_MOTOR_BLOCK;
    }

    @Override
    protected RenderType getBlockRenderLayer(BlockState state, BlockPos pos, BlockAndTintGetter level) {
        return RenderType.solid();
    }

    @Override
    protected RenderData getBlockRenderData(BlockState state, BlockPos pos, BlockAndTintGetter level) {

        createIconTransforms();

        int s = state.getValue(ProjectRedBlock.SIDE);
        int r = state.getValue(ProjectRedBlock.ROTATION);

        boolean isWorking = state.getValue(ProjectRedBlock.WORKING);
        boolean isCharged = state.getValue(ProjectRedBlock.CHARGED);
        MultiIconTransformation iconT = isWorking ? iconT3 : isCharged ? iconT2 : iconT1;

        return new RenderData(s, r, iconT);
    }

    @Override
    protected RenderData getItemRenderData(ItemStack stack) {

        createIconTransforms();
        return new RenderData(0, 0, iconT1);
    }

    private void createIconTransforms() {
        // If resources reloaded, re-create the icon transformations
        if (iconT1 == null || iconT1.icons[0] != bottomIcon) {
            iconT1 = new MultiIconTransformation(bottomIcon, topIcon, frontBack0Icon, frontBack0Icon, leftIcon, rightIcon);
            iconT2 = new MultiIconTransformation(bottomIcon, topIcon, frontBack1Icon, frontBack1Icon, leftIcon, rightIcon);
            iconT3 = new MultiIconTransformation(bottomIcon, topIcon, frontBack2Icon, frontBack2Icon, leftIcon, rightIcon);
        }
    }

    public static void registerIcons(AtlasRegistrar registrar) {
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_top"), i -> topIcon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_0"), i -> frontBack0Icon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_1"), i -> frontBack1Icon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_2"), i -> frontBack2Icon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_left"), i -> leftIcon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_right"), i -> rightIcon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/frame_motor_bottom"), i -> bottomIcon = i);
    }

    public static TextureAtlasSprite getParticleIcon(BlockState state, int side) {
        return switch (side) {
            case 0 -> bottomIcon;
            case 1 -> topIcon;
            case 2, 3 -> frontBack0Icon;
            case 4 -> leftIcon;
            case 5 -> rightIcon;
            default -> null;
        };
    }

    //@formatter:off
    @Override public boolean useAmbientOcclusion() { return true; }
    @Override public boolean isGui3d() { return true; }
    @Override public boolean usesBlockLight() { return true; }
    @Override public ModelState getModelTransform() { return TransformUtils.DEFAULT_BLOCK; }
    //@formatter:on
}
