package mrtjp.projectred.expansion.client;

import codechicken.lib.model.PerspectiveModelState;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.uv.MultiIconTransformation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.client.FullyOrientableBlockRenderer;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;

import javax.annotation.Nullable;
import java.util.Objects;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FrameMotorBlockRenderer extends FullyOrientableBlockRenderer {

    public static final FrameMotorBlockRenderer INSTANCE = new FrameMotorBlockRenderer();

    private static TextureAtlasSprite topIcon;
    private static TextureAtlasSprite frontBack0Icon;
    private static TextureAtlasSprite frontBack1Icon;
    private static TextureAtlasSprite frontBack2Icon;
    private static TextureAtlasSprite leftIcon;
    private static TextureAtlasSprite rightIcon;
    private static TextureAtlasSprite bottomIcon;

    private static @Nullable MultiIconTransformation iconT1;
    private static @Nullable MultiIconTransformation iconT2;
    private static @Nullable MultiIconTransformation iconT3;

    public FrameMotorBlockRenderer() {
    }

    @Override
    public boolean canHandleBlock(BlockAndTintGetter world, BlockPos pos, BlockState blockState, @Nullable RenderType renderType) {
        return blockState.getBlock() == ExpansionBlocks.FRAME_MOTOR_BLOCK.get();
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

        return new RenderData(s, r, Objects.requireNonNull(iconT));
    }

    @Override
    protected RenderData getItemRenderData(ItemStack stack) {

        createIconTransforms();
        return new RenderData(0, 0, Objects.requireNonNull(iconT1));
    }

    private void createIconTransforms() {
        // If resources reloaded, re-create the icon transformations
        if (iconT1 == null || iconT1.icons[0] != bottomIcon) {
            iconT1 = new MultiIconTransformation(bottomIcon, topIcon, frontBack0Icon, frontBack0Icon, leftIcon, rightIcon);
            iconT2 = new MultiIconTransformation(bottomIcon, topIcon, frontBack1Icon, frontBack1Icon, leftIcon, rightIcon);
            iconT3 = new MultiIconTransformation(bottomIcon, topIcon, frontBack2Icon, frontBack2Icon, leftIcon, rightIcon);
        }
    }

    public static  void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;
        topIcon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_top"));
        frontBack0Icon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_0"));
        frontBack1Icon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_1"));
        frontBack2Icon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_front_back_2"));
        leftIcon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_left"));
        rightIcon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_right"));
        bottomIcon = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "block/frame_motor_bottom"));
    }

    public static TextureAtlasSprite getParticleIcon(BlockState state, int side) {
        return switch (side) {
            case 0 -> bottomIcon;
            case 1 -> topIcon;
            case 2, 3 -> frontBack0Icon;
            case 4 -> leftIcon;
            case 5 -> rightIcon;
            default -> throw new IllegalArgumentException("Invalid side: " + side);
        };
    }

    //@formatter:off
    @Override public boolean useAmbientOcclusion() { return true; }
    @Override public boolean isGui3d() { return true; }
    @Override public boolean usesBlockLight() { return true; }
    @Override public @Nullable PerspectiveModelState getModelState() { return TransformUtils.DEFAULT_BLOCK; }
    //@formatter:on
}
