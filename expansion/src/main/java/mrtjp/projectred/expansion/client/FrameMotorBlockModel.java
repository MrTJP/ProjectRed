package mrtjp.projectred.expansion.client;

import codechicken.lib.vec.uv.MultiIconTransformation;
import mrtjp.projectred.core.block.ProjectRedBlock;
import mrtjp.projectred.core.client.FullyOrientableBlockModel;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;

import javax.annotation.Nullable;
import java.util.Objects;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class FrameMotorBlockModel extends FullyOrientableBlockModel {

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

    @Override
    protected RenderType getBlockRenderLayer(@Nullable BlockState state) {
        return RenderType.solid();
    }

    @Override
    protected RenderData getBlockRenderData(@Nullable BlockState state) {
        if (state == null) {
            return new RenderData(0, 0, Objects.requireNonNull(iconT1));
        }

        int s = state.getValue(ProjectRedBlock.SIDE);
        int r = state.getValue(ProjectRedBlock.ROTATION);

        boolean isWorking = state.getValue(ProjectRedBlock.WORKING);
        boolean isCharged = state.getValue(ProjectRedBlock.CHARGED);
        MultiIconTransformation iconT = isWorking ? iconT3 : isCharged ? iconT2 : iconT1;

        return new RenderData(s, r, Objects.requireNonNull(iconT));
    }

    @Override
    protected BlockState getItemRenderState() {
        return ExpansionBlocks.FRAME_MOTOR_BLOCK.get().defaultBlockState();
    }

    @Override
    public TextureAtlasSprite getParticleIcon() {
        return topIcon;
    }

    public static void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;
        topIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_top"));
        frontBack0Icon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_front_back_0"));
        frontBack1Icon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_front_back_1"));
        frontBack2Icon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_front_back_2"));
        leftIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_left"));
        rightIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_right"));
        bottomIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/frame_motor_bottom"));

        iconT1 = new MultiIconTransformation(bottomIcon, topIcon, frontBack0Icon, frontBack0Icon, leftIcon, rightIcon);
        iconT2 = new MultiIconTransformation(bottomIcon, topIcon, frontBack1Icon, frontBack1Icon, leftIcon, rightIcon);
        iconT3 = new MultiIconTransformation(bottomIcon, topIcon, frontBack2Icon, frontBack2Icon, leftIcon, rightIcon);
    }
}
