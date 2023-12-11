package mrtjp.projectred.core.client;

import codechicken.lib.model.PerspectiveModelState;
import codechicken.lib.render.CCModel;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.block.ICCBlockRenderer;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Vector3;
import codechicken.lib.vec.uv.MultiIconTransformation;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.core.BlockPos;
import net.minecraft.util.RandomSource;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.data.ModelData;
import net.minecraftforge.client.model.generators.ConfiguredModel;

import javax.annotation.Nullable;

/**
 * Needs to exist because Vanilla's default {@link ConfiguredModel} only supports a single x rotation and y rotation,
 * making it very difficult to render blocks that can not only be oriented to all 6 sides, but also rotated about those
 * sides. With only x/y rotations, we would have to use UV-mapping to rotate the textures themselves rather than the model.
 */
public abstract class FullyOrientableBlockRenderer implements ICCBlockRenderer, IItemRenderer {

    private static final CCModel[] models = new CCModel[6 * 4];

    static {
        CCModel baseModel = CCModel.quadModel(24).generateBlock(0, Cuboid6.full);
        for (int s = 0; s < 6; s++) {
            for (int r = 0; r < 4; r++) {
                CCModel m = baseModel.copy().apply(Rotation.sideOrientation(s, r).at(Vector3.CENTER));
                m.computeNormals();
                m.shrinkUVs(0.0005);
                m.computeLightCoords();
                models[modelKey(s, r)] = m;
            }
        }
    }

    private static int modelKey(int side, int rot) {
        return side << 2 | rot & 3;
    }

    //region Implementation abstracts
    protected abstract RenderType getBlockRenderLayer(BlockState state, BlockPos pos, BlockAndTintGetter level);

    protected abstract RenderData getBlockRenderData(BlockState state, BlockPos pos, BlockAndTintGetter level);

    protected abstract RenderData getItemRenderData(ItemStack stack);
    //endregion

    @Override
    public void renderBlock(BlockState state, BlockPos pos, BlockAndTintGetter world, PoseStack mStack, VertexConsumer builder, RandomSource random, ModelData data, @Nullable RenderType renderType) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.bind(new TransformingVertexConsumer(builder, mStack), DefaultVertexFormat.BLOCK);
        ccrs.lightMatrix.locate(world, pos);

        render(ccrs, renderType, getBlockRenderData(state, pos, world));
    }

    @Override
    public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource source, int packedLight, int packedOverlay) {

        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(ItemBlockRenderTypes.getRenderType(stack, true), source, mStack);

        render(ccrs, null, getItemRenderData(stack));
    }

    private void render(CCRenderState ccrs, @Nullable RenderType layer, RenderData data) {

        CCModel model = models[modelKey(data.side, data.rotation)];
        MultiIconTransformation iconT = data.iconT;

        if (layer != null) {
            model.render(ccrs, iconT, ccrs.lightMatrix);
        } else {
            model.render(ccrs, iconT);
        }
    }

    public record RenderData(int side, int rotation, MultiIconTransformation iconT) {
    }

    // Note: Due to an obfuscation bug, these must be manually implemented in the subclass
//    //@formatter:off
//    @Override public boolean useAmbientOcclusion() { return true; }
//    @Override public boolean isGui3d() { return true; }
//    @Override public boolean usesBlockLight() { return true; }
//    @Override public @Nullable PerspectiveModelState getModelState() { return TransformUtils.DEFAULT_BLOCK; }
//    //@formatter:on
}
