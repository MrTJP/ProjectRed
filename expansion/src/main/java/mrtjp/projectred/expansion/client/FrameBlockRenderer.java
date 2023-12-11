package mrtjp.projectred.expansion.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.block.ICCBlockRenderer;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.render.item.IItemRenderer;
import codechicken.lib.util.TransformUtils;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import mrtjp.projectred.expansion.init.ExpansionBlocks;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.block.model.ItemTransforms;
import net.minecraft.client.resources.model.ModelState;
import net.minecraft.core.BlockPos;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.BlockAndTintGetter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.client.model.data.IModelData;

import java.util.Random;

public class FrameBlockRenderer implements ICCBlockRenderer, IItemRenderer {

    public static final FrameBlockRenderer INSTANCE = new FrameBlockRenderer();

    private FrameBlockRenderer() {
    }

    //region ICCBlockRenderer
    @Override
    public boolean canHandleBlock(BlockAndTintGetter world, BlockPos pos, BlockState blockState) {
        return blockState.getBlock() == ExpansionBlocks.FRAME_BLOCK.get();
    }

    @Override
    public boolean renderBlock(BlockState state, BlockPos pos, BlockAndTintGetter world, PoseStack mStack, VertexConsumer builder, Random random, IModelData data) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.bind(new TransformingVertexConsumer(builder, mStack), DefaultVertexFormat.BLOCK);
        ccrs.lightMatrix.locate(world, pos);
        ccrs.setBrightness(world, pos);

        FrameModelRenderer.renderStatic(ccrs, 0);
        return true;
    }
    //endregion

    //region IItemRenderer
    @Override
    public void renderItem(ItemStack stack, ItemTransforms.TransformType transformType, PoseStack mStack, MultiBufferSource source, int packedLight, int packedOverlay) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(ItemBlockRenderTypes.getRenderType(stack, true), source, mStack);

        FrameModelRenderer.renderStatic(ccrs, 0);
    }

    //@formatter:off
    @Override public boolean useAmbientOcclusion() { return true; }
    @Override public boolean isGui3d() { return true; }
    @Override public boolean usesBlockLight() { return true; }
    @Override public ModelState getModelTransform() { return TransformUtils.DEFAULT_BLOCK; }
    //@formatter:on
    //endregion
}
