package mrtjp.projectred.integration.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;

import javax.annotation.Nullable;

public class GatePartRenderer implements PartRenderer<GatePart> {

    public static final PartRenderer<GatePart> INSTANCE = new GatePartRenderer();

    private GatePartRenderer() {
    }

    @Override
    public void renderStatic(GatePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.cutout() && Configurator.staticGates)) {
            ccrs.setBrightness(part.level(), part.pos());
            GateModelRenderer.instance().renderStatic(ccrs, part, RedundantTransformation.INSTANCE);
        }
    }

    @Override
    public void renderDynamic(GatePart part, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.cutout()), mStack), DefaultVertexFormat.BLOCK);
        GateModelRenderer.instance().renderDynamic(ccrs, part, RedundantTransformation.INSTANCE, mStack, buffers, packedLight, packedOverlay, partialTicks);
    }
}
