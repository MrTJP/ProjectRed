package mrtjp.projectred.integration.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexBuilder;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.integration.part.GatePart;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;

import javax.annotation.Nullable;

public class GatePartRenderer implements PartRenderer<GatePart> {

    public static final PartRenderer<GatePart> INSTANCE = new GatePartRenderer();

    private GatePartRenderer() {
    }

    @Override
    public boolean renderStatic(GatePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.cutout() && Configurator.staticGates())) {
            ccrs.setBrightness(part.world(), part.pos());
            GateModelRenderer.instance().renderStatic(ccrs, part);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(GatePart part, MatrixStack mStack, IRenderTypeBuffer buffers, int packedLight, int packedOverlay, float partialTicks) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();
        ccrs.brightness = packedLight;
        ccrs.overlay = packedOverlay;
        ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.cutout()), mStack), DefaultVertexFormats.BLOCK);
        GateModelRenderer.instance().renderDynamic(ccrs, part, partialTicks);
        GateModelRenderer.instance().renderCustomDynamic(ccrs, part, mStack, buffers, packedLight, packedOverlay, partialTicks);
    }
}
