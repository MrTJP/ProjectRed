package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexBuilder;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;

import javax.annotation.Nullable;

public class CenterWirePartRenderer implements PartRenderer<BaseCenterWirePart> {

    public static final CenterWirePartRenderer INSTANCE = new CenterWirePartRenderer();

    @Override
    public boolean renderStatic(BaseCenterWirePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.cutout() && part.useStaticRenderer())) {
            ccrs.setBrightness(part.world(), part.pos());
            FramedWireModelRenderer.render(ccrs, part);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(BaseCenterWirePart part, MatrixStack mStack, IRenderTypeBuffer buffers, int packedLight, int packedOverlay, float partialTicks) {
        if (!part.useStaticRenderer()) {
            CCRenderState ccrs = CCRenderState.instance();
            ccrs.reset();
            ccrs.brightness = packedLight;
            ccrs.overlay = packedOverlay;
            ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormats.BLOCK);
            FramedWireModelRenderer.render(ccrs, part);
        }
    }
}
