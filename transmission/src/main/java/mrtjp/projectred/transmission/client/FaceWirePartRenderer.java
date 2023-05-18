package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.transmission.part.BaseFaceWirePart;
import mrtjp.projectred.transmission.part.BundledCablePart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;

import javax.annotation.Nullable;

public class FaceWirePartRenderer implements PartRenderer<BaseFaceWirePart> {

    public static final FaceWirePartRenderer INSTANCE = new FaceWirePartRenderer();

    @Override
    public boolean renderStatic(BaseFaceWirePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.solid() && useStaticRenderer(part))) {
            ccrs.setBrightness(part.level(), part.pos());
            WireModelRenderer.render(ccrs, part);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(BaseFaceWirePart part, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        if (!useStaticRenderer(part)) {
            CCRenderState ccrs = CCRenderState.instance();
            ccrs.reset();
            ccrs.brightness = packedLight;
            ccrs.overlay = packedOverlay;
            ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormat.BLOCK);
            WireModelRenderer.render(ccrs, part);
        }
    }

    private boolean useStaticRenderer(BaseFaceWirePart part) {
        // Force static rendering for bundled cables, as they don't
        // change their appearance based on signal.
        if (part instanceof BundledCablePart) {
            return true;
        }

        return Configurator.staticWires;
    }
}
