package mrtjp.projectred.transmission.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.transmission.part.BaseCenterWirePart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;

import javax.annotation.Nullable;

public class CenterWirePartRenderer implements PartRenderer<BaseCenterWirePart> {

    public static final CenterWirePartRenderer INSTANCE = new CenterWirePartRenderer();

    @Override
    public boolean renderStatic(BaseCenterWirePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.cutout() && part.useStaticRenderer())) {
            ccrs.setBrightness(part.level(), part.pos());
            FramedWireModelRenderer.render(ccrs, part);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(BaseCenterWirePart part, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        if (!part.useStaticRenderer()) {
            CCRenderState ccrs = CCRenderState.instance();
            ccrs.reset();
            ccrs.brightness = packedLight;
            ccrs.overlay = packedOverlay;
            ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormat.BLOCK);
            FramedWireModelRenderer.render(ccrs, part);
        }
    }
}
