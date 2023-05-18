package mrtjp.projectred.illumination.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.core.client.HaloRenderer;
import mrtjp.projectred.illumination.part.MultipartLightPart;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import org.jetbrains.annotations.Nullable;

public class MultipartLightPartRenderer implements PartRenderer<MultipartLightPart> {

    public static final MultipartLightPartRenderer INSTANCE = new MultipartLightPartRenderer();

    private MultipartLightPartRenderer() {
    }

    @Override
    public boolean renderStatic(MultipartLightPart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || layer == RenderType.cutout()) {
            ccrs.setBrightness(part.level(), part.pos());
            part.getProperties().render(part, Vector3.ZERO, ccrs);
            return true;
        }
        return false;
    }

    @Override
    public void renderDynamic(MultipartLightPart part, PoseStack pStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        //        if (isOn)
//            RenderHalo.addLight(pos, getColor, getLightBounds) //TODO RenderWorldLastEvent rendering is broken
        if (part.isLightOn())
            HaloRenderer.renderHalo(CCRenderState.instance(), pStack, buffers, part.getProperties().getGlowBounds(part.getSide()), part.getColor(), Vector3.ZERO);
    }
}
