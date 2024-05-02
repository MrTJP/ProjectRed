package mrtjp.projectred.expansion.client;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.api.part.render.PartRenderer;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.expansion.part.BaseTubePart;
import mrtjp.projectred.expansion.part.PneumaticTubePayload;
import mrtjp.projectred.expansion.pneumatics.PneumaticTransportContainer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.Direction;
import net.minecraft.world.item.ItemDisplayContext;

import javax.annotation.Nullable;

public class TubePartRenderer implements PartRenderer<BaseTubePart> {

    public static final TubePartRenderer INSTANCE = new TubePartRenderer();

    private static final Vector3[] SIDE_OFFSETS = new Vector3[] {
            new Vector3(0.5, 0.0, 0.5), // Down
            new Vector3(0.5, 1.0, 0.5), // Up
            new Vector3(0.5, 0.5, 0.0), // North
            new Vector3(0.5, 0.5, 1.0), // South
            new Vector3(0.0, 0.5, 0.5), // West
            new Vector3(1.0, 0.5, 0.5)  // East
    };

    @Override
    public void renderStatic(BaseTubePart part, @Nullable RenderType layer, CCRenderState ccrs) {
        if (layer == null || (layer == RenderType.cutout() && part.useStaticRenderer())) {
            ccrs.setBrightness(part.level(), part.pos());
            TubeModelRenderer.render(ccrs, part);
        }
    }

    @Override
    public void renderDynamic(BaseTubePart part, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {
        if (!part.useStaticRenderer()) {
            CCRenderState ccrs = CCRenderState.instance();
            ccrs.reset();
            ccrs.brightness = packedLight;
            ccrs.overlay = packedOverlay;
            ccrs.bind(new TransformingVertexConsumer(buffers.getBuffer(RenderType.solid()), mStack), DefaultVertexFormat.BLOCK);
            TubeModelRenderer.render(ccrs, part);
        }

        renderPayloadTransport(part, mStack, buffers, packedLight, packedOverlay, partialTicks);
    }

    private void renderPayloadTransport(BaseTubePart part, PoseStack mStack, MultiBufferSource buffers, int packedLight, int packedOverlay, float partialTicks) {

        if (!(part instanceof PneumaticTransportContainer ptc)) return;


        for (var p : ptc.getPneumaticTransport().getPayloads()) {
            boolean isHalfWay = p.getProgress() >= (PneumaticTubePayload.MAX_PROGRESS / 2);
            float pf = (float) p.getProgress() / PneumaticTubePayload.MAX_PROGRESS;
            float sf = (float) p.getSpeed() / PneumaticTubePayload.MAX_PROGRESS;
            float progress = pf + sf * partialTicks;

            int d = isHalfWay && p.getOutputSide() != -1 ? p.getOutputSide() : p.getInputSide() ^ 1;
            int dx = Direction.values()[d].getStepX();
            int dy = Direction.values()[d].getStepY();
            int dz = Direction.values()[d].getStepZ();

            Vector3 pos = SIDE_OFFSETS[d ^ 1].copy();
            Vector3 dir = new Vector3(dx, dy, dz);

            pos.add(dir.multiply(progress));

            mStack.pushPose();
            mStack.translate(pos.x, pos.y, pos.z);
            mStack.scale(0.5f, 0.5f, 0.5f);

            var itemRenderer = Minecraft.getInstance().getItemRenderer();
            var model = itemRenderer.getModel(p.getItemStack(), part.level(), null, 0); //TODO last int is seed
            itemRenderer.render(p.getItemStack(), ItemDisplayContext.FIXED, false, mStack, buffers, packedLight, packedOverlay, model);

            mStack.popPose();
        }
    }

}
