package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicRenderUtils;
import codechicken.lib.render.CCRenderState;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GateDynamicRenderer {

    public final static GateDynamicRenderer instance = new GateDynamicRenderer();
    RotatedRenderer rotatedTess = new RotatedRenderer();
    private GateRenderBridge defaultRendering = new GateRenderBridge.Default();

    public void renderGateWithTESR(GatePart var1, double x, double y, double z) {
        GatePart te = (GatePart) var1;
        if (te.getType() == null) {
            return;
        }
        EnumGate type = te.getGateType();

        rotatedTess.x = x;
        rotatedTess.y = y;
        rotatedTess.z = z;
        rotatedTess.side = te.getSide();
        rotatedTess.front = te.getFront();
        GateRenderBridge rendering = (type == null ? defaultRendering : type.createRenderBridge());
        rendering.set(te.getRenderState());
        
        CCRenderState.reset();
        BasicRenderUtils.bindTerrainResource();
        CCRenderState.useNormals(true);
        CCRenderState.pullLightmap();
        CCRenderState.startDrawing(7);
        for (int i = 0; i < rendering.pointerX.length; i++) {
            float xOffset = rendering.pointerX[i];
            float zOffset = rendering.pointerZ[i];
            renderPointerOnGateWithRotation(rotatedTess, xOffset, 0, zOffset, rendering._pointer, te.pointerPos);
        }
        CCRenderState.draw();
        rendering.renderSpecials(rotatedTess, true);
    }

    /**
     * Render a centered torch model on a gate using the RotatedRenderer.
     * 
     * Offsets are where on the actual gate its rendered. Note that the torch is
     * already offsetted to render at the center.
     */
    public static void renderTorchOnGate(RotatedRenderer rt, float xOffset, float yOffset, float zOffset, boolean on, RotatedPartModel torchon, RotatedPartModel torchoff) {
        xOffset = ((16f - xOffset) / 16f) + .03f;
        yOffset = (yOffset) / 16f;
        zOffset = ((16f - zOffset) / 16f) + .03f;

        if (!on) {
            rt.renderPartModel(torchoff, "torch", xOffset, yOffset, zOffset, -1, -1, false);
            return;
        } else {
            rt.renderPartModel(torchon, "torch", xOffset, yOffset, zOffset, -1, -1, false);
            return;
        }
    }

    /**
     * Render a centered torch glow on a gate using the RotatedRenderer.
     */
    public static void renderGlowOnTorch(RotatedRenderer rt, float xOffset, float yOffset, float zOffset, RotatedPartModel torch) {
        rt.renderPartModel(torch, "glow1", ((16f - xOffset) / 16f) + .03f, (yOffset) / 16f, ((16f - zOffset) / 16f) + .03f, -1, -1, false);
        rt.renderPartModel(torch, "glow2", ((16f - xOffset) / 16f) + .03f, (yOffset) / 16f, ((16f - zOffset) / 16f) + .03f, -1, -1, false);
    }

    /**
     * Render a centered wire model on a gate using the RotatedRenderer.
     */
    public static void renderWireOnGate(RotatedRenderer rt, float[] xOffsets, float[] zOffsets, RotatedPartModel wire, int color) {
        for (int i = 0; i < xOffsets.length; i++) {
            float xOffset = ((16f - xOffsets[i]) / 16f) + .03f;
            float zOffset = ((16f - zOffsets[i]) / 16) + .03f;
            // Render border
            rt.renderPartModel(wire, "border", xOffset, 0, zOffset, -1, -1, true);
            // Render the wire.
            rt.renderPartModel(wire, "wire", xOffset, 0, zOffset, -1, color, true);
        }
    }

    /**
     * Render a centered pointer model with the specified rotation (0 facing
     * north) on a gate using the RotatedRenderer.
     */
    public static void renderPointerOnGateWithRotation(RotatedRenderer rt, float xOffset, float yOffset, float zOffset, RotatedPartModel pointer, float degreesRotation) {
        if (degreesRotation < 0) {
            degreesRotation = 360 + degreesRotation;
        }
        xOffset = ((16f - xOffset) / 16f) + .03f;
        yOffset = (yOffset) / 16f;
        zOffset = ((16f - zOffset) / 16f) + .03f;
        rt.renderPartModel(pointer, "pointer", xOffset, yOffset, zOffset, degreesRotation, -1, true);
    }
}
