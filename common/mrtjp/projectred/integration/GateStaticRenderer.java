package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicRenderUtils;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.item.ItemStack;
import net.minecraftforge.client.IItemRenderer;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GateStaticRenderer implements IItemRenderer {

    private GateRenderBridge defaultRendering = new GateRenderBridge.Default();
    private RotatedRenderer rotatedRenderer = new RotatedRenderer();
    public static final GateStaticRenderer instance = new GateStaticRenderer();

    public boolean renderWorldBlock(GatePart te, int x, int y, int z) {
        int side = te.getSide();
        int front = te.getFront();
        EnumGate type = te.getGateType();
        if (type == null) {
            return true;
        }

        GateRenderBridge rendering = type.createRenderBridge();
        rendering.set(te.getRenderState());
        rotatedRenderer.x = x;
        rotatedRenderer.y = y;
        rotatedRenderer.z = z;
        rotatedRenderer.side = side;
        rotatedRenderer.front = front;

        BasicRenderUtils.bindTerrainResource();
        CCRenderState.reset();
        CCRenderState.setColour(0);
        Tessellator.instance.setColorRGBA(255, 255, 255, 255);
        CCRenderState.setBrightness(te.world(), te.x(), te.y(), te.z());
        rotatedRenderer.renderPartModel(rendering._modelBase, "base", .5f, 0, .5f, -1, -1, false);
        for (int i = 0; i < rendering.wireColor.length; i++) {
            float[] xPositions = rendering.wirePosX[i];
            float[] zPositions = rendering.wirePosZ[i];
            int color = rendering.wireColor[i];
            GateDynamicRenderer.renderWireOnGate(rotatedRenderer, xPositions, zPositions, rendering._wire, color);
        }
        for (int i = 0; i < rendering.torchState.length; i++) {
            GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering.torchState[i], rendering._torchOn, rendering._torchOff);
        }
        for (int i = 0; i < rendering.pointerX.length; i++) {
            float xOffset = rendering.pointerX[i];
            float zOffset = rendering.pointerZ[i];
            GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], true, rendering._torchOn, rendering._torchOff);
        }
        for (int i = 0; i < rendering.torchState.length; i++) {
            if (rendering.torchState[i]) {
                GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering._torchOn);
            }
        }
        for (int i = 0; i < rendering.pointerX.length; i++) {
            GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._torchOn);
        }
        rendering.renderSpecials(rotatedRenderer, false);
        return true;
    }

    public void renderGateInInventory(int meta, float x, float y, float z, float scale) {
        EnumGate type = EnumGate.VALID_GATES[meta];
        GateRenderBridge rendering = (type == null ? defaultRendering : type.createRenderBridge());
        rendering.setItemRender();
        rotatedRenderer.x = x;
        rotatedRenderer.y = y;
        rotatedRenderer.z = z;
        rotatedRenderer.front = 2;
        rotatedRenderer.side = 0;

        GL11.glPushMatrix();
        GL11.glScalef(scale, scale, scale);
        CCRenderState.reset();
        CCRenderState.useNormals(true);
        CCRenderState.startDrawing(7);
        rotatedRenderer.renderPartModel(rendering._modelBase, "base", .5f, 0, .5f, -1, -1, false);
        for (int i = 0; i < rendering.wireColor.length; i++) {
            float[] xPositions = rendering.wirePosX[i];
            float[] zPositions = rendering.wirePosZ[i];
            int color = rendering.wireColor[i];
            GateDynamicRenderer.renderWireOnGate(rotatedRenderer, xPositions, zPositions, rendering._wire, color);
        }
        for (int i = 0; i < rendering.torchState.length; i++) {
            GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering.torchState[i], rendering._torchOn, rendering._torchOff);
        }
        for (int i = 0; i < rendering.pointerX.length; i++) {
            GateDynamicRenderer.renderPointerOnGateWithRotation(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._pointer, 0);
            GateDynamicRenderer.renderTorchOnGate(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], true, rendering._torchOn, rendering._torchOff);
        }
        for (int i = 0; i < rendering.torchState.length; i++) {
            if (rendering.torchState[i]) {
                GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.torchX[i], rendering.torchY[i], rendering.torchZ[i], rendering._torchOn);
            }
        }
        for (int i = 0; i < rendering.pointerX.length; i++) {
            GateDynamicRenderer.renderGlowOnTorch(rotatedRenderer, rendering.pointerX[i], 0, rendering.pointerZ[i], rendering._torchOn);
        }
        rendering.renderSpecials(rotatedRenderer, false);
        CCRenderState.draw();
        GL11.glPopMatrix();
    }

    @Override
    public boolean handleRenderType(ItemStack item, ItemRenderType type) {
        return true;
    }

    @Override
    public boolean shouldUseRenderHelper(ItemRenderType type, ItemStack item, ItemRendererHelper helper) {
        return true;
    }

    @Override
    public void renderItem(ItemRenderType type, ItemStack item, Object... data) {
        int damage = item.getItemDamage();
        switch (type) {
        case ENTITY:
            renderGateInInventory(damage, -.5f, 0f, -.5f, .6f);
            return;
        case EQUIPPED:
            renderGateInInventory(damage, 0f, .15f, 0f, 1f);
            return;
        case EQUIPPED_FIRST_PERSON:
            renderGateInInventory(damage, 1f, -.2f, -.4f, 2f);
            return;
        case INVENTORY:
            renderGateInInventory(damage, 0f, .15f, 0f, 1f);
            return;
        default: return;
        }

    }

}
