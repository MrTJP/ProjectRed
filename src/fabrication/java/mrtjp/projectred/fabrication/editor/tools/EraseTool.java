package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import static mrtjp.projectred.fabrication.editor.tools.IICEditorTool.toNearestPosition;

public class EraseTool extends BaseICEditorTool {

    private final Vector3 initialMouseDown = new Vector3();
    private boolean leftMouseDown;

    @Override
    public ICEditorToolType getToolType() {
        return ICEditorToolType.ERASE_TOOL;
    }

    @Override
    public void readPacket(MCDataInput input) {

        TileCoord start = new TileCoord(input.readByte(), input.readByte(), input.readByte());
        TileCoord end = new TileCoord(input.readByte(), input.readByte(), input.readByte());

        TileCoord min = start.min(end);
        TileCoord max = start.max(end);

        // Delete all tiles in range
        for (int x = min.x; x <= max.x; x++) {
            for (int z = min.z; z <= max.z; z++) {
                TileCoord pos = new TileCoord(x, start.y, z);
                if (editor.getTileMap().getBaseTile(pos).isPresent()) editor.removeTile(pos);
            }
        }
    }

    private void executeTool(Vector3 startMouseDown, Vector3 endMouseDown) {
        MCDataOutput out = editor.getToolStream(this);

        TileCoord start = toNearestPosition(startMouseDown);
        TileCoord end = toNearestPosition(endMouseDown);

        out.writeByte(start.x).writeByte(start.y).writeByte(start.z);
        out.writeByte(end.x).writeByte(end.y).writeByte(end.z);
    }

    @Override
    public boolean toolStart(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            leftMouseDown = true;
            initialMouseDown.set(mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public boolean toolReleased(Vector3 mousePosition, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && leftMouseDown) {
            leftMouseDown = false;
            executeTool(initialMouseDown, mousePosition);
            return true;
        }
        return false;
    }

    @Override
    public void toolLayerChanged(int previousLayer, int newLayer) {
        if (leftMouseDown) {
            initialMouseDown.y = newLayer;
        }
    }

    @Override
    public boolean toolDragged(Vector3 mousePosition, Vector3 delta, int glfwMouseButton) {
        if (glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            return leftMouseDown;
        }
        return false;
    }

    @Override
    public boolean toolScrolled(Vector3 mousePosition, double scroll) {
        return false;
    }

    @Override
    public void toolCanceled(Vector3 mousePosition) {
        leftMouseDown = false;
    }

    @Override
    public void toolActivated() {
        leftMouseDown = false;
    }

    @Override
    public void toolDeactivated() {
        leftMouseDown = false;
    }

    @Override
    @OnlyIn (Dist.CLIENT)
    public void renderOverlay(Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, IRenderTypeBuffer getter, MatrixStack matrixStack) {
        Vector3 a = leftMouseDown ? initialMouseDown : mousePosition;
        Vector3 b = mousePosition;

        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);
        ccrs.baseColour = EnumColour.PINK.rgba(leftMouseDown ? 200 : 32);

        ICRenderTypes.renderSelection(ccrs, a, b, 3 / 16D, 2 / 16D);
    }
}
