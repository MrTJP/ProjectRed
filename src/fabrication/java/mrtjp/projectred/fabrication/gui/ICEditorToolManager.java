package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.util.text.ITextProperties;
import org.lwjgl.glfw.GLFW;

import java.util.ArrayList;
import java.util.List;

public class ICEditorToolManager implements ICRenderNode.IICRenderNodeEventReceiver {

    private final Vector3 initialLeftMousePosition = new Vector3();
    private final Vector3 initialRightMousePosition = new Vector3();

    private boolean leftMouseDown = false;
    private boolean rightMouseDown = false;

    private final ArrayList<IICEditorTool> toolList;
    private IICEditorTool selectedTool;

    private boolean upPressed = false;
    private boolean rightPressed = false;
    private boolean downPressed = false;
    private boolean leftPressed = false;

    public ICEditorToolManager(ArrayList<IICEditorTool> toolList) {
        this.toolList = toolList;
        selectedTool = toolList.get(0);
    }

    public void swapTools(ICEditorToolType nextToolType) {
        IICEditorTool nextTool = toolList.get(nextToolType.ordinal());
        if (nextTool != selectedTool) {
            selectedTool.toolDeactivated();
            selectedTool = nextTool;
            selectedTool.toolActivated();
        }
    }

    public boolean keyPressed(int glfwKeyCode, int glfwFlags) {
        switch (glfwKeyCode) {
            case GLFW.GLFW_KEY_W:
                upPressed = true;
                break;
            case GLFW.GLFW_KEY_A:
                leftPressed = true;
                break;
            case GLFW.GLFW_KEY_S:
                downPressed = true;
                break;
            case GLFW.GLFW_KEY_D:
                rightPressed = true;
                break;
            default:
                return false;
        }
        return true;
    }

    public boolean keyReleased(int glfwKeyCode, int glfwFlags) {
        switch (glfwKeyCode) {
            case GLFW.GLFW_KEY_W:
                upPressed = false;
                break;
            case GLFW.GLFW_KEY_A:
                leftPressed = false;
                break;
            case GLFW.GLFW_KEY_S:
                downPressed = false;
                break;
            case GLFW.GLFW_KEY_D:
                rightPressed = false;
                break;
            default:
                return false;
        }
        return true;
    }

    public void update(ICRenderNode renderNode) {
        Vector3 cameraDelta = new Vector3();
        double deltaPerFrame =  0.1D;

        cameraDelta.z = (upPressed ? -deltaPerFrame : 0) + (downPressed ? deltaPerFrame : 0);
        cameraDelta.x = (leftPressed ? -deltaPerFrame : 0) + (rightPressed ? deltaPerFrame : 0);

        renderNode.applyCameraDelta(cameraDelta);
    }

    @Override
    public void mouseButtonPressed(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton) {
        switch (glfwMouseButton) {
            case GLFW.GLFW_MOUSE_BUTTON_LEFT:
                leftMouseDown = true;
                initialLeftMousePosition.set(mousePosition);
                break;
            case GLFW.GLFW_MOUSE_BUTTON_RIGHT:
                rightMouseDown = true;
                initialRightMousePosition.set(mousePosition);
                break;
            default:
                // ignore
        }

        selectedTool.toolStart(mousePosition, glfwMouseButton);
    }

    @Override
    public void mouseButtonReleased(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton) {
        switch (glfwMouseButton) {
            case GLFW.GLFW_MOUSE_BUTTON_LEFT:
                leftMouseDown = false;
                break;
            case GLFW.GLFW_MOUSE_BUTTON_RIGHT:
                rightMouseDown = false;
                break;
            default:
                // ignore
        }

        selectedTool.toolReleased(mousePosition, glfwMouseButton);
    }

    @Override
    public void mouseButtonDragged(ICRenderNode renderNode, Vector3 mousePosition, Vector3 delta, int glfwMouseButton) {
        if (!selectedTool.toolDragged(mousePosition, delta, glfwMouseButton)) {
            if (leftMouseDown) renderNode.moveCamera(delta.copy().negate());
        }
    }

    @Override
    public void mouseScrolled(ICRenderNode renderNode, Vector3 mousePosition, double scroll) {
        if (!selectedTool.toolScrolled(mousePosition, scroll)) {
            renderNode.moveZoomAt(mousePosition, scroll * 0.3D);
        }
    }

    @Override
    public void layerChanged(ICRenderNode renderNode, int previousLayer, int newLayer) {
        // Update bump the initial position up or down if a drag is in progress
        if (leftMouseDown) initialLeftMousePosition.y = newLayer;
        if (rightMouseDown) initialRightMousePosition.y = newLayer;

        selectedTool.toolLayerChanged(previousLayer, newLayer);
    }

    @Override
    public void onRenderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, IRenderTypeBuffer getter, MatrixStack matrixStack) {
        selectedTool.renderOverlay(mousePosition, isFirstHit, ccrs, getter, matrixStack);
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, List<ITextProperties> tooltip) {
        selectedTool.buildTooltip(mousePosition, isFirstHit, tooltip);
    }
}
