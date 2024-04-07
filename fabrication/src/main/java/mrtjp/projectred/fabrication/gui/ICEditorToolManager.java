package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import org.lwjgl.glfw.GLFW;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

public class ICEditorToolManager implements ICRenderNode.IICRenderNodeEventReceiver {

    private final Vector3 lastMousePosition = new Vector3();
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
    private boolean layerUpPressed = false;
    private boolean layerDownPressed = false;

    private final List<Consumer<ICEditorToolType>> toolSwappedListeners = new LinkedList<>();

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

            for (Consumer<ICEditorToolType> listener : toolSwappedListeners) {
                listener.accept(nextToolType);
            }
        }
    }

    public void addToolSwappedListener(Consumer<ICEditorToolType> listener) {
        toolSwappedListeners.add(listener);
    }

    public boolean keyPressed(int glfwKeyCode, int glfwFlags) {
        switch (glfwKeyCode) {
            case GLFW.GLFW_KEY_ESCAPE:
                // Try to cancel current tool
                if (selectedTool.toolCanceled()) break;

                // Otherwise, try to swap to the interact tool
                if (selectedTool.getToolType() != ICEditorToolType.INTERACT_TOOL) {
                    swapTools(ICEditorToolType.INTERACT_TOOL);
                    break;
                }
                return false;
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
            case GLFW.GLFW_KEY_UP:
                layerUpPressed = true;
                break;
            case GLFW.GLFW_KEY_DOWN:
                layerDownPressed = true;
                break;
            default:
                return selectedTool.toolKeyPressed(lastMousePosition, glfwKeyCode, glfwFlags);
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
            case GLFW.GLFW_KEY_UP:
                layerUpPressed = false;
                break;
            case GLFW.GLFW_KEY_DOWN:
                layerDownPressed = false;
                break;
            default:
                return selectedTool.toolKeyReleased(lastMousePosition, glfwKeyCode, glfwFlags);
        }

        return true;
    }

    public void update(ICRenderNode renderNode) {
        // Pan camera
        if (upPressed || downPressed || leftPressed || rightPressed) {
            Vector3 panDelta = new Vector3();
            double deltaPerTick = 0.05D;
            panDelta.z = (upPressed ? -deltaPerTick : 0) + (downPressed ? deltaPerTick : 0);
            panDelta.x = (leftPressed ? -deltaPerTick : 0) + (rightPressed ? deltaPerTick : 0);
            renderNode.applyPanningDelta(panDelta);
        }

        // Shift Layers
        if (layerUpPressed) {
            renderNode.setLayer(renderNode.getLayer() + 1);
        } else if (layerDownPressed) {
            renderNode.setLayer(renderNode.getLayer() - 1);
        }
        layerUpPressed = false;
        layerDownPressed = false;
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
            // No default handling of mouse drag events
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
    public void onRenderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        lastMousePosition.set(mousePosition); // Used to pass mouse position to tool key press events
        selectedTool.renderOverlay(mousePosition, isFirstHit, ccrs, getter, matrixStack);
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip) {
        selectedTool.buildTooltip(mousePosition, isFirstHit, tooltip);
    }
}
