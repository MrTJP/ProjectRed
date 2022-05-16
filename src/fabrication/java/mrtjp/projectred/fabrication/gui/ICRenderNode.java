package mrtjp.projectred.fabrication.gui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Vec2;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.engine.BaseTileMap;
import mrtjp.projectred.redui.ViewportRenderNode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderHelper;
import net.minecraft.util.text.ITextProperties;
import org.lwjgl.glfw.GLFW;

import java.util.LinkedList;
import java.util.List;

public class ICRenderNode extends ViewportRenderNode {

    private final ICWorkbenchEditor editor;
    private final IICRenderNodeEventReceiver eventReceiver;

    private final Vector3 cameraPosition = new Vector3();

    private final Vector3 cameraLayerDelta = new Vector3();
    private final Vector3 cameraZoomDelta = new Vector3(0, 1, 0);

    private final Vector3 cameraLayerComponent = new Vector3();
    private final Vector3 cameraZoomComponent = new Vector3(0, 3, 0);

    private int currentLayer = 0;

    private Point lastMousePos = Point.zeroPoint();

    public ICRenderNode(ICWorkbenchEditor editor, IICRenderNodeEventReceiver eventReceiver) {
        this.editor = editor;
        this.eventReceiver = eventReceiver;
    }

    public void setLayer(int layer) {
        int previousLayer = currentLayer;
        this.currentLayer = layer;
        cameraLayerDelta.add(0, layer-previousLayer, 0);
        eventReceiver.layerChanged(this, previousLayer, currentLayer);
    }

    public void moveCamera(Vector3 delta) {
        cameraPosition.add(delta);
    }

    public void moveZoomAt(Vector3 zoomPos, double zoomDelta) {

        Vector3 zoomVec = zoomPos.copy().subtract(cameraPosition).normalize();
        zoomVec.multiply(zoomDelta);
        cameraZoomDelta.add(zoomVec);
    }

    public void applyCameraDelta(Vector3 delta) {
        cameraZoomDelta.add(delta);
    }

    @Override
    public void frameUpdate(Point mouse, float partialFrame) {
        eventReceiver.update(this);

        // Store mouse position for mouse drag calculations
        lastMousePos = mouse;

        // Apply pending camera movements
        Vector3 cameraPositionStep = cameraZoomDelta.copy().multiply(0.25D);
        cameraZoomComponent.add(cameraPositionStep);
        cameraZoomDelta.subtract(cameraPositionStep);
        if (cameraZoomDelta.equalsT(Vector3.ZERO)) {
            cameraZoomDelta.set(0);
        }

        // Apply layer camera movements
        Vector3 cameraLayerStep = cameraLayerDelta.copy().multiply(0.1D);
        cameraLayerComponent.add(cameraLayerStep);
        cameraLayerDelta.subtract(cameraLayerStep);
        if (cameraLayerDelta.equalsT(Vector3.ZERO)) {
            cameraLayerDelta.set(0);
        }

        // Combine components to calculate final position
        cameraPosition.set(0);
        cameraPosition.add(cameraZoomComponent);
        cameraPosition.add(cameraLayerComponent);
    }

    @Override
    protected double getTargetPlaneDistance() {
        return cameraPosition.y - currentLayer - 2/16D;
    }

    @Override
    protected double getVerticalFOV() {
        return 70D * MathHelper.torad;
    }

    @Override
    protected double getMaxRenderDist() {
        return 20D;
    }

    @Override
    protected Vector3 getCameraPosition() {
        return cameraPosition;
    }

    @Override
    protected void renderInViewport(MatrixStack renderStack, Vec2 ndcMouse, float partialFrame, boolean isFirstHit) {

        RenderHelper.setupForFlatItems(); // TODO proper diffuse lighting
        CCRenderState ccrs = CCRenderState.instance();
        IRenderTypeBuffer.Impl getter = Minecraft.getInstance().renderBuffers().bufferSource();

        Vector3 worldPos = ndcMouseToWorld(ndcMouse);

        // At most, render 1 layer below and 1 render above (for transition effects)
        for (int y = currentLayer-1; y <= currentLayer+1; y++) {

            ccrs.reset();
            ccrs.alphaOverride = 255 - (int) (255 * Math.min(Math.abs(y - cameraLayerComponent.y), 1));

            // Render grid
            renderStack.pushPose();
            renderStack.translate(0, y, 0);
            TileCoord minBounds = editor.getTileMap().getMinBounds();
            TileCoord maxBounds = editor.getTileMap().getMaxBounds();
            Cuboid6 bounds = new Cuboid6(minBounds.x, minBounds.y, minBounds.z, maxBounds.x + 1, maxBounds.y + 1, maxBounds.z + 1);

            ICRenderTypes.renderICGrid(renderStack, getter, bounds, ccrs); //TODO the cuboid going in here is kinda awkward

            renderStack.popPose();

            // Render tiles
            for (BaseTileMap.BaseTileEntry entry : editor.getTileMap().getTilesOnLayer(y)) {
                renderStack.pushPose();
                renderStack.translate(entry.getCoord().x, y, entry.getCoord().z);
                ccrs.bind(ICRenderTypes.layersRenderType, getter, renderStack);

                entry.getTile().renderTile(ccrs, new RedundantTransformation(), partialFrame);

                renderStack.popPose();
            }

            if (y == currentLayer) {
                eventReceiver.onRenderOverlay(this, worldPos, isFirstHit, ccrs, getter, renderStack);
            }
        }

        // Force-end the batch to make sure it happens before the custom viewport is altered
        getter.endBatch();
    }

    @Override
    protected List<ITextProperties> getToolTip(Point mousePosition, boolean isFirstHit) {

        List<ITextProperties> tooltip = new LinkedList<>();
        eventReceiver.buildTooltip(this, mouseToWorld(mousePosition), isFirstHit, tooltip);

        return tooltip;
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        if (!consumed) {
            if (glfwKeyCode == GLFW.GLFW_KEY_UP) {
                setLayer(currentLayer + 1);
                return true;
            }

            if (glfwKeyCode == GLFW.GLFW_KEY_DOWN) {
                setLayer(currentLayer - 1);
                return true;
            }
        }
        return false;
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        lastMousePos = p;
        if (!consumed && isFirstHit(p)) {
            eventReceiver.mouseButtonPressed(this, mouseToWorld(p), glfwMouseButton);
            return true;
        }
        return false;
    }

    @Override
    public boolean mouseReleased(Point p, int glfwMouseButton, long timeHeld, boolean consumed) {
        lastMousePos = p;
        eventReceiver.mouseButtonReleased(this, mouseToWorld(p), glfwMouseButton);
        return true;
    }

    @Override
    public boolean mouseDragged(Point p, int glfwMouseButton, long timeHeld, boolean consumed) {
        Vector3 worldPos = mouseToWorld(p);
        Vector3 delta = worldPos.copy().subtract(mouseToWorld(lastMousePos));

        eventReceiver.mouseButtonDragged(this, worldPos, delta, glfwMouseButton);

        lastMousePos = p;
        return true;
    }

    @Override
    public boolean mouseScrolled(Point p, double scroll, boolean consumed) {
        lastMousePos = p;
        if (!consumed && isFirstHit(p)) {
            eventReceiver.mouseScrolled(this, mouseToWorld(p), scroll);
            return true;
        }
        return false;
    }

    public interface IICRenderNodeEventReceiver {
        void update(ICRenderNode renderNode);

        void mouseButtonPressed(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton);

        void mouseButtonReleased(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton);

        void mouseButtonDragged(ICRenderNode renderNode, Vector3 mousePosition, Vector3 delta, int glfwMouseButton);

        void mouseScrolled(ICRenderNode renderNode, Vector3 mousePosition, double scroll);

        void layerChanged(ICRenderNode renderNode, int previousLayer, int newLayer);

        void onRenderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, IRenderTypeBuffer getter, MatrixStack matrixStack);

        void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, List<ITextProperties> tooltip);
    }
}
