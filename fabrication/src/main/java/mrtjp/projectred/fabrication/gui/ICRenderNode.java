package mrtjp.projectred.fabrication.gui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.RedundantTransformation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Vec2;
import mrtjp.projectred.redui.ViewportRenderNode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.LightTexture;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class ICRenderNode extends ViewportRenderNode {

    private static final int ZOOM_ANIMATION_TIME_MS = 100;
    private static final int LAYER_ANIMATION_TIME_MS = 200;
    private static final int CAMERA_ANIMATION_TIME_MS = 100;
    private static final int FOCUS_ANIMATION_TIME_MS = 400;
    private static final int ZOOM_DIST_MAX = 18;
    private static final int ZOOM_DIST_MIN = 2;

    private final ICWorkbenchEditor editor;
    private final IICRenderNodeEventReceiver eventReceiver;

    private final Vector3 cameraPosition = new Vector3();

    private final LinearVectorAnimation cameraLayerAnimator = new LinearVectorAnimation();
    private final LinearVectorAnimation cameraZoomAnimator = new LinearVectorAnimation(0, 8, 0);

    private int currentLayer = 0;

    private Point lastMousePos = Point.ZERO;

    public ICRenderNode(ICWorkbenchEditor editor, IICRenderNodeEventReceiver eventReceiver) {
        this.editor = editor;
        this.eventReceiver = eventReceiver;
    }

    public void setLayer(int layer) {
        int previousLayer = currentLayer;
        this.currentLayer = layer;
        cameraLayerAnimator.addDeltaWithNewDuration(new Vector3(0, layer-previousLayer, 0), LAYER_ANIMATION_TIME_MS);
        eventReceiver.layerChanged(this, previousLayer, currentLayer);
    }

    public int getLayer() {
        return currentLayer;
    }

    public void moveZoomAt(Vector3 zoomPos, double zoomDelta) {

        Vector3 zoomVec = zoomPos.copy().subtract(cameraPosition).normalize();
        zoomVec.multiply(zoomDelta);
        cameraZoomAnimator.addDeltaWithNewDuration(zoomVec, ZOOM_ANIMATION_TIME_MS);
    }

    public void applyCameraDelta(Vector3 delta) {
        cameraZoomAnimator.addDeltaWithNewDuration(delta, CAMERA_ANIMATION_TIME_MS);
    }

    /**
     * Apply a camera delta based on NDC delta
     * @param delta Amount to pan camera in frame NDC units
     */
    public void applyPanningDelta(Vector3 delta) {

        // Calculate scale based on how many tiles are visible in the viewport across its largest dimension
        double scale;
        if (getFrame().width() > getFrame().height()) {
            scale = ndcMouseToWorld(new Vec2(1, 0)).subtract(ndcMouseToWorld(new Vec2(-1, 0))).x;
        } else {
            scale = ndcMouseToWorld(new Vec2(0, -1)).subtract(ndcMouseToWorld(new Vec2(0, 1))).z; // Z because top-down view
        }

        // Scale delta to world units
        Vector3 scaledDelta = delta.copy().multiply(scale);

        // Move camera
        applyCameraDelta(scaledDelta);
    }

    public void focusCameraAtTiles(List<TileCoord> positions) {

        if (positions.isEmpty()) return;

        // Create cuboid enclosing all tiles
        Cuboid6 bounds = new Cuboid6();

        Iterator<TileCoord> it = positions.iterator();
        TileCoord first = it.next();
        bounds.set(first.x, first.y, first.z, first.x+1, first.y+1, first.z+1);
        while (it.hasNext()) {
            TileCoord next = it.next();
            bounds.enclose(next.x, next.y, next.z, next.x+1, next.y+1, next.z+1);
        }
        bounds.expand(1.0); // Expand by 1 tile in all directions

        // X and Z will be at midpoints
        double midX = (bounds.min.x + bounds.max.x) / 2D;
        double midZ = (bounds.min.z + bounds.max.z) / 2D;

        // Y will be far enough away to enclose all tiles inside FOV
        double dist = distanceToEncloseRect(bounds.max.x - bounds.min.x, bounds.max.z - bounds.min.z);

        // Set camera position
        cameraZoomAnimator.moveToTargetWithDuration(new Vector3(midX, dist, midZ), FOCUS_ANIMATION_TIME_MS);
    }

    @Override
    public void frameUpdate(Point mouse, float partialFrame) {

        // Set camera location bounds
        TileCoord minBounds = editor.getTileMap().getMinBounds();
        TileCoord maxBounds = editor.getTileMap().getMaxBounds();
        cameraZoomAnimator.setBounds(
                minBounds.x,
                cameraLayerAnimator.vector.y + ZOOM_DIST_MIN, // Y bounds follows zoom
                minBounds.z,
                maxBounds.x + 1,
                cameraLayerAnimator.vector.y + ZOOM_DIST_MAX,
                maxBounds.z + 1);

        long t = System.currentTimeMillis();
        cameraZoomAnimator.tick(t);
        cameraLayerAnimator.tick(t);

        // Store mouse position for mouse drag calculations
        lastMousePos = mouse;

        // Combine components to calculate final position
        cameraPosition.set(0);
        cameraZoomAnimator.apply(cameraPosition);
        cameraLayerAnimator.apply(cameraPosition);
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
    protected void renderInViewport(PoseStack renderStack, Vec2 ndcMouse, float partialFrame, boolean isFirstHit) {

        CCRenderState ccrs = CCRenderState.instance();
        MultiBufferSource.BufferSource getter = Minecraft.getInstance().renderBuffers().bufferSource();

        Vector3 worldPos = ndcMouseToWorld(ndcMouse);

        // At most, render 1 layer below and 1 render above (for transition effects)
        for (int y = currentLayer-1; y <= currentLayer+1; y++) {

            ccrs.reset();
            // Linearly ramp down alpha as we move away from the current layer
            ccrs.alphaOverride = 255 - (int) (255 * Math.min(Math.abs(y - cameraLayerAnimator.vector.y), 1));
            ccrs.brightness = LightTexture.pack(15, 15);

            // Render grid
            renderStack.pushPose();
            renderStack.translate(0, y, 0);
            TileCoord minBounds = editor.getTileMap().getMinBounds();
            TileCoord maxBounds = editor.getTileMap().getMaxBounds();
            Cuboid6 bounds = new Cuboid6(minBounds.x, minBounds.y, minBounds.z, maxBounds.x + 1, maxBounds.y + 1, maxBounds.z + 1);

            ICRenderTypes.renderICGrid(renderStack, getter, bounds, ccrs); //TODO the cuboid going in here is kinda awkward

            renderStack.popPose();

            // Render tiles
            for (var entry : editor.getTileMap().getTilesOnLayer(y)) {
                renderStack.pushPose();
                renderStack.translate(entry.getKey().x, y, entry.getKey().z);
                ccrs.bind(ICRenderTypes.layersRenderType, getter, renderStack);

                //TODO [1.21.1] Is this valid? Minecraft.getInstance().getPartialTick() -> partialFrame? Why was partialFrame not used here before?
                entry.getValue().renderTile(ccrs, RedundantTransformation.INSTANCE, partialFrame);

                renderStack.popPose();
            }

            if (y == currentLayer) {
                eventReceiver.onRenderOverlay(this, worldPos, isFirstHit, ccrs, getter, renderStack);
            }
        }

        // Edit mode grid effect
        if (!editor.getStateMachine().isSimulating()) {
            TileCoord minBounds = editor.getTileMap().getMinBounds();
            TileCoord maxBounds = editor.getTileMap().getMaxBounds();
            double girdHeight = 2;
            Cuboid6 bounds = new Cuboid6(minBounds.x, currentLayer - girdHeight - 1, minBounds.z, maxBounds.x + 1, currentLayer + girdHeight, maxBounds.z + 1).expand(0.01);
            ICRenderTypes.renderGridInBounds(renderStack, getter, bounds, 0x7);
        }

        // Force-end the batch to make sure it happens before the custom viewport is altered
        getter.endBatch();
    }

    @Override
    public void onAddedToParent() {
        // Set initial zoom to enclose entire tile map
        focusCameraAtTiles(List.of(editor.getTileMap().getMinBounds(), editor.getTileMap().getMaxBounds()));
    }

    @Override
    public void update() {
        super.update();
        eventReceiver.update(this);
    }

    @Override
    protected List<Component> getToolTip(Point mousePosition, boolean isFirstHit) {

        List<Component> tooltip = new LinkedList<>();
        eventReceiver.buildTooltip(this, mouseToWorld(mousePosition), isFirstHit, tooltip);

        return tooltip;
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
    public boolean mouseScrolled(Point p, double scrollX, double scrollY, boolean consumed) {
        lastMousePos = p;
        if (!consumed && isFirstHit(p)) {
            eventReceiver.mouseScrolled(this, mouseToWorld(p), scrollY);
            return true;
        }
        return false;
    }

    public interface IICRenderNodeEventReceiver {

        default void update(ICRenderNode renderNode) { }

        default void mouseButtonPressed(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton) { }

        default void mouseButtonReleased(ICRenderNode renderNode, Vector3 mousePosition, int glfwMouseButton) { }

        default void mouseButtonDragged(ICRenderNode renderNode, Vector3 mousePosition, Vector3 delta, int glfwMouseButton) { }

        default void mouseScrolled(ICRenderNode renderNode, Vector3 mousePosition, double scroll) { }

        default void layerChanged(ICRenderNode renderNode, int previousLayer, int newLayer) { }

        void onRenderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack);

        void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip);
    }
}
