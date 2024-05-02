package mrtjp.projectred.redui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexSorting;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.lib.Size;
import mrtjp.projectred.lib.Vec2;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import org.lwjgl.opengl.GL11;

import java.util.List;

/**
 * Renders a viewport within the node frame. Overrides provided to obtain the camera position fov, etc.
 */
public abstract class ViewportRenderNode extends AbstractGuiNode {

    private final PVMMatrix pvMatrix = new PVMMatrix();

    protected abstract void renderInViewport(PoseStack renderStack, Vec2 ndcMouse, float partialFrame, boolean isFirstHit);

    protected abstract List<Component> getToolTip(Point mousePosition, boolean isFirstHit);

    protected abstract double getTargetPlaneDistance();

    protected abstract double getVerticalFOV();

    protected abstract double getMaxRenderDist();

    protected abstract Vector3 getCameraPosition();

    protected Rect getGlFrame() {
        // Convert frame to screen space anchored at bottom-left instead of top-left
        Rect screenFrame = getRoot().getScreenFrame();
        Rect frame = convertParentRectToScreen(getFrame());

        Rect bottomLeftFrame = new Rect(
                new Point(frame.x(), screenFrame.height() - frame.y() - frame.height()),
                frame.size);

        // Convert from GUI screen space to GL11 screen space using the Minecraft Gui Scale value
        double glWScale = getRoot().getMinecraft().getWindow().getGuiScale();
        double glHScale = getRoot().getMinecraft().getWindow().getGuiScale();

        return new Rect(new Point((int) Math.round(bottomLeftFrame.x() * glWScale), (int) Math.round(bottomLeftFrame.y() * glHScale)),
                new Size((int) Math.round(bottomLeftFrame.width() * glWScale), (int) Math.round(bottomLeftFrame.height() * glHScale)));
    }

    protected Vector3 mouseToWorld(Point mouse) {
        return ndcMouseToWorld(getFrame().ndc(mouse));
    }

    protected Vector3 ndcMouseToWorld(Vec2 ndcMouse) {
        return pvMatrix.ndcToWorldCoordinates(ndcMouse.dx, ndcMouse.dy, getTargetPlaneDistance());
    }

    protected double distanceToEncloseRect(double width, double height) {
        double fovY = getVerticalFOV();
        double fovX = fovY * getFrame().width() / getFrame().height();

        double xDist = width / (2 * Math.tan(fovX / 2));
        double yDist = height / (2 * Math.tan(fovY / 2));

        return Math.max(xDist, yDist);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {

        // Set up projection and view matrices
        Rect glFrame = getGlFrame();
        pvMatrix.setProjection(getVerticalFOV(), glFrame.width(), glFrame.height(), 0.2F, getMaxRenderDist());
        pvMatrix.setView(getCameraPosition().x, getCameraPosition().y, getCameraPosition().z, 90 * MathHelper.torad, 0);

        // Save viewport state
        int glvpH = GlStateManager.Viewport.height();
        int glvpW = GlStateManager.Viewport.width();
        int glvpX = GlStateManager.Viewport.x();
        int glvpY = GlStateManager.Viewport.y();

        // Create a new viewport
        RenderSystem.viewport(glFrame.x(), glFrame.y(), glFrame.width(), glFrame.height());

        // Apply projection matrix
        RenderSystem.backupProjectionMatrix();
        RenderSystem.setProjectionMatrix(pvMatrix.getProjectionMatrix().toMatrix4f(), VertexSorting.ORTHOGRAPHIC_Z);

        PoseStack mvStack = RenderSystem.getModelViewStack();
        mvStack.pushPose();
        mvStack.setIdentity();
        RenderSystem.applyModelViewMatrix();

        /*
          TODO Figure out how to sandwich the viewport at this node's z position, +- some small range.
               For now, this only renders properly if correctly ordered in the tree (i.e. everything
               under it renders before, and everything above it renders after).
         */
        RenderSystem.clear(GL11.GL_DEPTH_BUFFER_BIT, false);

        // Render
        PoseStack pStack = pvMatrix.getModelViewMatrixStack();
        renderInViewport(pStack, getFrame().ndc(mouse), partialFrame, isFirstHit(mouse));

        // Restore previous matrices
        mvStack.popPose();
        RenderSystem.applyModelViewMatrix();

        RenderSystem.restoreProjectionMatrix();

        // Restore previous viewport
        RenderSystem.viewport(glvpX, glvpY, glvpW, glvpH);
    }

    @Override
    public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {

        List<Component> tooltip = getToolTip(mouse, isFirstHit(mouse));

        renderTooltip(graphics, mouse, tooltip);
    }
}
