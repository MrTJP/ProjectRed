package mrtjp.projectred.redui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;
import mrtjp.core.vec.Vec2;
import net.minecraft.util.text.ITextProperties;
import org.lwjgl.opengl.GL11;

import java.util.List;

/**
 * Renders a viewport within the node frame. Overrides provided to obtain the camera position fov, etc.
 */
public abstract class ViewportRenderNode extends AbstractGuiNode {

    private final PVMMatrix pvMatrix = new PVMMatrix();

    protected abstract void renderInViewport(MatrixStack renderStack, Vec2 ndcMouse, float partialFrame, boolean isFirstHit);

    protected abstract List<ITextProperties> getToolTip(Point mousePosition, boolean isFirstHit);

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
                frame.size());

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
        return pvMatrix.ndcToWorldCoordinates(ndcMouse.dx(), ndcMouse.dy(), getTargetPlaneDistance());
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {

        // Set up projection and view matrices
        Rect glFrame = getGlFrame();
        pvMatrix.setProjection(getVerticalFOV(), glFrame.width(), glFrame.height(), 0.2F, getMaxRenderDist());
        pvMatrix.setView(getCameraPosition().x, getCameraPosition().y, getCameraPosition().z, 90 * MathHelper.torad, 0);

        // Create a new viewport
        GL11.glPushAttrib(GL11.GL_VIEWPORT_BIT);
        RenderSystem.viewport(glFrame.x(), glFrame.y(), glFrame.width(), glFrame.height());

        // Apply projection matrix
        RenderSystem.matrixMode(GL11.GL_PROJECTION);
        RenderSystem.pushMatrix();
        RenderSystem.loadIdentity();
        RenderSystem.multMatrix(pvMatrix.getProjectionMatrix().toMatrix4f());

        RenderSystem.matrixMode(GL11.GL_MODELVIEW);
        RenderSystem.pushMatrix();
        RenderSystem.loadIdentity();

        // Render
        renderInViewport(pvMatrix.getModelViewMatrixStack(), getFrame().ndc(mouse), partialFrame, isFirstHit(mouse));

        // Restore previous matrices
        RenderSystem.matrixMode(GL11.GL_PROJECTION);
        RenderSystem.popMatrix();

        RenderSystem.matrixMode(GL11.GL_MODELVIEW);

        RenderSystem.popMatrix();

        // Restore previous viewport
        GL11.glPopAttrib();
    }

    @Override
    public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {

        List<ITextProperties> tooltip = getToolTip(mouse, isFirstHit(mouse));

        renderTooltip(stack, mouse, tooltip);
    }
}
