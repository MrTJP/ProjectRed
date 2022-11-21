package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;
import net.minecraft.util.text.StringTextComponent;

import java.util.Collections;

public abstract class ScrollBarNode extends AbstractGuiNode {

    private final ScrollAxis axis;

    private Point lastDragPosition = Point.zeroPoint();
    private boolean isDraggingSlider = false;
    private Rect sliderFrame = new Rect(0, 0, 0, 0);

    private double scrollPercentage = 0.0;

    public ScrollBarNode(ScrollAxis axis) {
        this.axis = axis;
    }

    public void setSliderSize(int w, int h) {
        sliderFrame = new Rect(sliderFrame.origin(), new Size(w, h));
        setScrollPercentage(0);
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {

        // Draw scroll bar background
        drawSlider(stack, sliderFrame);
    }

    @Override
    public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {
        if (!isFirstHit(mouse)) return;

        renderTooltip(stack, mouse, Collections.singletonList(new StringTextComponent("Scroll (" + scrollPercentage + ")")));
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (isFirstHit(p)) {
            if (sliderFrame.contains(p)) {
                isDraggingSlider = true;
                lastDragPosition = p;
                return true;
            }

            //TODO jump to position if clicked on bar
            return true;
        }
        return false;
    }

    @Override
    public boolean mouseReleased(Point p, int glfwMouseButton, long timeHeld, boolean consumed) {
        if (isDraggingSlider) {
            isDraggingSlider = false;
            return true;
        }
        return false;
    }

    @Override
    public boolean mouseDragged(Point p, int glfwMouseButton, long timeHeld, boolean consumed) {

        if (!isDraggingSlider)
            return false;

        Point delta = p.subtract(lastDragPosition).multiply(axis.vec);
        lastDragPosition = p;

        Rect targetSliderFrame = new Rect(sliderFrame.origin().add(delta), sliderFrame.size());
        sliderFrame = getFrame().trap(targetSliderFrame);
        recalcScrollPercentage();

        return true;
    }

    private void recalcScrollPercentage() {

        if (axis == ScrollAxis.VERTICAL) {
            int minY = getFrame().y();
            int maxY = getFrame().maxY() - sliderFrame.height();
            int sliderY = sliderFrame.y();

            scrollPercentage = (sliderY - minY) / (double) (maxY - minY);
        } else {
            int minX = getFrame().x();
            int maxX = getFrame().maxX() - sliderFrame.width();
            int sliderX = sliderFrame.x();

            scrollPercentage = (sliderX - minX) / (double) (maxX - minX);
        }

        adjustContent(scrollPercentage);
    }

    public double getScrollPercentage() {
        return scrollPercentage;
    }

    public void setScrollPercentage(double percentage) {
        scrollPercentage = Math.min(1.0, Math.max(0.0, percentage));

        int sliderX = getFrame().x();
        int sliderY = getFrame().y();

        if (axis == ScrollAxis.VERTICAL) {
            int minY = getFrame().y();
            int maxY = getFrame().maxY() - sliderFrame.height();
            sliderY = (int) (minY + (maxY - minY) * percentage);
        } else {
            int minX = getFrame().x();
            int maxX = getFrame().maxX() - sliderFrame.width();
            sliderX = (int) (minX + (maxX - minX) * percentage);
        }

        sliderFrame = new Rect(new Point(sliderX, sliderY), sliderFrame.size());
    }

    public enum ScrollAxis {
        HORIZONTAL(1, 0),
        VERTICAL(0, 1);

        public final Point vec;

        ScrollAxis(int x, int y) {
            this.vec = new Point(x, y);
        }
    }

    protected abstract void drawSlider(MatrixStack stack, Rect sliderFrame);

    protected abstract void adjustContent(double scrollPercentage); //move content based on scroll position
}
