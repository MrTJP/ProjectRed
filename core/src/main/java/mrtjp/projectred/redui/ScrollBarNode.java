package mrtjp.projectred.redui;

import codechicken.lib.colour.EnumColour;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.lib.Size;
import net.minecraft.client.gui.GuiComponent;

public abstract class ScrollBarNode extends AbstractGuiNode {

    private final ScrollAxis axis;

    private Point initialClickPosition = Point.ZERO;
    private Rect initialSliderFrame = new Rect(0, 0, 0, 0);
    private boolean isDraggingSlider = false;
    private Rect sliderFrame = new Rect(0, 0, 0, 0);

    private double scrollPercentage = 0.0;

    public ScrollBarNode(ScrollAxis axis) {
        this.axis = axis;
    }

    public void setSliderSize(int w, int h) {
        sliderFrame = new Rect(sliderFrame.origin, new Size(w, h));
        setScrollPercentage(0);
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {

        // Draw semi-transparent grey background
        int x = getFrame().x();
        int y = getFrame().y();
        GuiComponent.fillGradient(stack, x, y, x + getFrame().width(), y + getFrame().height(), EnumColour.BLACK.argb(127), EnumColour.BLACK.argb(127), 0);

        // Draw slider rectangle
        drawSlider(stack, sliderFrame);
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (isFirstHit(p)) {
            if (sliderFrame.contains(p)) {
                isDraggingSlider = true;
                initialClickPosition = p;
                initialSliderFrame = sliderFrame;
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

        Point delta = p.subtract(initialClickPosition).multiply(axis.vec);
        Rect targetSliderFrame = new Rect(initialSliderFrame.origin.add(delta), sliderFrame.size);
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

        sliderFrame = new Rect(new Point(sliderX, sliderY), sliderFrame.size);
    }

    public enum ScrollAxis {
        HORIZONTAL(1, 0),
        VERTICAL(0, 1);

        public final Point vec;

        ScrollAxis(int x, int y) {
            this.vec = new Point(x, y);
        }
    }

    protected abstract void drawSlider(PoseStack stack, Rect sliderFrame);

    protected abstract void adjustContent(double scrollPercentage); //move content based on scroll position
}
