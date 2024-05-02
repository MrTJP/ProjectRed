package mrtjp.projectred.redui;

import codechicken.lib.colour.EnumColour;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.lib.Size;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import org.lwjgl.glfw.GLFW;

import java.util.Collections;

public class DebugRectNode extends AbstractGuiNode {

    private boolean clickDown = false;
    private int colorArgb = EnumColour.RED.argb();
    private String name = "";

    public void setName(String name) {
        this.name = name;
    }

    public void setColorArgb(int colorArgb) {
        this.colorArgb = colorArgb;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        GuiLib.drawLine(graphics, getFrame().x(),      getFrame().y(),     getFrame().maxX(),  getFrame().y(),     3, EnumColour.RED.argb());
        GuiLib.drawLine(graphics, getFrame().maxX(),   getFrame().y(),     getFrame().maxX(),  getFrame().maxY(),  3, EnumColour.RED.argb());
        GuiLib.drawLine(graphics, getFrame().maxX(),   getFrame().maxY(),  getFrame().x(),     getFrame().maxY(),  3, EnumColour.RED.argb());
        GuiLib.drawLine(graphics, getFrame().x(),      getFrame().maxY(),  getFrame().x(),     getFrame().y(),     3, EnumColour.RED.argb());

        graphics.fill(getFrame().x(), getFrame().y(), getFrame().maxX(), getFrame().maxY(), colorArgb);
    }

    @Override
    public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {

        if (!isFirstHit(mouse))
            return;

        // Rect around mouse position
        Rect cursorRect = new Rect(mouse.subtract(3, 3), new Size(6, 6));
        graphics.fill(cursorRect.x(), cursorRect.y(), cursorRect.maxX(), cursorRect.maxY(), EnumColour.WHITE.argb(clickDown ? 150 : 50));

        // Tooltip showing name
        renderTooltip(graphics, mouse, Collections.singletonList(Component.literal(name)));
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (!consumed&& glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT && isFirstHit(p) ) {
            clickDown = true;
            return true;
        }
        return false;
    }

    @Override
    public boolean mouseReleased(Point p, int glfwMouseButton, long timeHeld, boolean consumed) {
        if (clickDown && glfwMouseButton == GLFW.GLFW_MOUSE_BUTTON_LEFT) {
            clickDown = false;
            return true;
        }
        return false;
    }
}
