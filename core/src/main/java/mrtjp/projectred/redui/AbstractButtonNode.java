package mrtjp.projectred.redui;

import mrtjp.projectred.lib.Point;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;

import java.util.LinkedList;
import java.util.List;

public abstract class AbstractButtonNode extends AbstractGuiNode {

    public static final int BUTTON_STATE_DISABLED = 0;
    public static final int BUTTON_STATE_IDLE = 1;
    public static final int BUTTON_STATE_HIGHLIGHT = 2;

    private static final ResourceLocation[] buttonSprites = {
        ResourceLocation.withDefaultNamespace("widget/button_disabled"),
        ResourceLocation.withDefaultNamespace("widget/button"),
        ResourceLocation.withDefaultNamespace("widget/button_highlighted")
    };

    protected abstract void onButtonClicked();

    protected boolean isButtonDisabled() {
        return false;
    }

    protected void buildTooltip(List<Component> tooltip) {
    }

    protected int getButtonState(boolean mouseover) {
        return isButtonDisabled() ? BUTTON_STATE_DISABLED : mouseover ? BUTTON_STATE_HIGHLIGHT : BUTTON_STATE_IDLE;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {

        boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);
        int state = getButtonState(mouseover);

        drawMCButton(graphics, state);
        drawButtonBody(graphics, mouseover);
    }

    @Override
    public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {

        if (!isFirstHit(mouse))
            return;

        List<Component> tooltip = new LinkedList<>();
        buildTooltip(tooltip);

        if (!tooltip.isEmpty())
            renderTooltip(graphics, mouse, tooltip);
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (!consumed && !isButtonDisabled() && isFirstHit(p)) {
            getRoot().getMinecraft().getSoundManager().play(SimpleSoundInstance.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
            onButtonClicked();
            return true;
        }
        return false;
    }

    protected void drawMCButton(GuiGraphics graphics, int state) {

        int x = getPosition().x;
        int y = getPosition().y;
        int width = getFrame().width();
        int height = getFrame().height();

        graphics.blitSprite(buttonSprites[state], x, y, width, height);
    }

    protected abstract void drawButtonBody(GuiGraphics graphics, boolean mouseover);
}
