package mrtjp.projectred.redui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.network.chat.Component;
import net.minecraft.sounds.SoundEvents;

import java.util.LinkedList;
import java.util.List;

import static net.minecraft.client.gui.GuiComponent.blit;

public abstract class AbstractButtonNode extends AbstractGuiNode {

    public static final int BUTTON_STATE_DISABLED = 0;
    public static final int BUTTON_STATE_IDLE = 1;
    public static final int BUTTON_STATE_HIGHLIGHT = 2;

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
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {

        boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);

        RenderSystem.setShaderTexture(0, GuiLib.WIDGETS_TEXTURE);
        int state = getButtonState(mouseover);

        drawMCButton(stack, state);
        drawButtonBody(stack, mouseover);
    }

    @Override
    public void drawFront(PoseStack stack, Point mouse, float partialFrame) {

        if (!isFirstHit(mouse))
            return;

        List<Component> tooltip = new LinkedList<>();
        buildTooltip(tooltip);

        if (!tooltip.isEmpty())
            renderTooltip(stack, mouse, tooltip);
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

    protected void drawMCButton(PoseStack stack, int state) {

        RenderSystem.setShaderTexture(0, GuiLib.WIDGETS_TEXTURE);

        int x = getPosition().x;
        int y = getPosition().y;
        int width = getFrame().width();
        int height = getFrame().height();

        blit(stack, x,              y,              0,                46 + state * 20,                    width / 2, height / 2, 256, 256);
        blit(stack, x + width / 2,  y,              200 - width / 2f, 46 + state * 20,                    width / 2, height / 2, 256, 256);
        blit(stack, x,              y + height / 2, 0,                46 + state * 20 + 20 - height / 2f, width / 2, height / 2, 256, 256);
        blit(stack, x + width / 2,  y + height / 2, 200 - width / 2f, 46 + state * 20 + 20 - height / 2f, width / 2, height / 2, 256, 256);
    }

    protected abstract void drawButtonBody(PoseStack stack, boolean mouseover);
}
