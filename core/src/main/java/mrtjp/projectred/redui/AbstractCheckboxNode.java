package mrtjp.projectred.redui;

import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.Point;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.network.chat.Component;
import net.minecraft.sounds.SoundEvents;

import java.util.LinkedList;
import java.util.List;

public abstract class AbstractCheckboxNode extends AbstractGuiNode {

    public AbstractCheckboxNode() {
        setSize(18, 18);
    }

    protected boolean isDisabled() {
        return false;
    }

    protected abstract boolean isChecked();

    protected abstract void onClicked();

    protected void buildTooltip(List<Component> tooltip) {
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {

        boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);
        RedUISprite sprite = RedUISprites.getCheckboxSprite(isDisabled(), mouseover, isChecked());
        blitSprite(stack, sprite);
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
        if (!consumed && !isDisabled() && isFirstHit(p)) {
            getRoot().getMinecraft().getSoundManager().play(SimpleSoundInstance.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
            onClicked();
            return true;
        }
        return false;
    }
}
