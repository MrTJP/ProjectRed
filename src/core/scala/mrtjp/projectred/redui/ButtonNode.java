package mrtjp.projectred.redui;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.gui.GuiLib;
import mrtjp.core.vec.Point;
import net.minecraft.client.audio.SimpleSound;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.text.ITextProperties;
import net.minecraftforge.fml.client.gui.GuiUtils;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

import static net.minecraft.client.gui.AbstractGui.blit;
import static net.minecraft.client.gui.AbstractGui.drawCenteredString;

public class ButtonNode extends AbstractGuiNode {

    private Runnable clickFunction = () -> { };
    private Consumer<List<ITextProperties>> tooltipBuilder = c -> { };

    private String buttonText = "";

    public void setClickFunction(Runnable clickFunction) {
        this.clickFunction = clickFunction;
    }

    public void setTooltipBuilder(Consumer<List<ITextProperties>> tooltipBuilder) {
        this.tooltipBuilder = tooltipBuilder;
    }

    public void setButtonText(String buttonText) {
        this.buttonText = buttonText;
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {

        boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);

        TextureUtils.changeTexture(GuiLib.guiTex());
        int state = mouseover ? 2 : 1;

        int x = getPosition().x();
        int y = getPosition().y();
        int width = getFrame().width();
        int height = getFrame().height();

        blit(stack, x,              y,              0,                46 + state * 20,                    width / 2, height / 2, 256, 256);
        blit(stack, x + width / 2,  y,              200 - width / 2f, 46 + state * 20,                    width / 2, height / 2, 256, 256);
        blit(stack, x,              y + height / 2, 0,                46 + state * 20 + 20 - height / 2f, width / 2, height / 2, 256, 256);
        blit(stack, x + width / 2,  y + height / 2, 200 - width / 2f, 46 + state * 20 + 20 - height / 2f, width / 2, height / 2, 256, 256);

        drawCenteredString(stack, getRoot().getFontRenderer(), buttonText, x+width/2, y+(height-8)/2, mouseover ? 0xFFFFFFA0 : 0xFFE0E0E0);
    }

    @Override
    public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {

        if (!isFirstHit(mouse))
            return;

        List<ITextProperties> tooltip = new LinkedList<>();
        tooltipBuilder.accept(tooltip);

        // Draw tooltip in screen-space to allow it to force-fit on screen

        Point screenOffset = getParent().getScreenOffset();
        Point mouseScreenSpace = screenOffset.add(mouse);

        stack.pushPose();
        stack.translate(-screenOffset.x(), -screenOffset.y(), 0);

        GuiUtils.drawHoveringText(stack, tooltip, mouseScreenSpace.x(), mouseScreenSpace.y(), getRoot().getScreenFrame().width(), getRoot().getScreenFrame().height(), -1, getRoot().getFontRenderer());

        stack.popPose();
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (!consumed && isFirstHit(p)) {
            getRoot().getMinecraft().getSoundManager().play(SimpleSound.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
            clickFunction.run();
            return true;
        }
        return false;
    }
}
