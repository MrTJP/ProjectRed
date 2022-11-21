package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.util.text.ITextProperties;

import java.util.List;
import java.util.function.Consumer;

import static net.minecraft.client.gui.AbstractGui.drawCenteredString;

public class ButtonNode extends AbstractButtonNode {

    public static final int BUTTON_TEXT_COLOR_IDLE = 0xFFE0E0E0;
    public static final int BUTTON_TEXT_COLOR_HIGHLIGHT = 0xFFFFFFA0;

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
    protected void onButtonClicked() {
        clickFunction.run();
    }

    @Override
    protected boolean isButtonDisabled() {
        return false;
    }

    @Override
    protected void buildTooltip(List<ITextProperties> tooltip) {
        tooltipBuilder.accept(tooltip);
    }

    protected int getTextColor(boolean mouseover) {
        return mouseover ? BUTTON_TEXT_COLOR_HIGHLIGHT : BUTTON_TEXT_COLOR_IDLE;
    }

    @Override
    protected void drawButtonBody(MatrixStack stack, boolean mouseover) {

        drawCenteredString(stack, getRoot().getFontRenderer(), buttonText,
                getPosition().x() + getFrame().width()/2,
                getPosition().y()+(getFrame().height()-8)/2,
                getTextColor(mouseover));
    }
}
