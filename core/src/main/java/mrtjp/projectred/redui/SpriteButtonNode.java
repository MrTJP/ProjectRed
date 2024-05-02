package mrtjp.projectred.redui;

import net.minecraft.client.gui.GuiGraphics;

import java.util.function.Supplier;

public class SpriteButtonNode extends AbstractButtonNode {

    private RedUISprite sprite;
    private Runnable clickReceiver = () -> { };
    private Supplier<Boolean> isDisabledProvider = () -> false;

    public SpriteButtonNode(RedUISprite sprite) {
        this.sprite = sprite;
    }

    public void setClickReceiver(Runnable clickReceiver) {
        this.clickReceiver = clickReceiver;
    }

    public void setIsDisabledProvider(Supplier<Boolean> isDisabledProvider) {
        this.isDisabledProvider = isDisabledProvider;
    }

    @Override
    protected boolean isButtonDisabled() {
        return isDisabledProvider.get();
    }

    @Override
    protected void onButtonClicked() {
        clickReceiver.run();
    }

    @Override
    protected void drawButtonBody(GuiGraphics graphics, boolean mouseover) {
        blitSpriteCentered(graphics, sprite);
    }
}
