package mrtjp.projectred.fabrication.gui;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.audio.SimpleSound;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.text.ITextProperties;

import java.util.LinkedList;
import java.util.List;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Predicate;

import static net.minecraft.client.gui.AbstractGui.blit;

public class SelectionGridNode extends AbstractGuiNode {

    private static final int BUTTON_WIDTH = 16;
    private static final int BUTTON_HEIGHT = 16;
    private static final int BUTTON_U = 495;
    private static final int BUTTON_V = 1;
    private static final int BUTTON_V_SHIFT = 17;

    private final int gridWidth;
    private final int gridHeight;

    private Consumer<Integer> clickFunction = i -> { };
    private Predicate<Integer> selectionCheckFunction = i -> false;
    private BiConsumer<Integer, List<ITextProperties>> tooltipBuilder = (i, l) -> { };

    public SelectionGridNode(int gridWidth, int gridHeight) {
        this.gridWidth = gridWidth;
        this.gridHeight = gridHeight;
        setSize(gridWidth * BUTTON_WIDTH, gridHeight * BUTTON_HEIGHT);
        addButtons();
    }

    public void setClickFunction(Consumer<Integer> clickFunction) {
        this.clickFunction = clickFunction;
    }

    public void setTooltipBuilder(BiConsumer<Integer, List<ITextProperties>> tooltipBuilder) {
        this.tooltipBuilder = tooltipBuilder;
    }

    public void setSelectionCheckFunction(Predicate<Integer> selectionCheckFunction) {
        this.selectionCheckFunction = selectionCheckFunction;
    }

    private void addButtons() {

        for (int y = 0; y < gridHeight; y++) {
            for (int x = 0; x < gridWidth; x++) {

                SelectionGridButtonNode button = new SelectionGridButtonNode(x + y * gridHeight);
                button.setPosition(x * button.getFrame().width(), y * button.getFrame().height());
                addChild(button);
            }
        }
    }

    private class SelectionGridButtonNode extends AbstractGuiNode {

        private final int i;

        public SelectionGridButtonNode(int i) {
            this.i = i;
            this.setSize(BUTTON_WIDTH, BUTTON_HEIGHT);
        }

        @Override
        public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {

            TextureUtils.changeTexture(ICWorkbenchScreen.BACKGROUND);

            boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);

            int state = SelectionGridNode.this.selectionCheckFunction.test(i) ? 2 :
                    mouseover ? 1 : 0;

            int x = getPosition().x();
            int y = getPosition().y();
            int w = getFrame().width();
            int h = getFrame().height();
            int u = BUTTON_U;
            int v = BUTTON_V + BUTTON_V_SHIFT * state;

            blit(stack, x, y, u, v, w, h, 512, 512);


        }

        @Override
        public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {

            if (!isFirstHit(mouse)) return;

            List<ITextProperties> tooltip = new LinkedList<>();
            tooltipBuilder.accept(i, tooltip);

            renderTooltip(stack, mouse, tooltip);
        }

        @Override
        public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
            if (!consumed && isFirstHit(p)) {
                getRoot().getMinecraft().getSoundManager().play(SimpleSound.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
                clickFunction.accept(i);
                return true;
            }
            return false;
        }
    }
}
