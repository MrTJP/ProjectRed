package mrtjp.projectred.fabrication.gui;

import mrtjp.projectred.redui.AbstractButtonNode;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ButtonNode;

import java.util.ArrayList;

public class ButtonArrayNode extends AbstractGuiNode {

    private final Listener listener;

    private final int rows;
    private final int columns;
    private final int spacing;

    private final ArrayList<AbstractButtonNode> buttons = new ArrayList<>();

    public ButtonArrayNode(Listener listener, int rows, int columns, int spacing) {
        this.listener = listener;
        this.rows = rows;
        this.columns = columns;
        this.spacing = spacing;

        initButtons();
    }

    private void initButtons() {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                AbstractButtonNode button = createButton(i * columns + j);
                addChild(button);
                buttons.add(button);
            }
        }
    }

    private void resizeButtons(int bw, int bh, int spacing) {

        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < columns; j++) {
                AbstractButtonNode button = buttons.get(i * columns + j);
                button.setSize(bw, bh);
                button.setPosition(j * (bw + spacing), i * (bh + spacing));
            }
        }

        setSize(columns * (bw + spacing) - spacing, rows * (bh + spacing) - spacing);
    }

    @Override
    public void setSize(int width, int height) {
        super.setSize(width, height);
    }

    public void setGridSize(int width, int height) {
        int bw = (width + spacing) / columns - spacing; // width = columns * (bw + spacing) - spacing
        int bh = (height + spacing) / rows - spacing; // height = rows * (bh + spacing) - spacing
        resizeButtons(bw, bh, spacing);
    }

    public void setButtonSize(int buttonWidth, int buttonHeight) {
        resizeButtons(buttonWidth, buttonHeight, spacing);
    }

    protected AbstractButtonNode createButton(int index) {

        ButtonNode button = new ButtonNode();
        button.setButtonText(listener.getButtonText(index));
        button.setClickFunction(() -> listener.onButtonClicked(index));
        button.setIsSelectedFunction(() -> listener.isButtonSelected(index));
        button.setIsEnabledFunction(() -> listener.isButtonEnabled(index));

        return button;
    }

    public interface Listener {
        String getButtonText(int index);

        void onButtonClicked(int index);

        boolean isButtonEnabled(int index);

        boolean isButtonSelected(int index);
    }
}
