package mrtjp.projectred.redui;

import mrtjp.projectred.lib.Point;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.Screen;

import static org.lwjgl.glfw.GLFW.*;

public abstract class TextBoxNode extends AbstractGuiNode {

    private static final int BORDER_COLOR = 0xFFA0A0A0;
    private static final int BACKGROUND_COLOR = 0xFF000000;
    private static final int SUGGESTION_COLOR = 0xFFA0A0A0;
    private static final int ENABLED_COLOR = 0xE0E0E0;
    private static final int DISABLED_COLOR = 0x707070;

    protected String currentText = "";

    protected int characterLimit = -1; // by default, width based limit
    protected boolean focused = false;
    protected boolean enabled = true;

    public TextBoxNode(String initialText) {
        this.currentText = initialText;
    }

    public String getText() {
        return currentText;
    }

    public void setText(String text) {
        this.currentText = text;
    }

    public boolean isEditing() {
        return enabled && focused;
    }

    public void setCharacterLimit(int limit) {
        this.characterLimit = limit;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        // Border
        int x = getPosition().x;
        int y = getPosition().y;
        int width = getFrame().width();
        int height = getFrame().height();

        graphics.fill(x, y, x + width, y + height, BORDER_COLOR);
        graphics.fill(x + 1, y + 1, x + width - 1, y + height - 1, BACKGROUND_COLOR);

        Font fontRenderer = getRoot().getFontRenderer();
        String suggestion = getSuggestionString();
        int lineHeight = fontRenderer.lineHeight;
        int leftPad = 4;

        int textX = x + leftPad;
        int textY = y + height/2 - lineHeight/2;

        if (currentText.isEmpty() && !suggestion.isEmpty()) {
            graphics.drawString(fontRenderer, suggestion, textX, textY, SUGGESTION_COLOR, false);
        }

        if (!currentText.isEmpty()) {
            String s = currentText;
            if (enabled && focused && System.currentTimeMillis() % 1000 > 1000/2) {
                s += "_";
            }

            graphics.drawString(fontRenderer, s, textX, textY, enabled ? ENABLED_COLOR : DISABLED_COLOR, true);
        }
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (enabled && !consumed && isFirstHit(p)) {
            setFocused(true);
            if (glfwMouseButton == GLFW_MOUSE_BUTTON_RIGHT) {
                currentText = "";
            }
            return true;
        }

        setFocused(false);
        return false;
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {

        if (!focused || !enabled) return false;

        boolean changed = false;

        if (Screen.isPaste(glfwKeyCode)) {
            String clipboard = Minecraft.getInstance().keyboardHandler.getClipboard();
            changed = appendText(clipboard);
        } else {
            switch (glfwKeyCode) {
                case GLFW_KEY_BACKSPACE:
                    changed = backspaceText();
                    break;
                case GLFW_KEY_ESCAPE:
                    setFocused(false);
                    break;
                case GLFW_KEY_ENTER:
                    setFocused(false);
                    onReturnPressed();
                    break;
            }
        }

        if (changed) {
            onTextChanged();
            return true;
        }
        return false;
    }

    @Override
    public boolean onCharTyped(char ch, int glfwFlags, boolean consumed) {
        if (!focused || !enabled) return false;

        if (appendChar(ch)) {
            onTextChanged();
            return true;
        }
        return false;
    }

    private void setFocused(boolean focused) {
        this.focused = focused;
    }

    private boolean appendText(String text) {
        StringBuilder builder = new StringBuilder(currentText);

        char[] chars = text.toCharArray();
        int i = 0;

        while (builder.length() < getCharacterLimit() && i < chars.length) {
            char next = chars[i++];
            if (isCharAllowed(next)) {
                builder.append(next);
            }
        }

        String newText = builder.toString();

        if (!newText.equals(currentText)) {
            currentText = newText;
            return true;
        }
        return false;
    }

    private boolean appendChar(char c) {
        if (currentText.length() < getCharacterLimit() && isCharAllowed(c)) {
            currentText += c;
            return true;
        }
        return false;
    }

    private boolean backspaceText() {
        if (currentText.length() > 0) {
            currentText = currentText.substring(0, currentText.length() - 1);
            return true;
        }
        return false;
    }

    private boolean isCharAllowed(char c) {
        return true;
    }

    private int getCharacterLimit() {
        if (characterLimit == -1) {
            return getFrame().width() / 6;
        }
        return characterLimit;
    }

    protected abstract String getSuggestionString();

    protected abstract void onTextChanged();

    protected abstract void onReturnPressed();
}
