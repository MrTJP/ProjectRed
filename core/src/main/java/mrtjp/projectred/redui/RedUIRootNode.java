package mrtjp.projectred.redui;

import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;

import javax.annotation.Nullable;

public interface RedUIRootNode extends RedUINode {

    Minecraft getMinecraft();

    Font getFontRenderer();

    Rect getScreenFrame();

    @Override
    default RedUIRootNode getRoot() {
        return this;
    }

    @Override
    default @Nullable RedUINode getParent() {
        throw new RuntimeException("Cannot get parent of root node");
    }

    @Override
    default void setRoot(@Nullable RedUIRootNode root) {
        throw new RuntimeException("Cannot set root of root node!");
    }

    @Override
    default void setParent(@Nullable RedUINode parent) {
        throw new RuntimeException("Cannot set parent of root node!");
    }

    @Override
    default double getRelativeZPosition() {
        return getZPosition(); // Assume screen's base z position is zero
    }

    default void drawBackForSubtree(GuiGraphics graphics, Point mouse, float partialFrame) {
        renderBackForSubtree(graphics, mouse, partialFrame);
    }

    default void drawFrontForSubtree(GuiGraphics graphics, Point mouse, float partialFrame) {
        renderFrontForSubtree(graphics, mouse, partialFrame);
    }
}
