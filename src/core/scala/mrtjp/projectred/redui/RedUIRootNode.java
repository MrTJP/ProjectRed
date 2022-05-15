package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.renderer.ItemRenderer;

import javax.annotation.Nullable;

public interface RedUIRootNode extends RedUINode {

    Minecraft getMinecraft();

    ItemRenderer getItemRenderer();

    FontRenderer getFontRenderer();

    Rect getScreenFrame();

    @Override
    default RedUIRootNode getRoot() {
        return this;
    }

    @Override
    default RedUINode getParent() {
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

    default void drawBackForSubtree(MatrixStack stack, Point mouse, float partialFrame) {
        renderBackForSubtree(stack, mouse, partialFrame);
    }

    default void drawFrontForSubtree(MatrixStack stack, Point mouse, float partialFrame) {
        renderFrontForSubtree(stack, mouse, partialFrame);
    }
}
