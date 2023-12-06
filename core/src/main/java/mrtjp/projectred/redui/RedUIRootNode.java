package mrtjp.projectred.redui;

import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.network.chat.Component;

import javax.annotation.Nullable;
import java.util.List;

public interface RedUIRootNode extends RedUINode {

    Minecraft getMinecraft();

    ItemRenderer getItemRenderer();

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

    default void drawBackForSubtree(PoseStack stack, Point mouse, float partialFrame) {
        renderBackForSubtree(stack, mouse, partialFrame);
    }

    default void drawFrontForSubtree(PoseStack stack, Point mouse, float partialFrame) {
        renderFrontForSubtree(stack, mouse, partialFrame);
    }

    void renderTooltipScreenSpace(PoseStack stack, Point screenSpacePoint, List<Component> tooltip);
}
