package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.List;

public interface InteractionZone {

    Cuboid6 getBounds();

    void onLeftClick();

    void onRightClick();

    @OnlyIn(Dist.CLIENT)
    void renderZone(CCRenderState ccrs, MultiBufferSource getter, PoseStack poseStack, boolean isSelected, boolean isMouseOver);

    @OnlyIn(Dist.CLIENT)
    void buildToolTip(List<Component> tooltip);

    /**
     * Client-side check to see if this zone can respond to a key press. If true, client sends packet
     * to respond to key press via {@link #onKeyPressed(int, int)}.
     */
    boolean canRespondToKey(int glfwKeyCode, int glfwFlags);

    /**
     * Server-side key press event. Called if client-side {@link #canRespondToKey(int, int)} returns true.
     */
    boolean onKeyPressed(int glfwKeyCode, int glfwFlags);
}
