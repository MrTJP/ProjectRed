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

    boolean onKeyPressed(int glfwKeyCode, int glfwFlags);
}
