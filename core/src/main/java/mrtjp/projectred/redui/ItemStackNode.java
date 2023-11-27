package mrtjp.projectred.redui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import net.minecraft.client.Minecraft;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

import static net.minecraft.client.gui.GuiComponent.fillGradient;

public class ItemStackNode extends AbstractGuiNode {

    private ItemStack itemStack;

    public ItemStackNode(ItemStack itemStack) {
        this.itemStack = itemStack;
        setSize(16, 16);
    }

    public ItemStackNode() {
        this(ItemStack.EMPTY);
    }

    public void setItemStack(ItemStack itemStack) {
        this.itemStack = itemStack;
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {

        // Would be nice if renderGuiItem can take the matrix stack...
        Point screenPos = convertParentPointToScreen(getPosition());
        getRoot().getItemRenderer().renderGuiItem(itemStack, screenPos.x, screenPos.y);

        if (isFirstHit(mouse)) {
            int slotColor = -2130706433;
            RenderSystem.disableDepthTest();
            RenderSystem.colorMask(true, true, true, false);
            Rect frame = getFrame();
            fillGradient(stack, frame.x(), frame.y(), frame.x() + frame.width(), frame.y() + frame.height(), slotColor, slotColor, 0);
            RenderSystem.colorMask(true, true, true, true);
            RenderSystem.enableDepthTest();
        }
    }

    @Override
    public void drawFront(PoseStack stack, Point mouse, float partialFrame) {
        if (isFirstHit(mouse)) {
            Minecraft minecraft = getRoot().getMinecraft();
            List<Component> tooltip = itemStack.getTooltipLines(minecraft.player, minecraft.options.advancedItemTooltips ? TooltipFlag.Default.ADVANCED : TooltipFlag.Default.NORMAL);
            renderTooltip(stack, mouse, tooltip);
        }
    }
}
