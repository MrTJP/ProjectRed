package mrtjp.projectred.redui;

import mrtjp.projectred.lib.Point;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;

import java.util.List;

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
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {

        // Render item
        graphics.renderItem(itemStack, getPosition().x, getPosition().y);

        // Draw tooltip
        if (isFirstHit(mouse)) {
            AbstractContainerScreen.renderSlotHighlight(graphics, getPosition().x, getPosition().y, 0);
        }
    }

    @Override
    public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {
        if (isFirstHit(mouse)) {
            Minecraft minecraft = getRoot().getMinecraft();
            List<Component> tooltip = itemStack.getTooltipLines(
                    Item.TooltipContext.of(getRoot().getMinecraft().level),
                    minecraft.player,
                    minecraft.options.advancedItemTooltips ? TooltipFlag.Default.ADVANCED : TooltipFlag.Default.NORMAL);
            renderTooltip(graphics, mouse, tooltip);
        }
    }
}
