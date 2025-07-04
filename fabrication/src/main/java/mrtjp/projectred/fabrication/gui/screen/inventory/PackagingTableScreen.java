package mrtjp.projectred.fabrication.gui.screen.inventory;

import codechicken.lib.colour.EnumColour;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.inventory.container.PackagingTableMenu;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;

public class PackagingTableScreen extends RedUIContainerScreen<PackagingTableMenu> {

    public static final ResourceLocation BACKGROUND = ResourceLocation.fromNamespaceAndPath(ProjectRedFabrication.MOD_ID, "textures/gui/packaging_table.png");

    public PackagingTableScreen(PackagingTableMenu container, Inventory playerInventory, Component title) {
        super(176, 171, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 79;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        super.drawBack(graphics, mouse, partialFrame);

        int x = getFrame().x();
        int y = getFrame().y();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());

        int s = getMenu().getProgressScaled(20);
        graphics.blit(BACKGROUND, x + 104, y + 41, 176, 0, s + 1, 16);

        if (getMenu().canConductorWork())
            graphics.blit(BACKGROUND, x + 16, y + 16, 177, 18, 7, 9);

        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 16, y + 26, 177, 27, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isFlowFull())
            graphics.blit(BACKGROUND, x + 27, y + 16, 185, 18, 7, 9);

        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 27, y + 26, 185, 27, 7, 48, getMenu().getFlowScaled(48));

        // Highlight problematic slots red.
        int mask = getMenu().getProblematicSlotMask();
        for (int i = 0; i < 9; i++) {
            if ((mask & 1 << i) != 0) {
                Slot slot = getMenu().getSlot(i + 36); // Offset past player inventory
                int colour = EnumColour.RED.argb(0x55);
                graphics.fillGradient(
                        x + slot.x,
                        y + slot.y,
                        x + slot.x + 16,
                        y + slot.y + 16, colour, colour);
            }
        }
    }
}
