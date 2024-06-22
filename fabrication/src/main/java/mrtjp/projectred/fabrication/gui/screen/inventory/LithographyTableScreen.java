package mrtjp.projectred.fabrication.gui.screen.inventory;

import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.inventory.container.LithographyTableContainer;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class LithographyTableScreen extends RedUIContainerScreen<LithographyTableContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/lithography_table.png");

    public LithographyTableScreen(LithographyTableContainer container, Inventory playerInventory, Component title) {
        super(176, 171, container, playerInventory, title); //TODO size

        inventoryLabelX = 8;
        inventoryLabelY = 79;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        super.drawBack(graphics, mouse, partialFrame);

        int x = getFrame().x();
        int y = getFrame().y();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());

        int s = getMenu().getProgressScaled(24);
        graphics.blit(BACKGROUND, x + 80, y + 40, 176, 0, s + 1, 16);

        if (getMenu().canConductorWork())
            graphics.blit(BACKGROUND, x + 16, y + 16, 177, 18, 7, 9);

        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 16, y + 26, 177, 27, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isFlowFull())
            graphics.blit(BACKGROUND, x + 27, y + 16, 185, 18, 7, 9);

        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 27, y + 26, 185, 27, 7, 48, getMenu().getFlowScaled(48));
    }
}
