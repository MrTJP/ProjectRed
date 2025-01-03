package mrtjp.projectred.core.gui.screen.inventory;

import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorMenu;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class ElectrotineGeneratorScreen extends RedUIContainerScreen<ElectrotineGeneratorMenu> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/electrotine_generator.png");

    public ElectrotineGeneratorScreen(ElectrotineGeneratorMenu container, Inventory playerInventory, Component title) {
        super(176, 171, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 79;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        super.drawBack(graphics, mouse, partialFrame);

        int x = getFrame().x();
        int y = getFrame().y();
        // Background
        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());

        // Charge icon
        if (getMenu().canConductorWork()) {
            graphics.blit(BACKGROUND, x + 22, y + 16, 176, 1, 7, 9);
        }
        // Charge Bar
        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 22, y + 26, 176, 10, 7, 48, getMenu().getChargeScaled(48));

        // Storage icon
        if (getMenu().isPowerStorageFull()) {
            graphics.blit(BACKGROUND, x + 54, y + 16, 184, 1, 14, 9);
        }
        // Storage Bar
        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 54, y + 26, 184, 10, 14, 48, getMenu().getPowerStoredScaled(48));

        // Burning icon
        if (getMenu().isBurning()) {
            graphics.blit(BACKGROUND, x + 93, y + 16, 199, 1, 7, 9);
        }
        // Burning Bar
        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 93, y + 26, 199, 10, 7, 48, getMenu().getBurnTimeScaled(48));

        // Storage charging arrow
        if (getMenu().isChargingStorage()) {
            graphics.blit(BACKGROUND, x + 69, y + 45, 211, 10, 23, 9);
        }

        // Conductor charging arrow
        if (getMenu().isChargingConductor()) {
            graphics.blit(BACKGROUND, x + 30, y + 46, 211, 0, 23, 9);
        }
    }
}
