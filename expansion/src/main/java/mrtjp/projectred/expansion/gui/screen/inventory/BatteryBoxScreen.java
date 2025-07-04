package mrtjp.projectred.expansion.gui.screen.inventory;

import mrtjp.projectred.expansion.inventory.container.BatteryBoxMenu;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class BatteryBoxScreen extends RedUIContainerScreen<BatteryBoxMenu> {

    public static final ResourceLocation BACKGROUND = ResourceLocation.fromNamespaceAndPath(MOD_ID, "textures/gui/battery_box.png");

    public BatteryBoxScreen(BatteryBoxMenu container, Inventory playerInventory, Component title) {
        super(176, 171, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 79;
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {

        int x = getFrame().x();
        int y = getFrame().y();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());

        if (getMenu().canConductorWork())
            graphics.blit(BACKGROUND, x + 57, y + 16, 176, 1, 7, 9);
        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 57, y + 26, 176, 10, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isPowerStorageFull())
            graphics.blit(BACKGROUND, x + 112, y + 16, 184, 1, 14, 9);
        GuiLib.drawVerticalTank(graphics, BACKGROUND, x + 112, y + 26, 184, 10, 14, 48, getMenu().getPowerStoredScaled(48));

        if (getMenu().isStorageCharging())
            graphics.blit(BACKGROUND, x + 65, y + 52, 199, 18, 48, 18);
        else if (getMenu().isStorageDischarging())
            graphics.blit(BACKGROUND, x + 65, y + 30, 199, 0, 48, 18);
    }
}
