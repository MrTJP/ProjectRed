package mrtjp.projectred.expansion.gui.screen.inventory;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchMenu;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class ChargingBenchScreen extends RedUIContainerScreen<ChargingBenchMenu> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/charging_bench.png");

    public ChargingBenchScreen(ChargingBenchMenu container, Inventory playerInventory, Component title) {
        super(176, 183, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 91;
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());

        if (getMenu().canConductorWork())
            blit(stack, x + 14, y + 17, 176, 1, 7, 9);
        GuiLib.drawVerticalTank(stack, this, x + 14, y + 27, 176, 10, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isPowerStorageFull())
            blit(stack, x + 41, y + 17, 184, 1, 14, 9);
        GuiLib.drawVerticalTank(stack, this, x + 41, y + 27, 184, 10, 14, 48, getMenu().getPowerStoredScaled(48));

        if (getMenu().isStorageCharging())
            blit(stack, x + 26, y + 48, 199, 0, 10, 8);

        if (getMenu().areItemsCharging())
            blit(stack, x + 63, y + 29, 210, 0, 17, 10);
    }
}
