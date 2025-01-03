package mrtjp.projectred.expansion.gui.screen.inventory;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.inventory.container.DeployerMenu;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class DeployerScreen extends RedUIContainerScreen<DeployerMenu> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/deployer.png");

    public DeployerScreen(DeployerMenu menu, Inventory playerInventory, Component title) {
        super(176, 168, menu, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 75;

        initSubNodes();
    }

    private void initSubNodes() {
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        int x = getFrame().x();
        int y = getFrame().y();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());
    }
}
