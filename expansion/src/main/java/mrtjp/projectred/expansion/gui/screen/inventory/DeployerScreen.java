package mrtjp.projectred.expansion.gui.screen.inventory;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.inventory.container.DeployerContainerMenu;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;

public class DeployerScreen extends RedUIContainerScreen<DeployerContainerMenu> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/deployer.png");

    public DeployerScreen(DeployerContainerMenu menu, Inventory playerInventory, Component title) {
        super(176, 168, menu, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 75;

        initSubNodes();
    }

    private void initSubNodes() {
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());
    }
}
