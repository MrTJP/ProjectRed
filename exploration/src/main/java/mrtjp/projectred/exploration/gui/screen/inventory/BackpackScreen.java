package mrtjp.projectred.exploration.gui.screen.inventory;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.exploration.inventory.container.BackpackContainer;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;

import static mrtjp.projectred.ProjectRedExploration.MOD_ID;

public class BackpackScreen extends RedUIContainerScreen<BackpackContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/backpack.png");

    public BackpackScreen(BackpackContainer container, PlayerInventory playerInventory, ITextComponent title) {
        super(176, 168, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 75;
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        super.drawBack(stack, mouse, partialFrame);

        TextureUtils.changeTexture(BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        return super.onKeyPressed(glfwKeyCode, glfwScanCode, glfwFlags, consumed);
    }
}
