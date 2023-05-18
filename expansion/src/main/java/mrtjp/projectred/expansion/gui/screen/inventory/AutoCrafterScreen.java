package mrtjp.projectred.expansion.gui.screen.inventory;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.expansion.inventory.container.AutoCrafterContainer;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractButtonNode;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import org.lwjgl.glfw.GLFW;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

public class AutoCrafterScreen extends RedUIContainerScreen<AutoCrafterContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/auto_crafter.png");

    private boolean isShiftDown = false;

    public AutoCrafterScreen(AutoCrafterContainer container, PlayerInventory playerInventory, ITextComponent title) {
        super(176, 212, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 120;

        initSubNodes();
    }

    private void initSubNodes() {
        AbstractButtonNode cyclePlanButton = new AbstractButtonNode() {
            @Override
            protected void onButtonClicked() {
                getMenu().getAutoCrafterTile().sendCyclePlan();
            }

            @Override
            protected void drawButtonBody(MatrixStack stack, boolean mouseover) {
                TextureUtils.changeTexture(BACKGROUND);
                blit(stack, getPosition().x, getPosition().y, 176, 0, 14, 14);
            }
        };
        cyclePlanButton.setPosition(126, 23);
        cyclePlanButton.setSize(14, 14);
        addChild(cyclePlanButton);
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        super.drawBack(stack, mouse, partialFrame);

        TextureUtils.changeTexture(BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());

        if (getMenu().canConductorWork())
            blit(stack, x + 16, y + 16, 177, 18, 7, 9);

        GuiLib.drawVerticalTank(stack, this, x + 16, y + 26, 177, 27, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isFlowFull())
            blit(stack, x + 27, y + 16, 185, 18, 7, 9);

        GuiLib.drawVerticalTank(stack, this, x + 27, y + 26, 185, 27, 7, 48, getMenu().getFlowScaled(48));

        // Draw progress bar
        int s = getMenu().getProgressScaled(22);
        blit(stack, x + 100, y + 41, 193, 23, s + 1, 14);

        // Draw plan output stack
        ItemStack output = getMenu().getPlanOutput();
        if (!output.isEmpty()) {
            getItemRenderer().renderGuiItem(output, x + 125, y + 40);
        }
    }

    @Override
    public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {
        // Plan output overlays
        if (isShiftDown) {
            drawPlanOutputsOverlay(stack, getFrame().x(), getFrame().y());
        }

        // Selected plan slot
        drawPlanSlotSelection(stack, getFrame().x() + 44, getFrame().y() + 22);
    }

    private void drawPlanSlotSelection(MatrixStack stack, int xPos, int yPos) {
        TextureUtils.changeTexture(BACKGROUND);
        int s = getMenu().getPlanSlot();
        int ix = s % 3;
        int iy = s / 3;
        int x = xPos + 18 * ix - 3;
        int y = yPos + 18 * iy - 3;
        blit(stack, x, y, 193, 0, 22, 22);
    }

    private void drawPlanOutputsOverlay(MatrixStack mStack, int xPos, int yPos) {

        for (Slot slot : getMenu().slots) {
            ItemStack stack = slot.getItem();
            if (RecipePlanItem.hasRecipeInside(stack)) {

                ItemStack output = RecipePlanItem.loadPlanOutput(stack);
                int colour = EnumColour.LIGHT_BLUE.argb(0xCC);
                fillGradient(mStack, xPos + slot.x, yPos + slot.y, xPos + slot.x + 16, yPos + slot.y + 16, colour, colour);

                ItemRenderer renderer = getItemRenderer();
                renderer.blitOffset += 200;
                renderer.renderGuiItem(output, xPos + slot.x, yPos + slot.y);
                renderer.blitOffset -= 200;
            }
        }
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        if (!consumed && glfwKeyCode == GLFW.GLFW_KEY_LEFT_SHIFT) {
            isShiftDown = true;
            return true;
        }
        return false;
    }

    @Override
    public boolean onKeyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        if (glfwKeyCode == GLFW.GLFW_KEY_LEFT_SHIFT) {
            isShiftDown = false;
            return true;
        }
        return false;
    }
}
