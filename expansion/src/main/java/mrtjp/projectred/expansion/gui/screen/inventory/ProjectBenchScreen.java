package mrtjp.projectred.expansion.gui.screen.inventory;

import codechicken.lib.colour.EnumColour;
import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.inventory.container.ProjectBenchContainer;
import mrtjp.projectred.expansion.item.RecipePlanItem;
import mrtjp.projectred.expansion.tile.ProjectBenchTile;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractButtonNode;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import org.lwjgl.glfw.GLFW;

public class ProjectBenchScreen extends RedUIContainerScreen<ProjectBenchContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/project_bench.png");

    private boolean isShiftDown = false;

    public ProjectBenchScreen(ProjectBenchContainer container, Inventory playerInventory, Component title) {
        super(176, 208, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 116;

        initSubNodes();
    }

    private void initSubNodes() {

        AbstractButtonNode writePlanButton = new AbstractButtonNode() {
            @Override
            protected void onButtonClicked() {
                getMenu().getProjectBenchTile().sendWriteButtonPressed();
            }

            @Override
            protected void drawButtonBody(GuiGraphics graphics, boolean mouseover) {
                graphics.blit(BACKGROUND, getPosition().x, getPosition().y, 176, 0, 14, 14);
            }
        };
        writePlanButton.setPosition(18, 56);
        writePlanButton.setSize(14, 14);
        addChild(writePlanButton);

        AbstractButtonNode clearGridButton = new AbstractButtonNode() {
            @Override
            protected void onButtonClicked() {
                getMenu().getProjectBenchTile().sendGridClearButtonPressed();
            }

            @Override
            protected void drawButtonBody(GuiGraphics graphics, boolean mouseover) {
                RenderSystem.setShaderTexture(0, BACKGROUND);
                graphics.blit(BACKGROUND, getPosition().x, getPosition().y, 176, 15, 8, 8);
            }
        };
        clearGridButton.setPosition(37, 17);
        clearGridButton.setSize(8, 8);
        addChild(clearGridButton);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        int x = getFrame().x();
        int y = getFrame().y();

        graphics.blit(BACKGROUND, x, y, 0, 0, getFrame().width(), getFrame().height());

        ProjectBenchTile tile = getMenu().getProjectBenchTile();
        if (tile.isPlanRecipe()) {
            int missingMask = tile.getCraftingHelper().getMissingIngredientMask();
            Container inputs = tile.getCraftingHelper().getCraftingInventory();
            drawPlanIngredientsOverlay(graphics, inputs, missingMask, x + 48, y + 18);
        }
    }

    @Override
    public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {

        if (isShiftDown) {
            drawPlanOutputsOverlay(graphics, getFrame().x(), getFrame().y());
        }
    }

    private void drawPlanIngredientsOverlay(GuiGraphics graphics, Container ingredients, int missingMask, int xPos, int yPos) {
        for (int y = 0; y < 3; y++) {
            for (int x = 0; x < 3; x++) {
                int drawPosX = xPos + (x * 18);
                int drawPosY = yPos + (y * 18);

                int index = (y * 3) + x;
                ItemStack ingredient = ingredients.getItem(index);
                int colour = (missingMask & 1 << index) != 0 ? EnumColour.RED.argb(0x77) : EnumColour.GRAY.argb(0x77);

                if (!ingredient.isEmpty()) {
                    graphics.fillGradient(drawPosX, drawPosY, drawPosX + 16, drawPosY + 16, colour, colour);
                    graphics.renderItem(ingredient, drawPosX, drawPosY);
                }
            }
        }
    }

    private void drawPlanOutputsOverlay(GuiGraphics graphics, int xPos, int yPos) {

        for (Slot slot : getMenu().slots) {
            ItemStack stack = slot.getItem();
            if (RecipePlanItem.hasRecipeInside(stack)) {

                ItemStack output = RecipePlanItem.loadPlanOutput(stack);
                int colour = EnumColour.LIGHT_BLUE.argb(0xCC);
                graphics.fillGradient(xPos + slot.x, yPos + slot.y, xPos + slot.x + 16, yPos + slot.y + 16, colour, colour);

                // Render item at Z +200
                graphics.renderItem(output, xPos + slot.x, yPos + slot.y, 0, 200);
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
