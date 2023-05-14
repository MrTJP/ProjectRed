package mrtjp.projectred.core.gui.screen.inventory;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.core.inventory.container.ElectrotineGeneratorContainer;
import mrtjp.projectred.lib.GuiLib;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.client.gui.IHasContainer;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;

import static mrtjp.projectred.ProjectRedCore.MOD_ID;

public class ElectrotineGeneratorScreen extends RedUIContainerScreen<ElectrotineGeneratorContainer> implements IHasContainer<ElectrotineGeneratorContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/electrotine_generator.png");

    public ElectrotineGeneratorScreen(ElectrotineGeneratorContainer container, PlayerInventory playerInventory, ITextComponent title) {
        super(176, 171, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 79;
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        super.drawBack(stack, mouse, partialFrame);

        TextureUtils.changeTexture(BACKGROUND);

        int x = getFrame().x();
        int y = getFrame().y();
        // Background
        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());

        // Charge icon
        if (getMenu().canConductorWork()) {
            blit(stack, x + 22, y + 16, 176, 1, 7, 9);
        }
        // Charge Bar
        GuiLib.drawVerticalTank(stack, this, x + 22, y + 26, 176, 10, 7, 48, getMenu().getChargeScaled(48));

        // Storage icon
        if (getMenu().isPowerStorageFull()) {
            blit(stack, x + 54, y + 16, 184, 1, 14, 9);
        }
        // Storage Bar
        GuiLib.drawVerticalTank(stack, this, x + 54, y + 26, 184, 10, 14, 48, getMenu().getPowerStoredScaled(48));

        // Burning icon
        if (getMenu().isBurning()) {
            blit(stack, x + 93, y + 16, 199, 1, 7, 9);
        }
        // Burning Bar
        GuiLib.drawVerticalTank(stack, this, x + 93, y + 26, 199, 10, 7, 48, getMenu().getBurnTimeScaled(48));

        // Storage charging arrow
        if (getMenu().isChargingStorage()) {
            blit(stack, x + 69, y + 45, 211, 10, 23, 9);
        }

        // Conductor charging arrow
        if (getMenu().isChargingConductor()) {
            blit(stack, x + 30, y + 46, 211, 0, 23, 9);
        }
//
//        // Title labels
//        getFontRenderer().draw(stack, title, x + 8, y + 6, EnumColour.GRAY.argb());
//        getFontRenderer().draw(stack, inventory.getDisplayName(), x + 8, y + 79, EnumColour.GRAY.argb());
    }
}
