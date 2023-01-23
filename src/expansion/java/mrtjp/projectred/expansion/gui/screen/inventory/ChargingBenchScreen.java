package mrtjp.projectred.expansion.gui.screen.inventory;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.gui.GuiLib$;
import mrtjp.core.vec.Point;
import mrtjp.projectred.expansion.inventory.container.ChargingBenchContainer;
import mrtjp.projectred.redui.RedUIContainerScreen;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;

import static mrtjp.projectred.ProjectRedExpansion.MOD_ID;

public class ChargingBenchScreen extends RedUIContainerScreen<ChargingBenchContainer> {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(MOD_ID, "textures/gui/charging_bench.png");

    public ChargingBenchScreen(ChargingBenchContainer container, PlayerInventory playerInventory, ITextComponent title) {
        super(176, 183, container, playerInventory, title);

        inventoryLabelX = 8;
        inventoryLabelY = 91;
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        TextureUtils.changeTexture(BACKGROUND);
        int x = getFrame().x();
        int y = getFrame().y();

        blit(stack, x, y, 0, 0, getFrame().width(), getFrame().height());

        if (getMenu().canConductorWork())
            blit(stack, x + 14, y + 17, 176, 1, 7, 9);
        GuiLib$.MODULE$.drawVerticalTank(stack, this, x + 14, y + 27, 176, 10, 7, 48, getMenu().getChargeScaled(48));

        if (getMenu().isPowerStorageFull())
            blit(stack, x + 112, y + 16, 184, 1, 14, 9);
        GuiLib$.MODULE$.drawVerticalTank(stack, this, x + 41, y + 27, 184, 10, 14, 48, getMenu().getPowerStoredScaled(48));

        if (getMenu().isStorageCharging())
            blit(stack, x + 26, y + 48, 199, 0, 10, 8);

        if (getMenu().areItemsCharging())
            blit(stack, x + 63, y + 29, 210, 0, 17, 10);
    }
}
