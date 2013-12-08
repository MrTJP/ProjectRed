package mrtjp.projectred.transportation;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import net.minecraft.inventory.Container;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;
import codechicken.lib.vec.BlockCoord;

public class GuiCraftingPipe extends GhostGuiContainer {
    RoutedCraftingPipePart pipe;

    public GuiCraftingPipe(Container container, RoutedCraftingPipePart pipe) {
        super(container, null, 176, 200);
        this.pipe = pipe;
    }

    @Override
    public void actionPerformed(String ident, Object... params) {
        PacketCustom packet = new PacketCustom(TransportationCPH.channel, NetConstants.gui_CraftingPipe_action);
        packet.writeCoord(new BlockCoord(pipe.tile()));
        packet.writeString(ident);
        packet.sendToServer();
    }

    @Override
    public void addWidgets() {
        add(new WidgetSimpleButton(142, 37, 20, 14).setText("+").setActionCommand("up"));
        add(new WidgetSimpleButton(142, 67, 20, 14).setText("-").setActionCommand("down"));
    }

    @Override
    public void drawBackground() {
        CCRenderState.changeTexture("projectred:textures/gui/guicraftingpipe.png");
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);

        FontUtils.drawCenteredString(""+pipe.priority, 152, 55, PRColors.BLACK.rgb);
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 118);
    }

    @Override
    public void drawForeground() {

    }
}
