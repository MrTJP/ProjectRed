package mrtjp.projectred.transportation;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.GhostContainer2.ISlotController;
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.utils.Pair2;
import net.minecraft.inventory.Container;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;
import codechicken.lib.vec.BlockCoord;

public class GuiCraftingPipe extends GhostGuiContainer
{
    RoutedCraftingPipePart pipe;

    public GuiCraftingPipe(Container container, RoutedCraftingPipePart pipe)
    {
        super(container, null, 176, 220);
        this.pipe = pipe;
    }

    @Override
    public void actionPerformed(String ident, Object... params)
    {
        PacketCustom packet = new PacketCustom(TransportationCPH.channel, NetConstants.gui_CraftingPipe_action);
        packet.writeCoord(new BlockCoord(pipe.tile()));
        packet.writeString(ident);
        packet.sendToServer();
    }

    @Override
    public void addWidgets()
    {
        add(new WidgetSimpleButton(122, 76, 20, 14).setText("+").setActionCommand("up"));
        add(new WidgetSimpleButton(76, 76, 20, 14).setText("-").setActionCommand("down"));
    }

    @Override
    public void drawBackground()
    {
        CCRenderState.changeTexture("projectred:textures/gui/guicraftingpipe.png");
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);

        FontUtils.drawCenteredString("" + pipe.priority, 110, 79, PRColors.BLACK.rgb);
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 138);
        
        CCRenderState.changeTexture(RL_extras);
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(8, 46, 9, 1, 0, 0))
            drawTexturedModalRect(p.getValue1(), p.getValue2(), 1, 11, 16, 16);
    }
    
    @Override
    public void drawForeground()
    {

    }
}
