package mrtjp.projectred.transportation;

import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.FontUtils;
import codechicken.lib.vec.BlockCoord;
import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.inventory.GhostGuiContainer;
import mrtjp.projectred.core.inventory.WidgetButton.WidgetSimpleButton;
import mrtjp.projectred.core.utils.Pair2;
import net.minecraft.inventory.Container;
import org.lwjgl.opengl.GL11;

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
        add(new WidgetSimpleButton(138, 12, 20, 14).setText("+").setActionCommand("up"));
        add(new WidgetSimpleButton(92, 12, 20, 14).setText("-").setActionCommand("down"));
    }

    @Override
    public void drawBackground()
    {
        CCRenderState.changeTexture("projectred:textures/gui/guicraftingpipe.png");
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize);

        FontUtils.drawCenteredString("" + pipe.priority, 126, 15, PRColors.BLACK.rgb);
        BasicGuiUtils.drawPlayerInventoryBackground(mc, 8, 138);

        int color = 0;
        CCRenderState.changeTexture(RL_extras);
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(8, 108, 9, 1, 0, 0))
        {
            GL11.glColor4f(1, 1, 1, 1);
            drawTexturedModalRect(p.getValue1(), p.getValue2(), 1, 11, 16, 16);

            int x = p.getValue1() + 4;
            int y = p.getValue2() - 2;
            drawRect(x, y, x + 8, y + 2, PRColors.get(color++).rgba);
        }
    }

    @Override
    public void drawForeground()
    {
        CCRenderState.changeTexture("projectred:textures/gui/guicraftingpipe.png");

        float oldZ = zLevel;
        zLevel = 300;

        int i = 0;
        for (Pair2<Integer, Integer> p : BasicGuiUtils.createSlotArray(20, 12, 2, 4, 20, 0))
        {
            int x = p.getValue1()-5;
            int y = p.getValue2()-2;

            int u = 178;
            int v = inventorySlots.getSlot(i++).getStack() == null ? 107 : 85;

            drawTexturedModalRect(x, y, u, v, 25, 20);
        }

        zLevel = oldZ;
    }
}
