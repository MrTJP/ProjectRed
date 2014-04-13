package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.integration.GateLogic.ICounterGuiLogic;
import codechicken.core.gui.GuiCCButton;
import codechicken.core.gui.GuiScreenWidget;
import codechicken.lib.packet.PacketCustom;

public class GuiCounter extends GuiScreenWidget
{
    public ICounterGuiLogic logic;
    public GatePart part;

    public GuiCounter(GatePart part)
    {
        this.part = part;
        logic = (ICounterGuiLogic) part.getLogic();
    }

    @Override
    public void initGui()
    {
        xSize = 256;
        ySize = 145;
        super.initGui();
    }

    @Override
    public void addWidgets()
    {
        for (int row = 0; row < 3; row++)
        {
            int y = 16 + 40 * row;
            add(new GuiCCButton(5, y, 40, 20, "-10").setActionCommand(row + "-10"));
            add(new GuiCCButton(46, y, 40, 20, "-5").setActionCommand(row + "-5"));
            add(new GuiCCButton(87, y, 40, 20, "-1").setActionCommand(row + "-1"));
            add(new GuiCCButton(129, y, 40, 20, "+1").setActionCommand(row + "+1"));
            add(new GuiCCButton(170, y, 40, 20, "+5").setActionCommand(row + "+5"));
            add(new GuiCCButton(211, y, 40, 20, "+10").setActionCommand(row + "+10"));
        }
    }

    @Override
    public void drawScreen(int mousex, int mousey, float f)
    {
        drawDefaultBackground();
        super.drawScreen(mousex, mousey, f);
    }

    @Override
    public void drawBackground()
    {
        BasicGuiUtils.drawGuiBox(0, 0, xSize, ySize, zLevel);
        String s = "Maximum: " + logic.getCounterMax();
        fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 5, 0x404040);
        s = "Increment: " + logic.getCounterIncr();
        fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 45, 0x404040);
        s = "Decrement: " + logic.getCounterDecr();
        fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 85, 0x404040);
        s = "State: " + logic.getCounterValue();
        fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 125, 0x404040);
    }

    @Override
    public boolean doesGuiPauseGame()
    {
        return false;
    }

    @Override
    public void updateScreen()
    {
        super.updateScreen();

        if (part.tile() == null)
            mc.thePlayer.closeScreen();
    }

    @Override
    public void actionPerformed(String ident, Object... params)
    {
        int id = Integer.parseInt(ident.substring(0, 1));
        ident = ident.substring(1);
        if (ident.startsWith("+"))
            ident = ident.substring(1);
        int value = Integer.parseInt(ident);

        PacketCustom packet = new PacketCustom(IntegrationCPH.channel(), 2);
        IntegrationCPH.writePartIndex(packet, part);
        packet.writeByte(id);
        packet.writeShort(value);
        packet.sendToServer();
    }
}
