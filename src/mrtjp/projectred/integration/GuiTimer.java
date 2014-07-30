package mrtjp.projectred.integration;

import codechicken.core.gui.GuiCCButton;
import codechicken.core.gui.GuiScreenWidget;
import codechicken.lib.packet.PacketCustom;
import mrtjp.projectred.core.libmc.gui.GuiLib;
import mrtjp.projectred.integration.GateLogic.ITimerGuiLogic;

public class GuiTimer extends GuiScreenWidget
{
    public ITimerGuiLogic logic;
    public GatePart part;

    public GuiTimer(GatePart part)
    {
        super(256, 55);
        this.part = part;
        logic = (ITimerGuiLogic)part.getLogic();
    }

    @Override
    public void addWidgets()
    {
        add(new GuiCCButton(5, 25, 40, 20, "-10s").setActionCommand("-200"));
        add(new GuiCCButton(46, 25, 40, 20, "-1s").setActionCommand("-20"));
        add(new GuiCCButton(87, 25, 40, 20, "-50ms").setActionCommand("-1"));
        add(new GuiCCButton(129, 25, 40, 20, "+50ms").setActionCommand("+1"));
        add(new GuiCCButton(170, 25, 40, 20, "+1s").setActionCommand("+20"));
        add(new GuiCCButton(211, 25, 40, 20, "+10s").setActionCommand("+200"));
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
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel);
        String s = "Timer interval: "+String.format("%.2f", logic.getTimerMax()*0.05)+"s";
        int name_w = fontRendererObj.getStringWidth(s);
        fontRendererObj.drawString(s, (xSize-name_w)/2, 8, 0x404040);
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
        if (ident.startsWith("+"))
            ident = ident.substring(1);
        int value = Integer.parseInt(ident);

        PacketCustom packet = new PacketCustom(IntegrationCPH.channel(), 1);
        IntegrationCPH.writePartIndex(packet, part);
        packet.writeShort(value);
        packet.sendToServer();
    }
}
