package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.integration.GateLogic.ITimerGuiLogic;
import codechicken.core.gui.GuiCCButton;
import codechicken.core.gui.GuiScreenWidget;
import codechicken.lib.packet.PacketCustom;

public class GuiTimer extends GuiScreenWidget
{
    public ITimerGuiLogic logic;
    public GatePart part;

    public GuiTimer(GatePart part)
    {
        this.part = part;
        logic = (ITimerGuiLogic) part.getLogic();
    }

    @Override
    public void initGui()
    {
        xSize = 256;
        ySize = 55;
        super.initGui();
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
        BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, true);
        String s = "Timer interval: " + String.format("%.2f", logic.getTimerMax() * 0.05) + "s";
        int name_w = fontRenderer.getStringWidth(s);
        fontRenderer.drawString(s, (xSize - name_w) / 2, 8, 0x404040);
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

        PacketCustom packet = new PacketCustom(IntegrationCPH.channel, 1);
        IntegrationSPH.writePartIndex(packet, part);
        packet.writeShort(value);
        packet.sendToServer();
    }
}
