package mrtjp.projectred.integration;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.utils.BasicGuiUtils;
import net.minecraft.inventory.Container;
import codechicken.core.gui.GuiCCButton;
import codechicken.core.gui.GuiScreenWidget;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;

public class GuiTimer extends GuiScreenWidget {

	int timerInterval = 4;
	BlockCoord coords;
	int face;

	public GuiTimer() {
		super();
	}

	@Override
	public void initGui() {
		xSize = 256;
		ySize = 55;
		super.initGui();
	}

	@Override
	public void addWidgets() {
		add(new GuiCCButton(5, 25, 40, 20, "-10s").setActionCommand("-200"));
		add(new GuiCCButton(46, 25, 40, 20, "-1s").setActionCommand("-20"));
		add(new GuiCCButton(87, 25, 40, 20, "-50ms").setActionCommand("-1"));
		add(new GuiCCButton(129, 25, 40, 20, "+50ms").setActionCommand("+1"));
		add(new GuiCCButton(170, 25, 40, 20, "+1s").setActionCommand("+20"));
		add(new GuiCCButton(211, 25, 40, 20, "+10s").setActionCommand("+200"));
	}

	@Override
	public void drawScreen(int mousex, int mousey, float f) {
		drawDefaultBackground();
		super.drawScreen(mousex, mousey, f);
	}

	@Override
	public void drawBackground() {
		BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, true);
		String s = "Timer interval: " + String.format("%.2f", timerInterval * 0.05) + "s";
		int name_w = fontRenderer.getStringWidth(s);
		fontRenderer.drawString(s, (xSize - name_w) / 2, 8, 0x404040);
	}

	@Override
	public boolean doesGuiPauseGame() {
		return false;
	}

	public void actionPerformed(String ident, Object... params) {
		PacketCustom packet = new PacketCustom(Configurator.integrationPacketChannel, IntegrationNetworkConstants.guiGateButtonPressed);
		packet.writeCoord(coords);
		packet.writeByte(face);
		packet.writeString(ident);
		packet.sendToServer();
	}
}
