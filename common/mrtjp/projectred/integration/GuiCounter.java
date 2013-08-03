package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicGuiUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.Coords;
import net.minecraft.client.Minecraft;
import codechicken.core.gui.GuiCCButton;
import codechicken.core.gui.GuiScreenWidget;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TileMultipart;

public class GuiCounter extends GuiScreenWidget {

	BlockCoord coords;
	int face;

	int max;
	int incr;
	int decr;
	int value;

	public GuiCounter() {
		super();
	}

	@Override
	public void initGui() {
		xSize = 256;
		ySize = 145;
		super.initGui();
	}

	@Override
	public void addWidgets() {
		for (int row = 0; row < 3; row++) {
			int y = 16 + 40 * row;
			int k = row * 8;
			add(new GuiCCButton(5, y, 40, 20, "-10").setActionCommand(row+"-10"));
			add(new GuiCCButton(46, y, 40, 20, "-5").setActionCommand(row+"-5"));
			add(new GuiCCButton(87, y, 40, 20, "-1").setActionCommand(row+"-1"));
			add(new GuiCCButton(129, y, 40, 20, "+1").setActionCommand(row+"+1"));
			add(new GuiCCButton(170, y, 40, 20, "+5").setActionCommand(row+"+5"));
			add(new GuiCCButton(211, y, 40, 20, "+10").setActionCommand(row+"+10"));
		}
	}

	@Override
	public void drawScreen(int mousex, int mousey, float f) {
		drawDefaultBackground();
		super.drawScreen(mousex, mousey, f);
	}

	@Override
	public void drawBackground() {
		BasicGuiUtils.drawGuiBackGround(mc, 0, 0, xSize, ySize, zLevel, true);
		String s = "Maximum: " + max;
		fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 5, 0x404040);
		s = "Increment by: " + incr;
		fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 45, 0x404040);
		s = "Decrement by: " + decr;
		fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 85, 0x404040);
		s = "Current count: " + value;
		fontRenderer.drawString(s, (xSize - fontRenderer.getStringWidth(s)) / 2, 125, 0x404040);
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
