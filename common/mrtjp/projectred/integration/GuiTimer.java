package mrtjp.projectred.integration;

import mrtjp.projectred.utils.BasicGuiUtils;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.inventory.Container;

public class GuiTimer extends GuiMultipart {

	public GuiTimer(Container container) {
		super(container, 256, 55);
	}

	@Override
	public void addElementsToGui() {
		buttonList.add(new GuiButton(0, guiLeft + 5, guiTop + 25, 40, 20, "-10s"));
		buttonList.add(new GuiButton(1, guiLeft + 46, guiTop + 25, 40, 20, "-1s"));
		buttonList.add(new GuiButton(2, guiLeft + 87, guiTop + 25, 40, 20, "-50ms"));
		buttonList.add(new GuiButton(3, guiLeft + 129, guiTop + 25, 40, 20, "+50ms"));
		buttonList.add(new GuiButton(4, guiLeft + 170, guiTop + 25, 40, 20, "+1s"));
		buttonList.add(new GuiButton(5, guiLeft + 211, guiTop + 25, 40, 20, "+10s"));
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
		BasicGuiUtils.drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, true);
		String s = "Timer interval: " + String.format("%.2f", ((ContainerTimer) container).timerInterval * 0.05) + "s";
		int name_w = fontRenderer.getStringWidth(s);
		fontRenderer.drawString(s, guiLeft + (xSize - name_w) / 2, guiTop + 8, 0x404040);
	}

}
