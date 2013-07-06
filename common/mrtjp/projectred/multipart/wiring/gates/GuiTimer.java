package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.utils.BasicGuiUtils;
import mrtjp.projectred.utils.gui.BaseContainer;
import mrtjp.projectred.utils.gui.BaseGuiContainer;
import net.minecraft.client.gui.GuiButton;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GuiTimer extends BaseGuiContainer {

	public GuiTimer(ContainerTimer container) {
		super(container, 256, 55);
	}
	
	@Override
	public void initGui() {
		super.initGui();
		buttonList.add(new GuiButton(0, guiLeft +   5, guiTop + 25, 40, 20, "-10s"));
		buttonList.add(new GuiButton(1, guiLeft +  46, guiTop + 25, 40, 20, "-1s"));
		buttonList.add(new GuiButton(2, guiLeft +  87, guiTop + 25, 40, 20, "-50ms"));
		buttonList.add(new GuiButton(3, guiLeft + 129, guiTop + 25, 40, 20, "+50ms"));
		buttonList.add(new GuiButton(4, guiLeft + 170, guiTop + 25, 40, 20, "+1s"));
		buttonList.add(new GuiButton(5, guiLeft + 211, guiTop + 25, 40, 20, "+10s"));
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
        BasicGuiUtils.drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, true);
        
        String s = "Timer interval: "+String.format("%.2f", ((ContainerTimer)container).intervalTicks * 0.05)+"s";
        
        int name_w = fontRenderer.getStringWidth(s);
        fontRenderer.drawString(s, guiLeft + (xSize - name_w) / 2, guiTop + 8, 0x404040);
	}
	
	@Override
	protected void actionPerformed(GuiButton par1GuiButton) {
		((BaseContainer) container).sendButtonPressed(par1GuiButton.id);
	}

}
