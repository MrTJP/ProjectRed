package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.utils.BasicGuiUtils;
import mrtjp.projectred.utils.gui.BaseContainer;
import mrtjp.projectred.utils.gui.BaseGuiContainer;
import net.minecraft.client.gui.GuiButton;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class GuiCounter extends BaseGuiContainer {

	public GuiCounter(ContainerCounter par1Container) {
		super(par1Container, 256, 145);
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public void initGui() {
		super.initGui();
		for(int row = 0; row < 3; row++) {
			int y = guiTop + 16 + 40*row;
			int k = row * 8;
			buttonList.add(new GuiButton(k+0, guiLeft +   5, y, 40, 20, "-10"));
			buttonList.add(new GuiButton(k+1, guiLeft +  46, y, 40, 20, "-5"));
			buttonList.add(new GuiButton(k+2, guiLeft +  87, y, 40, 20, "-1"));
			buttonList.add(new GuiButton(k+3, guiLeft + 129, y, 40, 20, "+1"));
			buttonList.add(new GuiButton(k+4, guiLeft + 170, y, 40, 20, "+5"));
			buttonList.add(new GuiButton(k+5, guiLeft + 211, y, 40, 20, "+10"));
		}
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
		//super.drawGuiContainerBackgroundLayer(var1, var2, var3);
        BasicGuiUtils.drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, true);
        ContainerCounter container = (ContainerCounter)inventorySlots;
        
        String s = "Maximum: "+container.max;
        fontRenderer.drawString(s, guiLeft + (xSize - fontRenderer.getStringWidth(s)) / 2, guiTop + 5, 0x404040);
        s = "Increment by: "+container.incr;
        fontRenderer.drawString(s, guiLeft + (xSize - fontRenderer.getStringWidth(s)) / 2, guiTop + 45, 0x404040);
        s = "Decrement by: "+container.decr;
        fontRenderer.drawString(s, guiLeft + (xSize - fontRenderer.getStringWidth(s)) / 2, guiTop + 85, 0x404040);
        s = "Current count: "+container.value;
        fontRenderer.drawString(s, guiLeft + (xSize - fontRenderer.getStringWidth(s)) / 2, guiTop + 125, 0x404040);
	}
	
	@Override
	protected void actionPerformed(GuiButton par1GuiButton) {
		((BaseContainer) container).sendButtonPressed(par1GuiButton.id);
	}

}
