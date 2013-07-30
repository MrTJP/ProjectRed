package mrtjp.projectred.integration;

import mrtjp.projectred.core.Configurator;
import net.minecraft.client.gui.GuiButton;
import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Container;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

import codechicken.lib.packet.PacketCustom;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public abstract class GuiMultipart extends GuiContainer {
	
	public Container container;
	protected int right;
	protected int bottom;
	protected int xCenter;
	protected int yCenter;
	protected int xCenterOffset;
	protected int yCenterOffset;
	
	public GuiMultipart(Container container, int x, int y) {
		super(container);
		this.container = container;
		this.xSize = x;
		this.ySize = y;
	}
	
	@Override
	public void initGui() {
		super.initGui();
		this.guiLeft = width / 2 - xSize / 2 + xCenterOffset;
		this.guiTop = height / 2 - ySize / 2 + yCenterOffset;
		this.right = width / 2 + xSize / 2 + xCenterOffset;
		this.bottom = height / 2 + ySize / 2 + yCenterOffset;
		this.xCenter = (right + guiLeft) / 2;
		this.yCenter = (bottom + guiTop) / 2;
		addElementsToGui();
	}
	
	/**
	 * Called to add buttons to the gui.
	 */
	public abstract void addElementsToGui();
	
	/**
	 * Sends button packet, container handles what actually happens.
	 */
	@Override
	protected void actionPerformed(GuiButton button) {
		new PacketCustom(Configurator.integrationPacketChannel, IntegrationNetworkConstants.guiButtonPressed).writeByte(button.id).sendToServer();
	}

	/**
	 * Draw the background as standard grey rect using min/max xy values.
	 */
	@Override
	protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/GuiBackground.png"));
		GL11.glColor3f(1, 1, 1);
		drawTexturedModalRect(guiLeft, guiTop, 0, 0, xSize, ySize);
	}
}
