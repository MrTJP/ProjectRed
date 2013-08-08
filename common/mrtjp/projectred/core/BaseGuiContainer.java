package mrtjp.projectred.core;

import net.minecraft.client.gui.inventory.GuiContainer;
import net.minecraft.inventory.Container;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class BaseGuiContainer extends GuiContainer {
    public Container container;
    private String texPath;
    protected int right;
    protected int bottom;
    protected int xCenter;
    protected int yCenter;
    protected final int xCenterOffset;
    protected final int yCenterOffset;

    public BaseGuiContainer(Container container, int xSize, int ySize, String texPath) {
        super(container);
        this.container = container;
        this.xSize = xSize;
        this.ySize = ySize;
        this.texPath = texPath;
        this.xCenterOffset = 0;
        this.yCenterOffset = 0;
    }

    public BaseGuiContainer(Container container, int xSize, int ySize) {
        super(container);
        this.container = container;
        this.xSize = xSize;
        this.ySize = ySize;
        this.texPath = "/mods/projectred/textures/gui/GuiBackground.png";
        this.xCenterOffset = 0;
        this.yCenterOffset = 0;
    }

    public BaseGuiContainer(int xSize, int ySize, int xCenterOffset, int yCenterOffset) {
        this(new GhostContainer(null, null), xSize, ySize);
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
    }

    @Override
    protected void drawGuiContainerBackgroundLayer(float var1, int var2, int var3) {
        mc.renderEngine.func_110577_a(new ResourceLocation(texPath));
        GL11.glColor3f(1, 1, 1);
        drawTexturedModalRect(guiLeft, guiTop, 0, 0, xSize, ySize);
    }

    protected void drawString(String s, int x, int y, int colour) {
        fontRenderer.drawStringWithShadow(s, x + guiLeft, y + guiTop, colour);
    }

    protected void drawStringWithoutShadow(String s, int x, int y, int colour) {
        fontRenderer.drawString(s, x + guiLeft, y + guiTop, colour);
    }
}