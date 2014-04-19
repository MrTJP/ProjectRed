package mrtjp.projectred.core.libmc;

import codechicken.core.gui.GuiDraw;
import codechicken.lib.render.CCRenderState;
import cpw.mods.fml.client.FMLClientHandler;
import mrtjp.projectred.core.lib.Pair2;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.util.MathHelper;
import net.minecraft.util.ResourceLocation;
import org.lwjgl.opengl.GL11;

import java.awt.*;
import java.util.ArrayList;

public class BasicGuiUtils
{

    public static void drawPlayerInventoryBackground(Minecraft mc, int xOffset, int yOffset)
    {
        // Player "backpack"
        for (int row = 0; row < 3; row++)
            for (int column = 0; column < 9; column++)
                drawSlotBackground(mc, xOffset + column * 18 - 1, yOffset + row * 18 - 1);
        // Player "hotbar"
        for (int i1 = 0; i1 < 9; i1++)
            drawSlotBackground(mc, xOffset + i1 * 18 - 1, yOffset + 58 - 1);
    }

    public static void drawPlayerHotbarBackground(Minecraft mc, int xOffset, int yOffset)
    {
        // Player "hotbar"
        for (int i1 = 0; i1 < 9; i1++)
            drawSlotBackground(mc, xOffset + i1 * 18 - 1, yOffset - 1);
    }

    public static void drawSlotBackground(Minecraft mc, int x, int y)
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/slot.png"));

        Tessellator var9 = Tessellator.instance;
        var9.startDrawingQuads();
        var9.addVertexWithUV(x, y + 18, 0, 0, 1);
        var9.addVertexWithUV(x + 18, y + 18, 0, 1, 1);
        var9.addVertexWithUV(x + 18, y, 0, 1, 0);
        var9.addVertexWithUV(x, y, 0, 0, 0);
        var9.draw();
    }

    public static void drawGuiBox(int x, int y, int width, int height, float zLevel)
    {
        drawGuiBox(x, y, width, height, zLevel, true, true, true, true);
    }

    private static ResourceLocation guiBack = new ResourceLocation("projectred", "textures/gui/guibackground.png");
    private static ResourceLocation guiExtras = new ResourceLocation("projectred:textures/gui/guiextras.png");

    public static void drawGuiBox(int x, int y, int width, int height, float zLevel, boolean top, boolean left, boolean bottom, boolean right)
    {
        int u = 1;
        int v = 29;
        CCRenderState.changeTexture(guiExtras);
        GuiDraw.gui.setZLevel(zLevel);

        //renderBackground
        GL11.glPushMatrix();
        GL11.glTranslated(x+2, y+2, 0);
        GL11.glScaled(width-4, height-4, 0);
        GuiDraw.drawTexturedModalRect(0, 0, u+19, v, 1, 1);
        GL11.glPopMatrix();

        if (top)
        {
            GL11.glPushMatrix();
            GL11.glTranslated(x+3, y, 0);
            GL11.glScaled(width-6, 1, 0);
            GuiDraw.drawTexturedModalRect(0, 0, u+4, v, 1, 3);
            GL11.glPopMatrix();
        }

        if (bottom)
        {
            GL11.glPushMatrix();
            GL11.glTranslated(x+3, y+height-3, 0);
            GL11.glScaled(width-6, 1, 0);
            GuiDraw.drawTexturedModalRect(0, 0, u+14, v, 1, 3);
            GL11.glPopMatrix();
        }

        if (left)
        {
            GL11.glPushMatrix();
            GL11.glTranslated(x, y+3, 0);
            GL11.glScaled(1, height-6, 0);
            GuiDraw.drawTexturedModalRect(0, 0, u, v+4, 3, 1);
            GL11.glPopMatrix();
        }

        if (right)
        {
            GL11.glPushMatrix();
            GL11.glTranslated(x+width-3, y+3, 0);
            GL11.glScaled(1, height-6, 0);
            GuiDraw.drawTexturedModalRect(0, 0, u+8, v, 3, 1);
            GL11.glPopMatrix();
        }

        if (top && left)
            GuiDraw.drawTexturedModalRect(x, y, u, v, 4, 4);
        if (top && right)
            GuiDraw.drawTexturedModalRect(x+width-3, y, u+5, v, 3, 3);
        if (bottom && left)
            GuiDraw.drawTexturedModalRect(x, y+height-3, u+11, v, 3, 3);
        if (bottom && right)
            GuiDraw.drawTexturedModalRect(x+width-4, y+height-4, u+15, v, 4, 4);
    }

    /**
     * Create positions for a grid of slots.
     *
     * @param x x staring position
     * @param y y starting position
     * @param xSize x size of grid
     * @param ySize y size of grid
     * @param xSpacing spacing between slots on x axis
     * @param ySpacing spacing between slots on y axis
     * @return
     */
    public static ArrayList<Pair2<Integer, Integer>> createSlotArray(int x, int y, int xSize, int ySize, int xSpacing, int ySpacing)
    {
        return createGridArray(x, y, xSize, ySize, xSpacing+18, ySpacing+18);
    }

    public static ArrayList<Pair2<Integer, Integer>> createGridArray(int x, int y, int xSize, int ySize, int xSpacing, int ySpacing)
    {
        ArrayList<Pair2<Integer, Integer>> list = new ArrayList<Pair2<Integer, Integer>>(xSize * ySize);

        for (int i = 0; i < ySize; i++)
            for (int j = 0; j < xSize; j++)
            {
                int xPos = x + j * xSpacing;
                int yPos = y + i * ySpacing;
                list.add(new Pair2<Integer, Integer>(xPos, yPos));
            }

        return list;
    }

    public static void drawLine(double x, double y, double x2, double y2)
    {
        int count = FMLClientHandler.instance().getClient().thePlayer.ticksExisted;
        float red = 0.7F+MathHelper.sin((float)((count+x)/10.0D))*0.15F+0.15F;
        float green = 0.0F+MathHelper.sin((float)((count+x+y)/11.0D))*0.15F+0.15F;
        float blue = 0.0F+MathHelper.sin((float)((count+y)/12.0D))*0.15F+0.15F;
        drawLine(x, y, x2, y2, new Color(red, green*0, blue*0).getRGB());
    }

    public static void drawLine(double x, double y, double x2, double y2, int color)
    {
        int count = FMLClientHandler.instance().getClient().thePlayer.ticksExisted;
        float alpha = 0.3F+MathHelper.sin((float)(count+x))*0.3F+0.3F;

        Color c = new Color(color);
        float red = c.getRed()/255.0F;
        float green = c.getGreen()/255.0F;
        float blue = c.getBlue()/255.0F;



        Tessellator var12 = Tessellator.instance;
        GL11.glPushMatrix();
        GL11.glLineWidth(3.0F);
        GL11.glDisable(3553);

        GL11.glBlendFunc(770, 1);
        var12.startDrawing(3);

        var12.setColorRGBA_F(red, green, blue, alpha);
        var12.addVertex(x, y, 0.0D);
        var12.addVertex(x2, y2, 0.0D);

        var12.draw();
        GL11.glBlendFunc(770, 771);
        GL11.glDisable(32826);
        GL11.glEnable(3553);
        GL11.glPopMatrix();
    }
}
