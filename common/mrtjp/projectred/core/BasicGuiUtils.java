package mrtjp.projectred.core;

import java.util.ArrayList;

import mrtjp.projectred.core.utils.Pair2;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.opengl.GL11;

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

    public static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag)
    {
        drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, flag, true, true, true, true);
    }

    private static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag, boolean displayTop, boolean displayLeft, boolean displayBottom, boolean displayRight)
    {
        if (flag)
            GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/guibackground.png"));

        if (displayTop)
        {
            // Top Side
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
            var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
            var9.addVertexWithUV(right - 15, guiTop, zLevel, 0.66, 0);
            var9.addVertexWithUV(guiLeft + 15, guiTop, zLevel, 0.33, 0);
            var9.draw();
        }

        if (displayLeft)
        {
            // Left Side
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(guiLeft, bottom - 15, zLevel, 0, 0.66);
            var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
            var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
            var9.addVertexWithUV(guiLeft, guiTop + 15, zLevel, 0, 0.33);
            var9.draw();
        }

        if (displayBottom)
        {
            // Bottom Side
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(guiLeft + 15, bottom, zLevel, 0.33, 1);
            var9.addVertexWithUV(right - 15, bottom, zLevel, 0.66, 1);
            var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
            var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
            var9.draw();
        }

        if (displayRight)
        {
            // Right Side
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
            var9.addVertexWithUV(right, bottom - 15, zLevel, 1, 0.66);
            var9.addVertexWithUV(right, guiTop + 15, zLevel, 1, 0.33);
            var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
            var9.draw();
        }

        if (displayTop && displayLeft)
        {
            // Top Left
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(guiLeft, guiTop + 15, zLevel, 0, 0.33);
            var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
            var9.addVertexWithUV(guiLeft + 15, guiTop, zLevel, 0.33, 0);
            var9.addVertexWithUV(guiLeft, guiTop, zLevel, 0, 0);
            var9.draw();
        }

        if (displayBottom && displayLeft)
        {
            // Bottom Left
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(guiLeft, bottom, zLevel, 0, 1);
            var9.addVertexWithUV(guiLeft + 15, bottom, zLevel, 0.33, 1);
            var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
            var9.addVertexWithUV(guiLeft, bottom - 15, zLevel, 0, 0.66);
            var9.draw();
        }

        if (displayBottom && displayRight)
        {
            // Bottom Right
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(right - 15, bottom, zLevel, 0.66, 1);
            var9.addVertexWithUV(right, bottom, zLevel, 1, 1);
            var9.addVertexWithUV(right, bottom - 15, zLevel, 1, 0.66);
            var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
            var9.draw();
        }

        if (displayTop && displayRight)
        {
            // Top Right
            Tessellator var9 = Tessellator.instance;
            var9.startDrawingQuads();
            var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
            var9.addVertexWithUV(right, guiTop + 15, zLevel, 1, 0.33);
            var9.addVertexWithUV(right, guiTop, zLevel, 1, 0);
            var9.addVertexWithUV(right - 15, guiTop, zLevel, 0.66, 0);
            var9.draw();
        }

        // Center
        Tessellator var9 = Tessellator.instance;
        var9.startDrawingQuads();
        var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
        var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
        var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
        var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
        var9.draw();
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
        ArrayList<Pair2<Integer, Integer>> list = new ArrayList<Pair2<Integer, Integer>>(xSize * ySize);
        xSpacing += 18;
        ySpacing += 18;

        for (int i = 0; i < ySize; i++)
            for (int j = 0; j < xSize; j++)
            {
                int slotNumber = i * xSize + j;
                int xPos = x + j * xSpacing;
                int yPos = y + i * ySpacing;
                list.add(new Pair2<Integer, Integer>(xPos, yPos));
            }

        return list;
    }
}
