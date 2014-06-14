package mrtjp.projectred.core.libmc.gui

import org.lwjgl.opengl.GL11
import net.minecraft.client.renderer.Tessellator
import mrtjp.projectred.core.libmc.ResourceLib
import cpw.mods.fml.client.FMLClientHandler
import net.minecraft.util.MathHelper
import java.awt.Color
import codechicken.lib.gui.GuiDraw

object GuiLib
{
    /**
     *
     * @param x x pos of grid
     * @param y y pos of grid
     * @param w width of grid
     * @param h height of grid
     * @param dx x spacing of slots (0 means touching like in inventories)
     * @param dy y spacing of slots (0 means touching like in inventories)
     * @return Sequence of x and
     */
    def createSlotGrid(x:Int, y:Int, w:Int, h:Int, dx:Int, dy:Int):Seq[(Int, Int)] =
        createGrid(x, y, w, h, dx+18, dy+18)

    def createGrid(x:Int, y:Int, w:Int, h:Int, dx:Int, dy:Int) =
    {
        var grid = Seq[(Int, Int)]()
        for (iy <- 0 until h) for (ix <- 0 until w)
            grid :+= ((x+ix*dx) -> (y+iy*dy))
        grid
    }

    def drawPlayerInvBackground(x:Int, y:Int)
    {
        for ((x, y) <- createSlotGrid(x, y, 9, 3, 0, 0))
            drawSlotBackground(x-1, y-1)
        for ((x, y) <- createSlotGrid(x, y+58, 9, 1, 0, 0))
            drawSlotBackground(x-1, y-1)
    }

    def drawSlotBackground(x:Int, y:Int)
    {
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        ResourceLib.guiSlot.bind()
        val t = Tessellator.instance
        t.startDrawingQuads()
        t.addVertexWithUV(x, y+18, 0, 0, 1)
        t.addVertexWithUV(x+18, y+18, 0, 1, 1)
        t.addVertexWithUV(x+18, y, 0, 1, 0)
        t.addVertexWithUV(x, y, 0, 0, 0)
        t.draw()
    }

    def drawGuiBox(x:Int, y:Int, width:Int, height:Int, zLevel:Float)
    {
        drawGuiBox(x, y, width, height, zLevel, true, true, true, true)
    }

    def drawGuiBox(x:Int, y:Int, width:Int, height:Int, zLevel:Float, top:Boolean, left:Boolean, bottom:Boolean, right:Boolean)
    {
        val u = 1
        val v = 29
        ResourceLib.guiExtras.bind()
        GuiDraw.gui.setZLevel(zLevel)
        GL11.glPushMatrix()
        GL11.glTranslated(x+2, y+2, 0)
        GL11.glScaled(width-4, height-4, 0)
        GuiDraw.drawTexturedModalRect(0, 0, u+19, v, 1, 1)
        GL11.glPopMatrix()
        if (top)
        {
            GL11.glPushMatrix()
            GL11.glTranslated(x+3, y, 0)
            GL11.glScaled(width-6, 1, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+4, v, 1, 3)
            GL11.glPopMatrix()
        }
        if (bottom)
        {
            GL11.glPushMatrix()
            GL11.glTranslated(x+3, y+height-3, 0)
            GL11.glScaled(width-6, 1, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+14, v, 1, 3)
            GL11.glPopMatrix()
        }
        if (left)
        {
            GL11.glPushMatrix()
            GL11.glTranslated(x, y+3, 0)
            GL11.glScaled(1, height-6, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u, v+4, 3, 1)
            GL11.glPopMatrix()
        }
        if (right)
        {
            GL11.glPushMatrix()
            GL11.glTranslated(x+width-3, y+3, 0)
            GL11.glScaled(1, height-6, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+8, v, 3, 1)
            GL11.glPopMatrix()
        }

        if (top && left) GuiDraw.drawTexturedModalRect(x, y, u, v, 4, 4)
        if (top && right) GuiDraw.drawTexturedModalRect(x+width-3, y, u+5, v, 3, 3)
        if (bottom && left) GuiDraw.drawTexturedModalRect(x, y+height-3, u+11, v, 3, 3)
        if (bottom && right) GuiDraw.drawTexturedModalRect(x+width-4, y+height-4, u+15, v, 4, 4)
    }

    def drawLine(x:Double, y:Double, x2:Double, y2:Double)
    {
        val count = FMLClientHandler.instance.getClient.thePlayer.ticksExisted
        val red = 0.7F+MathHelper.sin(((count+x)/10.0D).asInstanceOf[Float])*0.15F+0.15F
        val green = 0.0F+MathHelper.sin(((count+x+y)/11.0D).asInstanceOf[Float])*0.15F+0.15F
        val blue = 0.0F+MathHelper.sin(((count+y)/12.0D).asInstanceOf[Float])*0.15F+0.15F
        drawLine(x, y, x2, y2, new Color(red, green*0, blue*0).getRGB)
    }

    def drawLine(x:Double, y:Double, x2:Double, y2:Double, color:Int)
    {
        val count = FMLClientHandler.instance.getClient.thePlayer.ticksExisted
        val alpha = 0.3F+MathHelper.sin((count+x).asInstanceOf[Float])*0.3F+0.3F
        val c = new Color(color)
        val red = c.getRed/255.0F
        val green = c.getGreen/255.0F
        val blue = c.getBlue/255.0F
        val var12 = Tessellator.instance

        GL11.glPushMatrix()
        GL11.glLineWidth(3.0F)
        GL11.glDisable(3553)
        GL11.glBlendFunc(770, 1)
        var12.startDrawing(3)
        var12.setColorRGBA_F(red, green, blue, alpha)
        var12.addVertex(x, y, 0.0D)
        var12.addVertex(x2, y2, 0.0D)
        var12.draw()
        GL11.glBlendFunc(770, 771)
        GL11.glDisable(32826)
        GL11.glEnable(3553)
        GL11.glPopMatrix()
    }
}
