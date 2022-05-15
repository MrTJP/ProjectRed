/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem
import mrtjp.core.vec.Vec2
import net.minecraft.client.gui.AbstractGui
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.client.renderer.{Tessellator, WorldVertexBufferUploader}
import net.minecraft.util.ResourceLocation
import org.lwjgl.opengl.GL11

/**
  * Provides utility functions useful for drawing GUIs.
  */
object GuiLib
{
    val guiSlot = new ResourceLocation("mrtjpcore", "textures/gui/slot.png")
    val guiExtras = new ResourceLocation("mrtjpcore", "textures/gui/guiextras.png")
    val guiTex = new ResourceLocation("minecraft", "textures/gui/widgets.png")

    /**
     *
     * @param x X pos of grid.
     * @param y Y pos of grid.
     * @param w Width of grid.
     * @param h Height of grid.
     * @param dx X spacing of slots (0 means touching like in inventories).
     * @param dy Y spacing of slots (0 means touching like in inventories).
     * @return Sequence of tuples representing (x, y) coordinates.
     */
    def createSlotGrid(x:Int, y:Int, w:Int, h:Int, dx:Int, dy:Int):Seq[(Int, Int)] =
        createGrid(x, y, w, h, dx+18, dy+18)

    /**
      * Creates a list of Int tuples representing (x, y) coordinates. These points represent the top-left corner of
      * the boxes of the created grid.
      *
      * @param x X pos of grid.
      * @param y Y pos of grid.
      * @param w Width of grid.
      * @param h Height of grid.
      * @param dx X spacing of slots. If zero, all x coordinates will be the same.
      * @param dy Y spacing of slots. If zero, all y coordinates will be the same.
      * @return Sequence of tuples representing (x, y) coordinates.
      */
    def createGrid(x:Int, y:Int, w:Int, h:Int, dx:Int, dy:Int) =
    {
        var grid = Seq[(Int, Int)]()
        for (iy <- 0 until h) for (ix <- 0 until w)
            grid :+= ((x+ix*dx) -> (y+iy*dy))
        grid
    }

    /**
      * Draws the standard Minecraft player inventory background. This background contains the 9x3 grid of slots
      * as well as the additional 9x1 grid of hotbar slots underneath.
      *
      * @param x The x coordinate of the top-left position of the slots to be rendered.
      * @param y The y coordinate of the top-left position of the slots to be rendered.
      */
    def drawPlayerInvBackground(x:Int, y:Int)
    {
        for ((x, y) <- createSlotGrid(x, y, 9, 3, 0, 0))
            drawSlotBackground(x-1, y-1)
        for ((x, y) <- createSlotGrid(x, y+58, 9, 1, 0, 0))
            drawSlotBackground(x-1, y-1)
    }

    /**
      * Draws the standard Minecraft inventory slot that is found in inventory GUIs.
      *
      * @param x The x coordinate of the top-left position of the slot to be rendered.
      * @param y The y coordinate of the top-left position of the slot to be rendered.
      */
    def drawSlotBackground(x:Int, y:Int)
    {
        RenderSystem.color4f(1, 1, 1, 1)

        TextureUtils.changeTexture(guiSlot)

        val rs = CCRenderState.instance()
        val vb = rs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX)
        vb.vertex(x, y+18, 0).uv(0, 1).endVertex()
        vb.vertex(x+18, y+18, 0).uv(1, 1).endVertex()
        vb.vertex(x+18, y, 0).uv(1, 0).endVertex()
        vb.vertex(x, y, 0).uv(0, 0).endVertex()
        rs.draw()
    }

    /**
      * Draws the standard GUI box typically used as background to a GUI.
      *
      * @param x The x position.
      * @param y The y position.
      * @param width The width of the box.
      * @param height The height of the box.
      * @param zLevel The z-position of the box.
      */
    def drawGuiBox(stack:MatrixStack, x:Int, y:Int, width:Int, height:Int, zLevel:Float)
    {
        drawGuiBox(stack, x, y, width, height, zLevel, true, true, true, true)
    }

    /**
      * Draws the standard GUI box typically used as background to a GUI.
      *
      * @param x The x position.
      * @param y The y position.
      * @param width The width of the box.
      * @param height The height of the box.
      * @param zLevel The z-position of the box.
      *
      * @param top True if should render the top edge.
      * @param left True if should render the left edge.
      * @param bottom True if should render the bottom edge.
      * @param right True if should render the right edge.
      */
    def drawGuiBox(stack:MatrixStack, x:Int, y:Int, width:Int, height:Int, zLevel:Float, top:Boolean, left:Boolean, bottom:Boolean, right:Boolean)
    {
/*        val u = 1
        val v = 29

        TextureUtils.changeTexture(guiExtras)

        GuiDraw.gui.setZLevel(zLevel)
        color(1, 1, 1, 1)
        pushMatrix()
        translate(x+2, y+2, 0)
        scale(width-4, height-4, 0)
        GuiDraw.drawTexturedModalRect(0, 0, u+19, v, 1, 1)
        popMatrix()
        if (top)
        {
            pushMatrix()
            translate(x+3, y, 0)
            scale(width-6, 1, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+4, v, 1, 3)
            popMatrix()
        }
        if (bottom)
        {
            pushMatrix()
            translate(x+3, y+height-3, 0)
            scale(width-6, 1, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+14, v, 1, 3)
            popMatrix()
        }
        if (left)
        {
            pushMatrix()
            translate(x, y+3, 0)
            scale(1, height-6, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u, v+4, 3, 1)
            popMatrix()
        }
        if (right)
        {
            pushMatrix()
            translate(x+width-3, y+3, 0)
            scale(1, height-6, 0)
            GuiDraw.drawTexturedModalRect(0, 0, u+8, v, 3, 1)
            popMatrix()
        }

        if (top && left) GuiDraw.drawTexturedModalRect(x, y, u, v, 4, 4)
        if (top && right) GuiDraw.drawTexturedModalRect(x+width-3, y, u+5, v, 3, 3)
        if (bottom && left) GuiDraw.drawTexturedModalRect(x, y+height-3, u+11, v, 3, 3)
        if (bottom && right) GuiDraw.drawTexturedModalRect(x+width-4, y+height-4, u+15, v, 4, 4)*/
    }

    /**
      * Draws a vertical progress bar anchored at the bottom left.
      *
      * @param x The x position of the top left.
      * @param y The y position of the top left.
      * @param u The texture u coordinate.
      * @param v The texture v coordinate.
      * @param w The width of the bar to be rendered.
      * @param h The height of the bar to be rendered.
      * @param prog The percentage progress of the bar, 100 being full at the top and 0 being empty at the bottom.
      */
    def drawVerticalTank(stack:MatrixStack, gui:AbstractGui, x:Int, y:Int, u:Int, v:Int, w:Int, h:Int, prog:Int):Unit = {
        gui.blit(stack, x, y+h-prog, u, v+h-prog, w, prog)
    }

    def drawLine(stack:MatrixStack, x1:Int, y1:Int, x2:Int, y2:Int, th:Int, argb:Int):Unit = {

        // Two points to rect
        val p1 = Vec2(x1, y1)
        val p2 = Vec2(x2, y2)
        val v = p2.subtract(p1)
        val p = Vec2(v.dy, -v.dx)
        val pnorm = p.normalize
        val widthVec = pnorm.multiply(th / 2.0)

        val r1 = p1.subtract(widthVec)
        val r2 = p2.subtract(widthVec)
        val r3 = p2.add(widthVec)
        val r4 = p1.add(widthVec)

        val p_238460_0_ = stack.last().pose()

        val f3 = (argb >> 24 & 255).toFloat / 255.0F
        val f = (argb >> 16 & 255).toFloat / 255.0F
        val f1 = (argb >> 8 & 255).toFloat / 255.0F
        val f2 = (argb & 255).toFloat / 255.0F
        val bufferbuilder = Tessellator.getInstance.getBuilder
        RenderSystem.enableBlend()
        RenderSystem.disableTexture()
        RenderSystem.defaultBlendFunc()
        bufferbuilder.begin(7, DefaultVertexFormats.POSITION_COLOR)
        bufferbuilder.vertex(p_238460_0_, r1.dx.toFloat, r1.dy.toFloat, 0.0F).color(f, f1, f2, f3).endVertex()
        bufferbuilder.vertex(p_238460_0_, r2.dx.toFloat, r2.dy.toFloat, 0.0F).color(f, f1, f2, f3).endVertex()
        bufferbuilder.vertex(p_238460_0_, r3.dx.toFloat, r3.dy.toFloat, 0.0F).color(f, f1, f2, f3).endVertex()
        bufferbuilder.vertex(p_238460_0_, r4.dx.toFloat, r4.dy.toFloat, 0.0F).color(f, f1, f2, f3).endVertex()
        bufferbuilder.end()
        WorldVertexBufferUploader.end(bufferbuilder)
        RenderSystem.enableTexture()
        RenderSystem.disableBlend()
    }
}
