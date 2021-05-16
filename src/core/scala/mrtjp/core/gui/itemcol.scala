/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem._
import mrtjp.core.item.ItemKeyStack
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.AbstractGui
import net.minecraft.client.util.ITooltipFlag.TooltipFlags._
import net.minecraft.item.ItemStack
import net.minecraft.util.text.TextFormatting

class ItemListNode extends TNode
{
    var items = Seq[ItemKeyStack]()
    var itemSize = Size(16, 16)
    var gridWidth = 3

    var displayNodeFactory = {stack:ItemKeyStack => new ItemDisplayNode}
    var cullFrame = Rect.infiniteRect

    private var dispNodes = Seq[ItemDisplayNode]()

    override def frame = Rect(position, itemSize.multiply(math.min(items.size, gridWidth), items.size/gridWidth+1))

    def reset()
    {
        val it = items.iterator
        var (x, y) = (0, 0)

        dispNodes.foreach(_.removeFromParent())
        dispNodes = Seq()

        while(it.hasNext)
        {
            val i = it.next()
            val d = displayNodeFactory(i)
            if (d != null)
            {
                d.stack = i
                d.size = itemSize
                d.position = Point(itemSize.multiply(x, y))
                addChild(d)

                val df = convertRectToScreen(d.frame)
                if (cullFrame.intersects(df))
                {
                    dispNodes :+= d
                }
                else d.removeFromParent()

                x += 1
                if (x >= gridWidth){x = 0; y += 1}
            }
        }
    }
}

class ItemDisplayNode extends TNode
{
    var stack:ItemKeyStack = null
    var size = Size.zeroSize

    var drawNumber = true
    var drawTooltip = true

    var backgroundColour = 0
    var clickDelegate = {() => }

    override def frame = Rect(position, size)

    override def drawBack_Impl(mStack:MatrixStack, mouse:Point, rframe:Float)
    {
        fillGradient(mStack, position.x, position.y, position.x+size.width, position.y+size.height, backgroundColour, backgroundColour)
        ItemDisplayNode.renderItem(mStack, this, position, size, zPosition, drawNumber, stack.makeStack)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && rayTest(p))
        {
            clickDelegate()
            true
        }
        else false
    }

    override def drawFront_Impl(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (drawTooltip && frame.contains(mouse) && rayTest(mouse))
            drawTooltip(mouse)
    }

    def drawTooltip(mouse:Point)
    {
        ClipNode.tempDisableScissoring()
        //draw tooltip with absolute coords to allow it to force-fit on screen
        translateToScreen()
        val Point(mx, my) = parent.convertPointToScreen(mouse)

        import scala.jdk.CollectionConverters._
        val lines = stack.makeStack.getTooltipLines(mcInst.player,
            if(mcInst.options.advancedItemTooltips) ADVANCED else NORMAL).asScala
        lines.tail.foreach(_.getStyle.applyFormat(TextFormatting.GRAY))

        //TODO tooltips
        //GuiDraw.drawMultiLineTip(mx+12, my-12, l2)

        translateFromScreen()
        ClipNode.tempEnableScissoring()
    }
}

object ItemDisplayNode
{
    val renderItem = Minecraft.getInstance().getItemRenderer

    def renderItem(mStack:MatrixStack, gui:AbstractGui, position:Point, size:Size, zPosition:Double, drawNumber:Boolean, stack:ItemStack)
    {
        val font = stack.getItem.getFontRenderer(stack) match {
            case null => Minecraft.getInstance().font
            case r => r
        }

//        val f = font.getUnicodeFlag
//        font.setUnicodeFlag(true)

        glItemPre()
        pushMatrix()
//        new Scale(size.width/16.0, size.height/16.0, 1)
//                .at(new Vector3(position.x, position.y, 0)).glApply
        translated(position.x, position.y, 0)
        scaled(size.width/16.0, size.height/16.0, 1)
        translated(-position.x, -position.y, 0)

        gui.setBlitOffset((zPosition+10).toInt)
        renderItem.blitOffset = (zPosition+10.0).toFloat
//        enableDepthTest()
//        enableLighting()
        renderItem.renderGuiItem(stack, position.x, position.y)
        renderItem.renderGuiItemDecorations(font, stack, position.x, position.y, "")
//        disableLighting()
//        disableDepthTest()
        renderItem.blitOffset = zPosition.toFloat

        if (drawNumber && stack.getCount > 1) {
            val s =
                if (stack.getCount == 1) ""
                else if (stack.getCount < 1000) stack.getCount+""
                else if (stack.getCount < 100000) stack.getCount/1000+"K"
                else if (stack.getCount < 1000000) "0."+stack.getCount/100000+"M"
                else stack.getCount/1000000+"M"
            font.draw(mStack, s, position.x+19-2-font.width(s), position.y+6+3, 16777215)
        }
        popMatrix()
        glItemPost()

//        font.setUnicodeFlag(f)
    }

    def glItemPre()
    {
//        disableRescaleNormal()
//        disableDepthTest()

        pushMatrix()
        color4f(1.0F, 1.0F, 1.0F, 1.0F)
//        RenderHelper.enableGUIStandardItemLighting()
        enableRescaleNormal()
        glMultiTexCoord2f(33986, 240.0F, 240.0F)
        color4f(1.0F, 1.0F, 1.0F, 1.0F)
        enableDepthTest()
//        OpenGlHelper.setLightmapTextureCoords(OpenGlHelper.lightmapTexUnit, 240/1.0F, 240/1.0F)
//        disableDepth()
//        disableLighting()
    }

    def glItemPost()
    {
        popMatrix()
//        enableDepthTest()
    }

}