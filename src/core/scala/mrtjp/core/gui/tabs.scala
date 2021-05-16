/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import codechicken.lib.colour.EnumColour
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.gui.AbstractGui
import net.minecraft.client.renderer.RenderHelper
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.item.ItemStack
import net.minecraft.util.text.StringTextComponent
import net.minecraftforge.fml.client.gui.GuiUtils

import scala.jdk.CollectionConverters._
import scala.collection.mutable.ListBuffer

/**
  * Represents an expandable tab that sits on the right edge of a GUI window. Expands when clicked on.
  * Must be added as a child to a [[TabControlNode]], which handles interactions between all tabs such as
  * closing one when you click on another.
  *
  * @constructor
  * @param wMin The width of the tab when closed.
  * @param hMin The height of the tab when closed.
  * @param wMax The width of the tab when open.
  * @param hMax The height of the tab when open.
  * @param color Background render color, in RGB format.
  */
class TabNode(wMin:Int, hMin:Int, wMax:Int, hMax:Int, val color:Int) extends TNode
{
    def this(wMin:Int, hMin:Int, wMax:Int, hMax:Int) = this(wMin, hMin, wMax, hMax, EnumColour.LIGHT_GRAY.rgb)

    var currentW = wMin.asInstanceOf[Double]
    var currentH = wMin.asInstanceOf[Double]

    def getControl = parent.asInstanceOf[TabControlNode]

    var size = Size(wMin, hMin)
    override def frame = Rect(position, size)
    private val startBounds = frame

    var active = false
    def isOpen = active && size.width==wMax && size.height==hMax

    override def drawBack_Impl(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        val w = if (active) wMax else wMin
        val h = if (active) hMax else hMin

        if (w != size.width) currentW += (w-currentW)/8
        if (h != size.height) currentH += (h-currentH)/8

        size = Size(currentW.round.toInt, currentH.round.toInt)

        drawBox(stack)
        drawIcon(stack)
        if (isOpen)
        {
            drawTab(stack)
            ourChildren.foreach(_.hidden = false)
        }
        else ourChildren.foreach(_.hidden = true)
    }

    override def drawFront_Impl(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (rayTest(mouse))
        {
            val list = ListBuffer[String]()
            buildToolTip(list)
            val root = getRoot
            GuiUtils.drawHoveringText(stack, list.map(new StringTextComponent(_)).asJava, mouse.x+12, mouse.y-12, root.width, root.height, -1, getFontRenderer)
        }
    }

    def drawTab(stack:MatrixStack){}

    def drawIcon(stack:MatrixStack){}

    def buildToolTip(list:ListBuffer[String]){}

    def drawBox(stack:MatrixStack)
    {
        val r = (color>>16&255)/255.0F
        val g = (color>>8&255)/255.0F
        val b = (color&255)/255.0F
        RenderSystem.color4f(r, g, b, 1)

        GuiLib.drawGuiBox(stack, position.x, position.y, size.width, size.height, 0)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && startBounds.contains(p))
        {
            getControl.onTabClicked(this)
            true
        }
        else false
    }
}

/**
  * A trait that allows a [[TabNode]] to render an ItemStack overlay.
  */
trait TStackTab extends TabNode
{
    /** The ItemStack to render as the overlay. */
    var iconStack:ItemStack = ItemStack.EMPTY

    abstract override def drawIcon(stack:MatrixStack)
    {
        super.drawIcon(stack)
        RenderSystem.color4f(1, 1, 1, 1)
        RenderSystem.enableRescaleNormal()
        mcInst.getItemRenderer.blitOffset = (zPosition+25).toFloat
        mcInst.getItemRenderer.renderGuiItem(iconStack, position.x+3, position.y+3)
        RenderSystem.disableRescaleNormal()
//        RenderSystem.disableLighting()
//        RenderHelper.disableStandardItemLighting()
    }
}

/**
  * A trait that allows a [[TabNode]] to render a texture sprite overlay.
  */
trait TIconTab extends TabNode
{
    /** The sprite to render as the overlay. */
    var icon:TextureAtlasSprite = null

    abstract override def drawIcon(stack:MatrixStack)
    {
        super.drawIcon(stack)
        AbstractGui.blit(stack, position.x+3, position.x+3, getBlitOffset, 16, 16, icon)
    }
}


/**
  * Parent node for a [[TabNode]] class. Negotiates the expanding and contracting of tabs, as well as the closing of
  * one tab as another one opens. This must be the direct parent to all `TabNode`s in the tree. It is assumed that all
  * children of this node are of type [[TabNode]].
  *
  * @constructor
  * @param x The top left x coordinate of this node.
  * @param y The top right y coordinate of this node.
  */
class TabControlNode(x:Int, y:Int) extends TNode
{
    position = Point(x, y)
    override def frame = Rect(position, Size.zeroSize)

    private var active:TabNode = null

    /**
      * Called by a child [[TabNode]] when it is clicked on.
      *
      * @param tab The `TabNode` that was clicked. Must be a direct child to this node.
      */
    def onTabClicked(tab:TabNode)
    {
        if (tab != active)
        {
            if (active != null) active.active = false
            tab.active = true
            active = tab
        }
        else
        {
            tab.active = false
            active = null
        }
    }

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        var dy = 0
        for (w <- ourChildren)
        {
            w.position = Point(w.position.x, dy)
            dy += w.frame.height
        }
    }
}
