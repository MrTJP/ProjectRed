package mrtjp.projectred.core.inventory

import codechicken.core.gui.GuiDraw
import net.minecraft.client.renderer.RenderHelper
import net.minecraft.client.renderer.entity.RenderItem
import net.minecraft.item.ItemStack
import net.minecraft.util.IIcon
import org.lwjgl.opengl.{GL12, GL11}
import scala.collection.JavaConversions
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.libmc.BasicGuiUtils

class WidgetTab(c:Int, wMin:Int, hMin:Int, wMax:Int, hMax:Int) extends GhostWidget(0, 0, wMin, hMin)
{
    var controller:WidgetTabController = _

    var currentW = wMin.asInstanceOf[Double]
    var currentH = wMin.asInstanceOf[Double]

    var active = false

    def isOpen = active && width==wMax && height==hMax

    override def subWidgets = isOpen

    override def drawBack(mouseX:Int, mouseY:Int, frame:Float)
    {
        super.drawBack(mouseX, mouseY, frame)

        val w = if (active) wMax else wMin
        val h = if (active) hMax else hMin

        if (w != width) currentW += (w-currentW)/8
        if (h != height) currentH += (h-currentH)/8

        width = currentW.round.asInstanceOf[Int]
        height = currentH.round.asInstanceOf[Int]

        drawBox()

        drawIcon()

        if (isOpen) drawTab()
    }

    override def drawFront(mouseX:Int, mouseY:Int)
    {
        super.drawFront(mouseX, mouseY)
        if (pointInsideStart(mouseX, mouseY))
        {
            val list = ListBuffer[String]()
            buildToolTip(list)
            GuiDraw.drawMultilineTip(mouseX+12, mouseY-12, JavaConversions.bufferAsJavaList(list))
        }
    }

    def drawTab() {}

    def drawIcon() {}

    def buildToolTip(list:ListBuffer[String]) {}

    def drawBox()
    {
        val r = (c>>16&255)/255.0F
        val g = (c>>8&255)/255.0F
        val b = (c&255)/255.0F
        GL11.glColor4f(r, g, b, 1)

        BasicGuiUtils.drawGuiBox(x, y, width, height, currentZLevel())
    }

    def bind(parent:WidgetTabController)
    {
        controller = parent
    }

    override def mouseClicked(px:Int, py:Int, button:Int)
    {
        super.mouseClicked(px, py, button)

        if (pointInsideStart(px, py))
            if (controller != null)
                controller.onTabClicked(this)
    }

    def pointInsideStart(px:Int, py:Int) = px>=x && px<x+wMin && py>=y && py<y+hMin
}

object WidgetTab
{
    val itemRender = new RenderItem
}

trait TStackTab extends WidgetTab
{
    var iconStack:ItemStack = _

    def setIconStack(stack:ItemStack) =
    {
        iconStack = stack
        this
    }

    abstract override def drawIcon()
    {
        GL11.glColor4f(1, 1, 1, 1)
        RenderHelper.enableGUIStandardItemLighting()
        GL11.glEnable(GL12.GL_RESCALE_NORMAL)
        WidgetTab.itemRender.zLevel = currentZLevel()+50
        WidgetTab.itemRender.renderItemAndEffectIntoGUI(mc.fontRenderer, mc.getTextureManager(), iconStack, x+3, y+3)
        GL11.glDisable(GL12.GL_RESCALE_NORMAL)
        GL11.glDisable(GL11.GL_LIGHTING)
        RenderHelper.disableStandardItemLighting()

        super.drawIcon()
    }
}

trait TIconTab extends WidgetTab
{
    var icon:IIcon = _

    def setIcon(i:IIcon) =
    {
        icon = i
        this
    }

    abstract override def drawIcon()
    {
        drawTexturedModelRectFromIcon(x+3, x+3, icon, 16, 16)
        super.drawIcon()
    }
}

class WidgetTabController(x:Int, y:Int) extends GhostWidget(x, y, 0, 0)
{
    var active:WidgetTab = _

    override def add(widget:GhostWidget)
    {
        if (widget.isInstanceOf[WidgetTab])
            widget.asInstanceOf[WidgetTab].bind(this)

        super.add(widget)
    }

    def onTabClicked(tab:WidgetTab)
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

    override def drawBack(mouseX:Int, mouseY:Int, frame:Float)
    {
        super.drawBack(mouseX, mouseY, frame)
        height = 0
        width = 0
        for (w <- widgets)
        {
            w.y = height
            width += w.width
            height += w.height-1
        }
    }

    override def subWidgets = true
}