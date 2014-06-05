package mrtjp.projectred.core.libmc.gui

import scala.collection.mutable.ListBuffer
import org.lwjgl.opengl.{GL12, GL11}
import mrtjp.projectred.core.libmc.PRColors
import scala.collection.JavaConversions
import net.minecraft.item.ItemStack
import net.minecraft.client.renderer.RenderHelper
import net.minecraft.util.IIcon
import net.minecraft.client.renderer.entity.RenderItem
import codechicken.lib.gui.GuiDraw

class WidgetTab(wMin:Int, hMin:Int, wMax:Int, hMax:Int, val color:Int) extends TWidget
{
    def this(wMin:Int, hMin:Int, wMax:Int, hMax:Int) = this(wMin, hMin, wMax, hMax, PRColors.LIGHT_GREY.rgb)

    var currentW = wMin.asInstanceOf[Double]
    var currentH = wMin.asInstanceOf[Double]

    def getControl = parentWidget.asInstanceOf[WidgetTabControl]

    override val bounds = new Rect().setWH(wMin, hMin)
    private val startBounds = bounds.copy

    var active = false
    def isOpen = active && bWidth==wMax && bHeight==hMax

    override def enableChildren = isOpen

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        val w = if (active) wMax else wMin
        val h = if (active) hMax else hMin

        if (w != bWidth) currentW += (w-currentW)/8
        if (h != bHeight) currentH += (h-currentH)/8

        setSize(currentW.round.asInstanceOf[Int], currentH.round.asInstanceOf[Int])

        drawBox()
        drawIcon()
        if (isOpen) drawTab()
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        if (startBounds.intersects(mouse))
        {
            val list = ListBuffer[String]()
            buildToolTip(list)
            GuiDraw.drawMultilineTip(mouse.x+12, mouse.y-12, JavaConversions.bufferAsJavaList(list))
        }
    }

    def drawTab(){}

    def drawIcon(){}

    def buildToolTip(list:ListBuffer[String]){}

    def drawBox()
    {
        val r = (color>>16&255)/255.0F
        val g = (color>>8&255)/255.0F
        val b = (color&255)/255.0F
        GL11.glColor4f(r, g, b, 1)

        GuiLib.drawGuiBox(x, y, bWidth, bHeight, getZ)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && startBounds.intersects(p))
        {
            getControl.onTabClicked(this)
            true
        }
        else false
    }
}

trait TStackTab extends WidgetTab
{
    var iconStack:ItemStack = null
    def setIconStack(stack:ItemStack):this.type = {iconStack = stack; this}

    abstract override def drawIcon()
    {
        super.drawIcon()
        GL11.glColor4f(1, 1, 1, 1)
        RenderHelper.enableGUIStandardItemLighting()
        GL11.glEnable(GL12.GL_RESCALE_NORMAL)
        TStackTab.itemRender.zLevel = getZ+50
        TStackTab.itemRender.renderItemAndEffectIntoGUI(fontRenderer, renderEngine, iconStack, x+3, y+3)
        GL11.glDisable(GL12.GL_RESCALE_NORMAL)
        GL11.glDisable(GL11.GL_LIGHTING)
        RenderHelper.disableStandardItemLighting()
    }
}

object TStackTab
{
    val itemRender = new RenderItem
}

trait TIconTab extends WidgetTab
{
    var icon:IIcon = null
    def setIcon(i:IIcon):this.type = {icon = i; this}

    abstract override def drawIcon()
    {
        super.drawIcon()
        drawTexturedModelRectFromIcon(x+3, x+3, icon, 16, 16)
    }
}


class WidgetTabControl(x:Int, y:Int) extends TWidget
{
    override val bounds = new Rect(x, y, 0, 0)

    var active:WidgetTab = null

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

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        setSize(0, 0)
        for (w <- widgets)
        {
            w.setPos(w.x, bHeight)
            bounds.setWidth(bWidth+w.bWidth)
            bounds.setHeight(bHeight+w.bHeight-1)
        }
    }
}