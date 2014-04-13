package mrtjp.projectred.core.inventory

import codechicken.core.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import net.minecraft.client.Minecraft
import org.lwjgl.opengl.GL11
import scala.collection.convert.WrapAsJava
import scala.collection.mutable.ListBuffer

class WidgetButton(x:Int, y:Int, l:Int, w:Int) extends GhostWidget(x, y, l, w)
{
    var actionCommand:String = null
    var text = ""

    override def mouseClicked(x:Int, y:Int, button:Int)
    {
        if (pointInside(x, y))
        {
            Minecraft.getMinecraft.sndManager.playSoundFX("random.click", 1, 1)
            onButtonClicked()
            if (actionCommand != null) sendAction(actionCommand)
        }
    }

    def onButtonClicked() {}

    override def drawBack(mousex:Int, mousey:Int, frame:Float)
    {
        GL11.glColor4f(1, 1, 1, 1)
        val mouseover = pointInside(mousex, mousey)
        drawButtonBackground(mousex, mousey, frame)
        drawButton(mouseover)
    }

    override def drawFront(mousex:Int, mousey:Int)
    {
        val mouseover = pointInside(mousex, mousey)
        val list = new ListBuffer[String]
        buildTooltip(list)
        if (mouseover) GuiDraw.drawMultilineTip(mousex+12, mousey-12, WrapAsJava.seqAsJavaList(list))
    }

    def drawButtonBackground(mousex:Int, mousey:Int, frame:Float) {}
    def drawButton(mouseover:Boolean) {}
    def buildTooltip(list:ListBuffer[String]) {}

    def setActionCommand(string:String) =
    {
        actionCommand = string
        this
    }

    def setText(text:String) =
    {
        this.text = text
        this
    }

    def getText = text
}

trait TButtonMCStyle extends WidgetButton
{
    abstract override def drawButtonBackground(mousex:Int, mousey:Int, frame:Float)
    {
        CCRenderState.changeTexture(GhostWidget.guiTex)
        GL11.glColor4f(1, 1, 1, 1)
        val state = if (pointInside(mousex, mousey)) 2 else 1

        drawTexturedModalRect(x, y, 0, 46+state*20, width/2, height/2)
        drawTexturedModalRect(x+width/2, y, 200-width/2, 46+state*20, width/2, height/2)
        drawTexturedModalRect(x, y+height/2, 0, 46+state*20+20-height/2, width/2, height/2)
        drawTexturedModalRect(x+width/2, y+height/2, 200-width/2, 46+state*20+20-height/2, width/2, height/2)

        super.drawButtonBackground(mousex, mousey, frame)
    }
}

trait TButtonTextOverlay extends WidgetButton
{
    abstract override def drawButton(mouseover:Boolean)
    {
        drawCenteredString(fontRenderer, getText, x+width/2, y+(height-8)/2, if (mouseover) 0xFFFFFFA0 else 0xFFE0E0E0)

        super.drawButton(mouseover)
    }
}

class WidgetCheckBox(x1:Int, y1:Int, private var checked:Boolean) extends WidgetButton(x1-7, y1-7, 14, 14) with TButtonMCStyle
{
    override def onButtonClicked()
    {
        super.onButtonClicked()
        setCheck(!checked)
    }

    override def drawButtonBackground(mousex:Int, mousey:Int, frame:Float)
    {
        super.drawButtonBackground(mousex, mousey, frame)
    }

    override def drawButton(mouseover:Boolean)
    {
        super.drawButton(mouseover)
        CCRenderState.changeTexture(GhostWidget.guiExtras)
        val u = if (checked) 17 else 1
        drawTexturedModalRect(x, y, u, 134, 14, 14)
    }

    def setCheck(flag:Boolean) =
    {
        val old = checked
        checked = flag
        if (old != checked) onStateChanged(old)
        this
    }

    def getChecked = checked

    def onStateChanged(oldState:Boolean) {}
}

class WidgetDotSelector(x1:Int, y1:Int) extends WidgetButton(x1-4, y1-4, 8, 8)
{
    override def drawButtonBackground(mousex:Int, mousey:Int, f:Float)
    {
        CCRenderState.changeTexture(GhostWidget.guiExtras)
        GL11.glColor4f(1, 1, 1, 1)
        val mouseover:Boolean = pointInside(mousex, mousey)
        val u = if (mouseover) 11 else 1
        drawTexturedModalRect(x, y, u, 1, 8, 8)
    }
}

class JWidgetButton(x:Int, y:Int, l:Int, w:Int) extends WidgetButton(x, y, l, w) with TButtonMCStyle with TButtonTextOverlay
{
    override def drawButton(mouseover:Boolean) = super.drawButton(mouseover)

    override def drawButtonBackground(mousex:Int, mousey:Int, frame:Float) = super.drawButtonBackground(mousex, mousey, frame)
}