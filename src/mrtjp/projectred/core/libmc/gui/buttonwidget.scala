package mrtjp.projectred.core.libmc.gui

import org.lwjgl.opengl.GL11
import scala.collection.mutable.ListBuffer
import scala.collection.convert.WrapAsJava
import mrtjp.projectred.core.libmc.ResourceLib
import codechicken.lib.gui.GuiDraw

/**
 * Base button class with position and width/height. Doesnt render anything, nor does it perform
 * action when clicked.
 * @param x X position
 * @param y Y position
 * @param w Width
 * @param h Height
 */
class WidgetButton(x:Int, y:Int, w:Int, h:Int) extends TWidget
{
    override val bounds = new Rect().setMin(x, y).setWH(w, h)

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && bounds.intersects(p))
        {
            ResourceLib.soundButton.play()
            onButtonClicked()
            true
        }
        else false
    }

    def onButtonClicked(){}

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GL11.glColor4f(1, 1, 1, 1)
        val mouseover = bounds.intersects(mouse)
        drawButtonBackground(mouseover)
        drawButton(mouseover)
    }

    override def drawFront_Impl(mouse:Point, frame:Float)
    {
        if (bounds.intersects(mouse))
        {
            val list = new ListBuffer[String]
            buildTooltip(list)
            GuiDraw.drawMultilineTip(mouse.x+12, mouse.y-12, WrapAsJava.seqAsJavaList(list))
        }
    }

    def drawButtonBackground(mouseover:Boolean){}
    def drawButton(mouseover:Boolean){}
    def buildTooltip(list:ListBuffer[String]){}
}

/**
 * Trait for buttons that renders their background as a default MC button.
 */
trait TButtonMC extends WidgetButton
{
    abstract override def drawButtonBackground(mouseover:Boolean)
    {
        super.drawButtonBackground(mouseover)

        ResourceLib.guiTex.bind()
        GL11.glColor4f(1, 1, 1, 1)
        val state = if (mouseover) 2 else 1

        drawTexturedModalRect(x, y, 0, 46+state*20, bWidth/2, bHeight/2)
        drawTexturedModalRect(x+bWidth/2, y, 200-bWidth/2, 46+state*20, bWidth/2, bHeight/2)
        drawTexturedModalRect(x, y+bHeight/2, 0, 46+state*20+20-bHeight/2, bWidth/2, bHeight/2)
        drawTexturedModalRect(x+bWidth/2, y+bHeight/2, 200-bWidth/2, 46+state*20+20-bHeight/2, bWidth/2, bHeight/2)
    }
}

/**
 * Trait for buttons that renders their foreground as text.
 */
trait TButtonText extends WidgetButton
{
    var text = ""
    def setText(t:String):this.type = {text = t; this}

    abstract override def drawButton(mouseover:Boolean)
    {
        super.drawButton(mouseover)
        drawCenteredString(fontRenderer, text, x+bWidth/2, y+(bHeight-8)/2, if (mouseover) 0xFFFFFFA0 else 0xFFE0E0E0)
    }
}

/**
 * Trait for buttons that send an action message when clicked.
 */
trait TButtonAction extends WidgetButton
{
    var actionBuild:(this.type => String) = {_ => ""}
    def setAction(a:String):this.type = {actionBuild = _ => a; this}
    def setActionBuilder(a:(this.type => String)):this.type = {actionBuild = a; this}

    abstract override def onButtonClicked()
    {
        super.onButtonClicked()
        val message = actionBuild(this)
        if (!message.isEmpty) startMessageChain(message)
    }
}

/**
 * Button that is used for selection.
 * @param x1 center x position
 * @param y1 center y position
 */
class WidgetDotSelect(x1:Int, y1:Int) extends WidgetButton(x1-4, y1-4, 8, 8) with TButtonAction
{
    override def drawButtonBackground(mouseover:Boolean)
    {
        super.drawButtonBackground(mouseover)
        ResourceLib.guiExtras.bind()
        GL11.glColor4f(1, 1, 1, 1)
        drawTexturedModalRect(x, y, if (mouseover) 11 else 1, 1, 8, 8)
    }
}

/**
 * Check box button that has either an on or off state.
 * @param x1 center x position
 * @param y1 center y position
 * @param state initial state
 */
class WidgetCheckBox(x1:Int, y1:Int, var state:Boolean) extends WidgetButton(x1-7, y1-7, 14, 14) with TButtonMC with TButtonAction
{
    def this(x1:Int, y1:Int) = this(x1, y1, false)

    override def onButtonClicked()
    {
        super.onButtonClicked()
        state = !state
    }

    def setState(flag:Boolean):this.type = {state = flag; this}

    def setActions(unchecked:String, checked:String):this.type =
    {
        setActionBuilder(w => if (w.state) checked else unchecked)
    }
}

/**
 * Default implementation of a button in mc with normal render and text overlay
 * @param x X position
 * @param y Y position
 * @param w Width
 * @param h Height
 */
class WidgetButtonMC(x:Int, y:Int, w:Int, h:Int) extends WidgetButton(x, y, w, h) with TButtonMC with TButtonText with TButtonAction

/**
 * Implementation of a button with manual icon rendering via override.
 * @param x X position
 * @param y Y position
 * @param w Width
 * @param h Height
 */
class WidgetButtonIcon(x:Int, y:Int, w:Int, h:Int) extends WidgetButton(x, y, w, h) with TButtonMC with TButtonAction