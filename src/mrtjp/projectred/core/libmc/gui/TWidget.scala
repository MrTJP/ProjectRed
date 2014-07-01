package mrtjp.projectred.core.libmc.gui

import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.texture.TextureManager
import net.minecraft.client.gui.{Gui, FontRenderer}
import org.lwjgl.opengl.GL11

//class WGui extends Gui
//{
//    //def getZ = zLevel
//}
//
trait TWidget extends Gui
{
    var parentWidget:TWidget = null
    def mcInst:Minecraft = parentWidget.mcInst
    def renderEngine:TextureManager = parentWidget.renderEngine
    def fontRenderer:FontRenderer = parentWidget.fontRenderer
    def getZ:Float = parentWidget.getZ //overriden by actual gui

    def isRoot = parentWidget == this

    def getRoot:WidgetGui = this match
    {
        case wg:WidgetGui => wg
        case _ => parentWidget.getRoot
    }

    var widgets = Seq[TWidget]()

    val bounds:Rect

    def enableChildren = true

    def bringToFront(w:TWidget)
    {
        if (widgets contains w)
        {
            widgets = widgets.filterNot(_ == w) ++ Seq(w)
        }
    }

    def setSize(w:Int, h:Int){bounds.setWH(w, h)}
    def setPos(x:Int, y:Int){bounds.setMin(x, y)}

    def size = new Point(bounds.width, bounds.height)
    def pos = bounds.min
    def x = bounds.min.x
    def y = bounds.min.y
    def bWidth = bounds.width
    def bHeight = bounds.height

    def add(w:TWidget):this.type = {w <--> this; widgets :+= w; this}
    def +(w:TWidget):this.type = add(w)

    def bind(parent:TWidget){parentWidget = parent}
    def <-->(parent:TWidget){bind(parent)}

    protected final def update()
    {
        update_Impl()
        if (enableChildren) for (w <- widgets) w.update()
    }

    protected final def runInit()
    {
        runInit_Impl()
        if (enableChildren) for (w <- widgets) w.runInit()
    }

    private def operate(consumed:Boolean, subCall:(Boolean, TWidget) => Boolean) =
    {
        enableChildren && widgets.foldLeft(false)((c, w) => subCall(consumed, w)||c)
    }

    protected final def mouseClicked(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        operate(mouseClicked_Impl(p, button, consumed) || consumed, (c, w) => w.mouseClicked(p-pos, button, c))
    }

    protected final def mouseReleased(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        operate(mouseReleased_Impl(p, button, consumed) || consumed, (c, w) => w.mouseReleased(p-pos, button, c))
    }

    protected final def mouseDragged(p:Point, button:Int, time:Long, consumed:Boolean):Boolean =
    {
        operate(mouseDragged_Impl(p, button, time, consumed) || consumed, (c, w) => w.mouseDragged(p-pos, button, time, c))
    }

    protected final def mouseScrolled(p:Point, dir:Int, consumed:Boolean):Boolean =
    {
        operate(mouseScrolled_Impl(p, dir, consumed) || consumed, (c, w) => w.mouseScrolled(p-pos, dir, c))
    }

    protected final def keyPressed(c:Char, keycode:Int, consumed:Boolean):Boolean =
    {
        operate(keyPressed_Impl(c, keycode, consumed) || consumed, (c1, w) => w.keyPressed(c, keycode, c1))
    }

    protected final def drawBack(mouse:Point, frame:Float)
    {
        drawBack_Impl(mouse, frame)
        if (enableChildren)
        {
            translateTo()
            for (w <- widgets) w.drawBack(mouse-pos, frame)
            translateFrom()
        }
    }
    final def rootDrawBack(mouse:Point, frame:Float)
    {
        translateTo()
        drawBack_Impl(mouse, frame)
        if (enableChildren) for (w <- widgets) w.drawBack(mouse-pos, frame)
        translateFrom()
    }

    protected final def drawFront(mouse:Point, frame:Float)
    {
        drawFront_Impl(mouse, frame)
        if (enableChildren)
        {
            translateTo()
            for (w <- widgets) w.drawFront(mouse-pos, frame)
            translateFrom()
        }
    }
    final def rootDrawFront(mouse:Point, frame:Float)
    {
        drawFront_Impl(mouse, frame)
        if (enableChildren) for (w <- widgets) w.drawFront(mouse-pos, frame)
    }

    protected def translateTo(){GL11.glTranslated(pos.x, pos.y, 0)}
    protected def translateFrom(){GL11.glTranslated(-pos.x, -pos.y, 0)}

    protected final def startMessageChain(message:String)
    {
        if (!isRoot) parentWidget.receiveMessage(message)
    }

    protected final def receiveMessage(message:String)
    {
        receiveMessage_Impl(message)
        if (!isRoot) parentWidget.receiveMessage(message)
    }

    /** IMPLEMENTATION OVERRIDES **/

    /**
     * Called every tick from the main game loop.
     */
    def update_Impl(){}

    /**
     * Called to initialize the gui and subwidgets. Should be the main override
     * point for initializing anything in the gui.
     */
    def runInit_Impl(){}

    /**
     * Called when the mouse button is clicked.
     * @param p The current position of the mouse, relative to the parent.
     * @param button The button that was clicked. 0 is left button, 1 is right.
     * @param consumed If another widget has consumed this event.
     * @return If this widget has consumed this event.
     */
    def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) = false

    /**
     * Called when the mouse button is released.
     * @param p The current position of the mouse, relative to the parent.
     * @param button The button that was released. 0 is left button, 1 is right.
     * @param consumed If another widget has consumed this event.
     * @return If this widget has consumed this event.
     */
    def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) = false

    /**
     * Called constantly while the mouse is held down.
     * @param p The current position of the mouse, relative to the parent.
     * @param button The button that is currently held down. 0 is left, 1 is right
     * @param time Amount of time the button has been held down for.
     * @param consumed If another widget has consumed this event.
     * @return If this widget has consumed this event.
     */
    def mouseDragged_Impl(p:Point, button:Int, time:Long, consumed:Boolean) = false

    /**
     * Called when the mouse wheel is scrolled.
     * @param p The current position of the mouse, relative to the parent.
     * @param dir The direction of scroll. Negative for down, positive for up.
     * @param consumed If another widget has consumed this event.
     * @return If this widget has consumed this event.
     */
    def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) = false

    /**
     * Called when a key is pressed on the keyboard.
     * @param c The charecter that was pressed.
     * @param keycode The keycode for the button that was pressed.
     * @param consumed If another widget has consumed this event.
     * @return If this widget has consumed this event.
     */
    def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) = false

    /**
     * Called to draw the background.
     * All drawing is done relative to the parent, as GL11 is translated to the
     * parents position during this operation.
     * @param mouse The current position of the mouse, relative to the parent.
     * @param frame The partial frame until the next frame.
     */
    def drawBack_Impl(mouse:Point, frame:Float){}

    /**
     * Called to draw the foreground.
     * All drawing is done relative to the parent, as GL11 is translated to the
     * parents position during this operation.
     * @param mouse The current position of the mouse, relative to the parent.
     * @param frame The partial frame until the next frame.
     */
    def drawFront_Impl(mouse:Point, frame:Float){}

    /**
     * Called when a subwidget sends a message. This message is relayed to all
     * superwidgets one by one and stops at the root widget.
     * @param message The message that was sent by a subwidget using
     *                startMessageChain
     */
    def receiveMessage_Impl(message:String){}
}
