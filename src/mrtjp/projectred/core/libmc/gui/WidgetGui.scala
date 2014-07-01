package mrtjp.projectred.core.libmc.gui

import codechicken.lib.gui.GuiDraw
import mrtjp.projectred.core.libmc.inventory.WidgetContainer
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.GuiScreen
import net.minecraft.client.gui.inventory.GuiContainer
import net.minecraft.inventory.Container
import org.lwjgl.input.Mouse

class WidgetGui(c:Container, w:Int, h:Int) extends GuiContainer(c) with TWidget
{
    def this(c:Container) = this(c, 176, 166)
    def this(x:Int, y:Int) = this(new WidgetContainer, x, y)

    xSize = w
    ySize = h
    this <--> this

    override def mcInst = mc
    override def renderEngine = mcInst.renderEngine
    override def fontRenderer = mcInst.fontRenderer
    override def getZ = zLevel

    // Lazy because initGui has to be called before this
    override lazy val bounds = new Rect().setMin(guiLeft, guiTop).setWH(xSize, ySize)

    var prevGui:GuiScreen = null
    def setJumpBack(p:GuiScreen){prevGui = p}

    def jumpTo(g:GuiScreen, containerHack:Boolean)
    {
        mcInst.displayGuiScreen(g)
        if (containerHack) g match
        {
            case cont:GuiContainer =>
                cont.inventorySlots.windowId = inventorySlots.windowId
            case _ =>
        }
    }

    override def initGui()
    {
        super.initGui()
        bounds.setMin(guiLeft, guiTop).setWH(xSize, ySize)
    }

    final override def updateScreen()
    {
        super.updateScreen()
        update()
    }

    final override def setWorldAndResolution(mc:Minecraft, i:Int, j:Int)
    {
        val init = this.mc == null
        super.setWorldAndResolution(mc, i, j)
        if (init) runInit()
    }

    final override def mouseClicked(x:Int, y:Int, button:Int)
    {
        super.mouseClicked(x, y, button)
        mouseClicked(new Point(x, y), button, false)
    }

    final override def mouseMovedOrUp(x:Int, y:Int, button:Int)
    {
        super.mouseMovedOrUp(x, y, button)
        if (button != -1) mouseReleased(new Point(x, y), button, false)
    }

    final override def mouseClickMove(x:Int, y:Int, button:Int, time:Long)
    {
        super.mouseClickMove(x, y, button, time)
        mouseDragged(new Point(x, y), button, time, false)
    }

    final override def handleMouseInput()
    {
        super.handleMouseInput()
        val i = Mouse.getEventDWheel
        if (i != 0)
        {
            val p = GuiDraw.getMousePosition
            mouseScrolled(new Point(p.x, p.y), if (i > 0) 1 else -1, false)
        }
    }

    final override def keyTyped(c:Char, i:Int)
    {
        if (isClosingKey(i)) //esc
        {
            if (prevGui != null) jumpTo(prevGui, prevGui.isInstanceOf[GuiContainer])
            else if (forwardClosing) super.keyTyped(c, i)
        }
        else if ((2 to 10 contains i) && !(blockedHotkeyNumbers contains i-1))
            super.keyTyped(c, i) //number for slot moving
        keyPressed(c, i, false)
    }

    def forwardClosing = true
    def isClosingKey(keycode:Int) =
        keycode == 1 || keycode == mc.gameSettings.keyBindInventory.getKeyCode //esc or inv key
    def blockedHotkeyNumbers:Set[Int] = Set()

    /**
     * Front/back rendering overridden, because at root, we dont push the children to our pos, because its zero.
     */
    private var lastFrame = 0.0F
    final override def drawGuiContainerBackgroundLayer(f:Float, mx:Int, my:Int)
    {
        lastFrame = f
        val mouse = new Point(mx, my)
        rootDrawBack(mouse, f)
    }

    final override def drawGuiContainerForegroundLayer(mx:Int, my:Int)
    {
        val mouse = new Point(mx, my)
        rootDrawFront(mouse, lastFrame)
    }
}
