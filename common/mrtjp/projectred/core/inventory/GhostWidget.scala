package mrtjp.projectred.core.inventory

import net.minecraft.client.Minecraft
import net.minecraft.client.gui.FontRenderer
import net.minecraft.client.gui.Gui
import net.minecraft.client.gui.GuiScreen
import net.minecraft.client.renderer.texture.TextureManager
import net.minecraft.util.ResourceLocation
import org.lwjgl.opengl.GL11

object GhostWidget
{
    def sendAction(screen:GuiScreen, actionCommand:String)
    {
        if (actionCommand != null && screen.isInstanceOf[IGuiActionListener])
            (screen.asInstanceOf[IGuiActionListener]).actionPerformed(actionCommand)
    }

    val guiTex = new ResourceLocation("textures/gui/widgets.png")
    val guiExtras = new ResourceLocation("projectred:textures/gui/guiextras.png")
}

class GhostWidget(var x:Int, var y:Int, var width:Int, var height:Int) extends Gui
{
    var mc:Minecraft = null
    var parentScreen:GhostGuiContainer = null
    var renderEngine:TextureManager = null
    var fontRenderer:FontRenderer = null

    var widgets = Seq[GhostWidget]()

    def currentZLevel() = zLevel

    def setSize(x:Int, y:Int, width:Int, height:Int)
    {
        this.x = x
        this.y = y
        this.width = width
        this.height = height
    }

    def pointInside(px:Int, py:Int):Boolean = px>=x && px<x+width && py>=y && py<y+height

    def sendAction(actionCommand:String)
    {
        GhostWidget.sendAction(parentScreen, actionCommand)
    }

    def mouseClicked(x:Int, y:Int, button:Int)
    {
        if (subWidgets) for (w <- widgets) w.mouseClicked(x-this.x, y-this.y, button)
    }

    def mouseMovedOrUp(x:Int, y:Int, button:Int)
    {
        if (subWidgets) for (w <- widgets) w.mouseMovedOrUp(x-this.x, y-this.y, button)
    }

    def mouseDragged(x:Int, y:Int, button:Int, time:Long)
    {
        if (subWidgets) for (w <- widgets) w.mouseDragged(x, y, button, time)
    }

    def update()
    {
        if (subWidgets) for (w <- widgets) w.update()
    }

    def drawBack(mouseX:Int, mouseY:Int, frame:Float)
    {
        if (subWidgets)
        {
            translateTo
            for (w <- widgets) w.drawBack(mouseX, mouseY, frame)
            translateFrom
        }
    }

    def drawFront(mouseX:Int, mouseY:Int)
    {
        if (subWidgets)
        {
            translateTo
            for (w <- widgets) w.drawFront(mouseX, mouseY)
            translateFrom
        }
    }

    def keyTyped(c:Char, keycode:Int)
    {
        if (subWidgets) for (w <- widgets) w.keyTyped(c, keycode)
    }

    def mouseScrolled(x:Int, y:Int, scroll:Int)
    {
        for (w <- widgets) w.mouseScrolled(x-this.x, y-this.y, scroll)
    }

    def onAdded(s:GhostGuiContainer)
    {
        mc = Minecraft.getMinecraft
        parentScreen = s
        renderEngine = mc.renderEngine
        fontRenderer = mc.fontRenderer
    }

    def add(widget:GhostWidget)
    {
        widgets = widgets:+widget
        widget.onAdded(parentScreen)
    }

    def translateTo() = GL11.glTranslated(this.x, this.y, 0)
    def translateFrom() = GL11.glTranslated(-this.x, -this.y, 0)

    def subWidgets() = false
}