/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.screen.inventory.ContainerScreen
import net.minecraft.entity.player.PlayerInventory
import net.minecraft.inventory.container.Container
import net.minecraft.util.text.ITextComponent

/**
  * Represents the root node in a GUI node tree.
  *
  * == Overview ==
  * The GUI rendering system provided in this library is based off of a render tree concept. Elements of the GUI are
  * implementations of `TNode`. Each node can draw things and be interacted with. It can also have child nodes, also
  * implementations of `TNode`.
  *
  * == Building a GUI ==
  * You can build a GUI by subclassing `NodeGui` or using it as is. Here is a simple example of creating a 100x100 window
  * with a text box and button inside. When the button is clicked, a custom function called onMyButtonClicked()
  * is called.
  *
  * {{{
  *
  * import mrtjp.core.gui._
  *
  * val rootNode = new NodeGui(100, 100) //Create centered window of size 100x100 on screen
  *
  * //add text box
  * val textBox = new SimpleTextBoxNode
  * textBox.size = Size(50, 16)
  * textBox.position = Point(25, 42) //Position is relative to parent
  * rootNode.addChild(textBox)
  *
  * //add button
  * val button = new MCButtonNode
  * button.position = Point(80, 42)
  * button.size = Size(16, 16)
  * button.text = "OK"
  * button.clickDelegate = {() => onMyButtonClicked()} //Click handler
  * rootNode.addChild(button)
  *
  * //... gui ready to present
  *
  * }}}
  *
  * == Using Custom Nodes ==
  * There are several predefined and flexible `TNode` implementations. Custom implementations can also be created
  * if none suit your needs. See [[TNode]] for more information.
  *
  *
  * @constructor
  * @param c The inventory container object that this GUI is representing. Typically a subclass of @class NodeContainer.
  * @param w The width of this GUI window.
  * @param h The height of this GUI window.
  */
//TODO, extract to common trait implementable on a base ContainerNodeGui and a NodeGui as this is not a great setup for non container based guis.
class NodeGui[T <: NodeContainer](c:T = new NodeContainer(null, -1), w:Int, h:Int, inv:PlayerInventory = null, title:ITextComponent = null) extends ContainerScreen[T](c, inv, title) with TNode
{
    /**
      * @constructor Used for creating a default sized GUI window
      * @param c The inventory container object that this GUI is representing. Typically a subclass of @class NodeContainer.
      */
    def this(c:T, inv:PlayerInventory, title:ITextComponent) = this(c, 176, 166, inv, title)

//    /**
//     * @constructor Used for creating a GUI with custom sized window that is not backed by an inventory.
//     * @param w The width of this GUI window.
//     * @param h The height of this GUI window.
//     */
//    def this(w:Int, h:Int) = this(new NodeContainer(null, -1), w, h, null, null)

    imageWidth = w
    imageHeight = h

    /**
      * Flag used for debugging. Enabling will cause all nodes in tree to render visible outline.
      */
    var debugDrawFrames = false

    private var lastClick: Long = 0L

    /** Represents size of the window. Initially set to width and height */
    var size = Size.zeroSize //todo initialize this to xSize x ySize
    override def frame = new Rect(position, size)

    final override def init()
    {
        super.init()
        position = Point(leftPos, topPos)
        if (size == Size.zeroSize) size = Size(imageWidth, imageHeight) //TODO Legacy (size should be set directly)
        else
        {
            imageWidth = size.width
            imageHeight = size.height
        }
    }

    final override def tick()
    {
        super.tick()
        update()
    }

    final override def render(stack:MatrixStack, mouseX: Int, mouseY: Int, partialTicks: Float)
    {
        renderBackground(stack)
        super.render(stack, mouseX, mouseY, partialTicks)
        renderTooltip(stack, mouseX, mouseY)
    }

    final override def init(mc:Minecraft, i:Int, j:Int) {
        val init = this.minecraft == null
        super.init(mc, i, j)
        if (init) onAddedToParent_Impl()
    }

    final override def mouseClicked(x:Double, y:Double, button:Int):Boolean = {
        lastClick = System.currentTimeMillis
        super.mouseClicked(x, y, button)
        mouseClicked(new Point(x.toInt, y.toInt), button, false)
    }


    final override def mouseReleased(x:Double, y:Double, button:Int):Boolean = {
        if (super.mouseReleased(x, y, button)) true
        else if (button != -1) mouseReleased(new Point(x.toInt, y.toInt), button, false)
        else false
    }

    final override def mouseDragged(x: Double, y: Double, button: Int, dragX: Double, dragY: Double): Boolean = {
        val time = System.currentTimeMillis() - lastClick
        super.mouseDragged(x, y, button, dragX, dragY)
        mouseDragged(new Point(x.toInt, y.toInt), button, time, false)
    }

    final override def mouseScrolled(x: Double, y: Double, scroll: Double): Boolean = {
        scroll != 0 &&  mouseScrolled(new Point(x.toInt, y.toInt), if (scroll > 0) 1 else -1, super.mouseScrolled(x, y, scroll))
    }

    override def keyPressed(glfwKeyCode: Int, glfwScanCode: Int, glfwFlags: Int): Boolean = {
        val consumed = super.keyPressed(glfwKeyCode, glfwScanCode, glfwFlags)
        keyPressed('\u0000', glfwKeyCode, consumed)
    }

    override def keyReleased(glfwKeyCode: Int, glfwScanCode: Int, glfwFlags: Int): Boolean = {
        val consumed = super.keyReleased(glfwKeyCode, glfwScanCode, glfwFlags)
        keyReleased('\u0000', glfwKeyCode, consumed)
    }

    /**
      * Used to check if the `keycode` should close the GUI.
      *
      * @param keycode The keycode to check.
      * @return True if this keycode corresponds to a close gui keybind.
      */
//    def isClosingKey(keycode:Int) =
//        keycode == 1 || keycode == minecraft.gameSettings.keyBindInventory.getKey //esc or inv key

    private var lastFrame = 0.0F

    // Front/back rendering overridden, because at root, we dont push the children to our pos, because its zero.
    final override def renderBg(stack:MatrixStack, f:Float, mx:Int, my:Int)
    {
        lastFrame = f
        val mouse = new Point(mx, my)
        frameUpdate(mouse, f)
        RenderSystem.disableDepthTest()
        RenderSystem.color4f(1, 1, 1, 1)
        rootDrawBack(stack, mouse, f)
        RenderSystem.color4f(1, 1, 1, 1)
        RenderSystem.enableDepthTest()
    }

    final override def renderLabels(stack:MatrixStack, mx:Int, my:Int)
    {
        val mouse = new Point(mx, my)
        RenderSystem.disableDepthTest()
        RenderSystem.color4f(1, 1, 1, 1)
        rootDrawFront(stack, mouse, lastFrame)
        RenderSystem.color4f(1, 1, 1, 1)
        RenderSystem.enableDepthTest()

/*        if (debugDrawFrames)
        {
            translate(-position.x, -position.y, 0)
            def render(node:TNode)
            {
                if (!node.hidden)
                {
                    val f = node.frame
                    val absF = Rect(node.parent.convertPointToScreen(f.origin), f.size)
                    GuiDraw.drawLine(absF.x, absF.y, absF.x, absF.maxY, 3, EnumColour.RED.rgba())
                    GuiDraw.drawLine(absF.x, absF.maxY, absF.maxX, absF.maxY, 3, EnumColour.RED.rgba())
                    GuiDraw.drawLine(absF.maxX, absF.maxY, absF.maxX, absF.y, 3, EnumColour.RED.rgba())
                    GuiDraw.drawLine(absF.maxX, absF.y, absF.x, absF.y, 3, EnumColour.RED.rgba())
                }
                for (c <- node.children) render(c)
            }
            for (c <- children) render(c)
            translate(position.x, position.y, 0)
        }*/
    }
}
