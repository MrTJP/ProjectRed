/*
/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import codechicken.lib.gui.GuiDraw
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.gui.GuiScreen
import net.minecraft.util.ChatAllowedCharacters
import org.lwjgl.input.Keyboard

/**
  * A node representing a simple text box used to type in text.
  *
  * @constructor
  * @param x The top left x coordinate.
  * @param y The top left y coordinate.
  * @param w The width.
  * @param h The height.
  * @param tq The initial text.
  */
class SimpleTextboxNode(x:Int, y:Int, w:Int, h:Int, tq:String) extends TNode
{
    def this(x:Int, y:Int, w:Int, h:Int) = this(x, y, w, h, "")
    def this() = this(0, 0, 0, 0, "")

    position = Point(x, y)
    text = tq

    /**
      * If enabled, the text box will be focus-able and will respond to events. When not enabled, existing text,
      * if any, will render in a grey colour.
      */
    var enabled = true
    /**
      * True if the text box is currently focused. A focused text box will consume keyboard events to type to
      * its text field.
      */
    var focused = false

    /** The current text of the text box. */
    var text = ""

    /** Phantom text that is shown when the text box is empty. */
    var phantom = ""
    /** A string of allowed characters. If empty, any character will be allowed. */
    var allowedcharacters = ""

    /** A callback function called when the text changes. */
    var textChangedDelegate = {() => }
    /** A callback function called when the `RETURN` key is pressed while typing in the text box. */
    var textReturnDelegate = {() => }

    /** A callback function called when the focus of the test box changes. */
    var focusChangeDelegate = {() => }

    /** Used internally to render the blinking cursor. */
    private var cursorCounter = 0

    /** Represents the size of the text box. */
    var size = Size(w, h)
    override def frame = Rect(position, size)


    /**
      * Used to set the text of the box.
      *
      * @param t The text to set.
      * @todo Make this private. Externally, [[text]] should be set directly.
      */
    def setText(t:String)
    {
        val old = text
        text = t
        if (old != text) textChangedDelegate()
    }

    /**
      * Sets the [[focused]] property of this text box.
      */
    def setFocused(flag:Boolean)
    {
        if (focused != flag)
        {
            focused = flag
            if (focused) cursorCounter = 0
            focusChangeDelegate()
        }
    }

    override def update_Impl(){cursorCounter += 1}

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean):Boolean =
    {
        if (enabled && focused && !consumed)
        {
            if (keycode == 1)//esc
            {
                setFocused(false)
                return true
            }

            if (c == '\u0016') //paste
            {
                val s = GuiScreen.getClipboardString
                if (s == null || s.isEmpty) return true
                for (c <- s) if (!tryAddChar(c)) return true
            }

            if (keycode == Keyboard.KEY_RETURN) //enter
            {
                setFocused(false)
                textReturnDelegate()
                return true
            }

            if (keycode == Keyboard.KEY_BACK) tryBackspace() else tryAddChar(c)

            true
        }
        else false
    }

    private def canAddChar(c:Char) =
        if (allowedcharacters.isEmpty) ChatAllowedCharacters.isAllowedCharacter(c)
        else allowedcharacters.indexOf(c) >= 0

    private def tryAddChar(c:Char):Boolean =
    {
        if (!canAddChar(c)) return false
        val ns = text+c
        if (GuiDraw.getStringWidth(ns) > size.width-8) return false
        setText(ns)
        true
    }

    private def tryBackspace():Boolean =
    {
        if (!text.isEmpty)
        {
            setText(text.substring(0, text.length-1))
            true
        }
        else false
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && enabled && rayTest(p))
        {
            setFocused(true)
            if (button == 1) setText("")
            true
        }
        else
        {
            setFocused(false)
            false
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        GuiDraw.drawRect(position.x-1, position.y-1, size.width+1, size.height+1, 0xFFA0A0A0) //todo make these colors configurable properties
        GuiDraw.drawRect(position.x, position.y, size.width, size.height, 0xFF000000)

        if (text.isEmpty && phantom.nonEmpty)
            GuiDraw.drawString(phantom, position.x+4, position.y+size.height/2-4, 0x404040)

        val drawText = text+(if (enabled && focused && cursorCounter/6%2 == 0) "_" else "")
        GuiDraw.drawString(drawText, position.x+4, position.y+size.height/2-4, if (enabled) 0xE0E0E0 else 0x707070)
    }
}*/
