package mrtjp.projectred.core.libmc.gui

import net.minecraft.util.ChatAllowedCharacters
import net.minecraft.client.gui.{Gui, GuiScreen}
import org.lwjgl.input.Keyboard

class WidgetTextBox(x:Int, y:Int, w:Int, h:Int, var text:String) extends TWidget
{
    def this(x:Int, y:Int, w:Int, h:Int) = this(x, y, w, h, "")

    var isFocused = false
    private var isEnabled = true
    private var cursorCounter = 0
    private var action = ""

    private var allowedcharacters = new String(ChatAllowedCharacters.allowedCharacters)
    var maxStringLength = 0

    override val bounds = new Rect().setMin(x, y).setWH(w, h)

    def setAction(a:String):this.type = {action = a; this}

    def setText(t:String):this.type =
    {
        val old = text
        text = t
        if (old != text) onTextChanged(old)
        this
    }

    def onTextChanged(old:String){}

    def setEnabled(flag:Boolean):this.type =
    {
        isEnabled = flag
        if (!isEnabled && isFocused) setFocused(false)
        this
    }

    def setFocused(flag:Boolean):this.type =
    {
        if (isFocused != flag)
        {
            isFocused = flag
            onFocusChanged()
        }
        this
    }

    def onFocusChanged()
    {
        if (isFocused) cursorCounter = 0
    }

    def canAddChar(c:Char) = allowedcharacters.indexOf(c) >= 0

    override def update_Impl(){cursorCounter += 1}

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean):Boolean =
    {
        if (isEnabled && isFocused)
        {
            if (c == '\026') //paste
            {
                val s = GuiScreen.getClipboardString
                if (s == null || (s == "")) return true
                for (i <- 0 until s.length)
                {
                    if (text.length == maxStringLength) return true
                    val tc = s.charAt(i)
                    if (canAddChar(tc)) setText(text+tc)
                }
            }

            if (keycode == Keyboard.KEY_RETURN) //enter
            {
                setFocused(false)
                startMessageChain(action)
            }

            if (keycode == Keyboard.KEY_BACK && text.length > 0) setText(text.substring(0, text.length-1))
            if ((text.length < maxStringLength || maxStringLength == 0) && canAddChar(c)) setText(text+c)

            true
        }
        else false
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        if (!consumed && isEnabled && bounds.intersects(p))
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

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        Gui.drawRect(x-1, y-1, x+bWidth+1, y+bHeight+1, 0xffa0a0a0)
        Gui.drawRect(x, y, x+bWidth, y+bHeight, 0xff000000)

        drawString(fontRenderer, getDrawText, x+4, y+bHeight/2-4, if (isEnabled) 0xe0e0e0 else 0x707070)

        def getDrawText =
        {
            var s = text
            if (isEnabled && isFocused && cursorCounter/6%2 == 0) s += "_"
            s
        }
    }

    def setMaxCharCount(i:Int):this.type = {maxStringLength = i; this}
    def setAllowedChars(s:String):this.type = {allowedcharacters = s; this}
}
