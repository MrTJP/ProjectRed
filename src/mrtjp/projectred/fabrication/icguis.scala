/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Translation
import mrtjp.core.gui.{GuiLib, MCButtonNode, TNode}
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.gui.Gui
import org.lwjgl.input.Keyboard
import org.lwjgl.opengl.GL11

class CircuitGui(val part:IGuiCircuitPart) extends Gui with TNode
{
    var size = Size.zeroSize
    override def frame = Rect(position, size)

    var lineColor = EnumColour.LIME.argb(0xAA)
    var linePointerCalc = {() => Point.zeroPoint}

    private def moverFrame = Rect(position+Point(4, 9), Size(4, 6))
    private var mouseDown = false
    private var mouseInit = Point.zeroPoint

    {
        val close = new MCButtonNode
        close.position = Point(4, 4)
        close.size = Size(5, 5)
        close.clickDelegate = {() => removeFromParent()}
        addChild(close)
    }

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        if (mouseDown)
        {
            position += mouse-mouseInit
            mouseInit = mouse
        }

        if (part.world == null) removeFromParent()
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        GuiLib.drawGuiBox(position.x, position.y, size.width, size.height, 0)
        GuiDraw.drawRect(moverFrame.x, moverFrame.y, moverFrame.width, moverFrame.height, EnumColour.LIGHT_GRAY.argb)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        val from = linePointerCalc()
        val to = from.clamp(frame)
        GL11.glColor4d(1, 1, 1, 1)
        GuiDraw.drawLine(from.x, from.y, to.x, to.y, 2, lineColor)
        GuiDraw.drawRect(to.x-3, to.y-3, 6, 6, lineColor)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        if (parent == null) false //we cant check for consume here, so manually check if closed
        else hitTest(p).find(_.isInstanceOf[CircuitGui]) match
        {
            case Some(gui) if gui == this =>
                val guis = parent.childrenByZ.collect{case g:CircuitGui => g}
                val otherGuis = guis.filter(_ != this)
                for (i <- otherGuis.indices)
                    otherGuis(i).pushZTo(0.1*i)
                pushZTo(0.1*otherGuis.size)

                if (moverFrame.contains(p))
                {
                    mouseDown = true
                    mouseInit = p
                }
                true
            case _ => false
        }
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        mouseDown = false
        false
    }

    override def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) =
        if (!consumed && keycode == Keyboard.KEY_ESCAPE)
        {
            removeFromParent()
            true
        }
        else false
}

trait TGateGui extends CircuitGui
{
    var gateRenderSize = Size(40, 40)
    var gateRenderX = 10

    def gate:GateICPart

    abstract override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)

        RenderICGate.renderDynamic(CCRenderState.instance(), gate,
            ICComponentStore.orthoGridT(gateRenderSize.width, gateRenderSize.height) `with`
                    new Translation(position.x+gateRenderX, position.y+(size/2-gateRenderSize/2).height, 0), true, rframe)
    }
}

class ICGateGui(override val gate:GateICPart) extends CircuitGui(gate) with TGateGui
{
    {
        size = Size(120, 55)

        val rotate = new MCButtonNode
        rotate.position = Point(58, 12)
        rotate.size = Size(50, 15)
        rotate.text = "rotate"
        rotate.clickDelegate = {() => gate.sendClientPacket(_.writeByte(0))}
        addChild(rotate)

        val conf = new MCButtonNode
        conf.position = Point(58, 28)
        conf.size = Size(50, 15)
        conf.text = "configure"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)
    }
}

class ICTimerGateGui(override val gate:SequentialGateICPart) extends CircuitGui(gate) with TGateGui
{
    {
        size = Size(160, 80)

        val ax = 54
        val aw = 50
        val ah = 15

        val rotate = new MCButtonNode
        rotate.position = Point(ax, 5)
        rotate.size = Size(aw, ah)
        rotate.text = "rotate"
        rotate.clickDelegate = {() => gate.sendClientPacket(_.writeByte(0))}
        addChild(rotate)

        val conf = new MCButtonNode
        conf.position = Point(ax+aw+1, 5)
        conf.size = Size(aw, ah)
        conf.text = "configure"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)

        def createButton(x:Int, y:Int, w:Int, h:Int, text:String, delta:Int)
        {
            val b = new MCButtonNode
            b.position = Point(x, y)
            b.size = Size(w, h)
            b.text = text
            b.clickDelegate = {() => gate.sendClientPacket(_.writeByte(3).writeShort(delta))}
            addChild(b)
        }

        val bw = 32
        val bh = 12
        val r1x = 69
        val r2x = r1x+35
        val by = 34
        val bdy = 14

        createButton(r1x, by+(0*bdy), bw, bh, "-50ms", -1)
        createButton(r1x, by+(1*bdy), bw, bh, "-1s", -20)
        createButton(r1x, by+(2*bdy), bw, bh, "-10s", -200)

        createButton(r2x, by+(0*bdy), bw, bh, "+50ms", 1)
        createButton(r2x, by+(1*bdy), bw, bh, "+1s", 20)
        createButton(r2x, by+(2*bdy), bw, bh, "+10s", 200)
    }

    def getLogic = gate.getLogic[ITimerGuiLogic]

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        val s = "Interval: "+"%.2f".format(getLogic.getTimerMax*0.05)+"s"
        GuiDraw.drawStringC(s, position.x+102, position.y+24, EnumColour.GRAY.argb, false)
    }
}

class ICCounterGateGui(override val gate:SequentialGateICPart) extends CircuitGui(gate) with TGateGui
{
    var valID = 0

    {
        size = Size(160, 94)

        val ax = 54
        val aw = 50
        val ah = 15

        val rotate = new MCButtonNode
        rotate.position = Point(ax, 5)
        rotate.size = Size(aw, ah)
        rotate.text = "rotate"
        rotate.clickDelegate = {() => gate.sendClientPacket(_.writeByte(0))}
        addChild(rotate)

        val conf = new MCButtonNode
        conf.position = Point(ax+aw+1, 5)
        conf.size = Size(aw, ah)
        conf.text = "configure"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)

        val sw = new MCButtonNode
        sw.position = Point(54, 28)
        sw.size = Size(20, 12)
        sw.text = "var"
        sw.clickDelegate = {() => valID = (valID+1)%3}
        addChild(sw)

        def createButton(x:Int, y:Int, w:Int, h:Int, delta:Int)
        {
            val b = new MCButtonNode
            b.position = Point(x, y)
            b.size = Size(w, h)
            b.text = (if (delta < 0) "" else "+")+delta
            b.clickDelegate = {() => gate.sendClientPacket(_.writeByte(4).writeByte(valID).writeShort(delta))}
            addChild(b)
        }

        val bw = 32
        val bh = 12
        val r1x = 69
        val r2x = r1x+35
        val by = 48
        val bdy = 14

        createButton(r1x, by+(0*bdy), bw, bh, -1)
        createButton(r1x, by+(1*bdy), bw, bh, -5)
        createButton(r1x, by+(2*bdy), bw, bh, -10)

        createButton(r2x, by+(0*bdy), bw, bh, 1)
        createButton(r2x, by+(1*bdy), bw, bh, 5)
        createButton(r2x, by+(2*bdy), bw, bh, 10)
    }

    def getLogic = gate.getLogic[ICounterGuiLogic]

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        val s = "State: "+getLogic.getCounterValue
        GuiDraw.drawStringC(s, position.x+102, position.y+24, EnumColour.GRAY.argb, false)

        val m = valID match {
            case 0 => "Max: "+getLogic.getCounterMax
            case 1 => "Incr: "+getLogic.getCounterIncr
            case 2 => "Decr: "+getLogic.getCounterDecr
        }
        GuiDraw.drawStringC(m, position.x+102, position.y+36, EnumColour.GRAY.argb, false)
    }
}

class ICIOGateGui(override val gate:IOGateICPart) extends CircuitGui(gate) with TGateGui
{
    {
        size = Size(124, 55)

        val conf = new MCButtonNode
        conf.position = Point(62, 33)
        conf.size = Size(46, 15)
        conf.text = "io mode"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)

        GuiDraw.drawStringC(gate.shape match
        {
            case 0 => "input"
            case 1 => "output"
            case 2 => "inout"
        }, position.x+85, position.y+16, EnumColour.GRAY.argb, false)
    }
}

class ICIOFreqGateGui(override val gate:IOGateICPart) extends CircuitGui(gate) with TGateGui
{
    {
        size = Size(138, 55)

        val conf = new MCButtonNode
        conf.position = Point(52, 7)
        conf.size = Size(46, 15)
        conf.text = "io mode"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)

        val minus = new MCButtonNode
        minus.position = Point(52, 33)
        minus.size = Size(14, 14)
        minus.text = "-"
        minus.clickDelegate = {() => gate.sendClientPacket(_.writeByte(6))}
        addChild(minus)

        val plus = new MCButtonNode
        plus.position = Point(117, 33)
        plus.size = Size(14, 14)
        plus.text = "+"
        plus.clickDelegate = {() => gate.sendClientPacket(_.writeByte(5))}
        addChild(plus)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)

        GuiDraw.drawStringC(gate.shape match
        {
            case 0 => "input"
            case 1 => "output"
            case 2 => "inout"
        }, position.x+117, position.y+11, EnumColour.GRAY.argb, false)

        GuiDraw.drawStringC("freq", position.x+66, position.y+22, 50, 14, EnumColour.GRAY.argb, false)
        GuiDraw.drawStringC(gate.getLogic[TFreqIOICGateLogic].getFreqName, position.x+66, position.y+33, 50, 14, EnumColour.GRAY.argb, false)
    }
}