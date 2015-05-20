/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.gui.GuiDraw
import codechicken.lib.vec.Translation
import mrtjp.core.color.Colors
import mrtjp.core.gui.{GuiLib, NodeButtonMC, TNode}
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.gui.Gui
import org.lwjgl.input.Keyboard

class CircuitGui(val part:IGuiCircuitPart) extends Gui with TNode
{
    var size = Size.zeroSize
    override def frame = Rect(position, size)

    var lineColor = Colors.LIME.rgb|0xAA000000
    var linePointerCalc:() => Point = Point.zeroPoint _

    private def moverFrame = Rect(position+Point(4, 9), Size(4, 6))
    private var mouseDown = false
    private var mouseInit = Point.zeroPoint

    {
        val close = new NodeButtonMC
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
        GuiDraw.drawRect(moverFrame.x, moverFrame.y, moverFrame.width, moverFrame.height, Colors.LIGHT_GREY.argb)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        val from = linePointerCalc()
        val to = from.clamp(frame)
        GuiLib.drawLine(from.x, from.y, to.x, to.y, lineColor)
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
                for (i <- 0 until otherGuis.size)
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

        RenderICGate.renderDynamic(gate,
            ICComponentStore.orthoGridT(gateRenderSize.width, gateRenderSize.height) `with`
                    new Translation(position.x+gateRenderX, position.y+(size/2-gateRenderSize/2).height, 0), true, rframe)
    }
}

class ICGateGui(override val gate:GateICPart) extends CircuitGui(gate) with TGateGui
{
    size = Size(138, 55)

    {
        val rotate = new NodeButtonMC
        rotate.position = Point(68, 12)
        rotate.size = Size(50, 15)
        rotate.text = "rotate"
        rotate.clickDelegate = {() => gate.sendClientPacket(_.writeByte(0))}
        addChild(rotate)

        val conf = new NodeButtonMC
        conf.position = Point(68, 28)
        conf.size = Size(50, 15)
        conf.text = "configure"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)
    }
}

class ICIOGateGui(override val gate:IOGateICPart) extends CircuitGui(gate) with TGateGui
{
    size = Size(138, 55)

    {
        val conf = new NodeButtonMC
        conf.position = Point(52, 33)
        conf.size = Size(46, 15)
        conf.text = "io mode"
        conf.clickDelegate = {() => gate.sendClientPacket(_.writeByte(1))}
        addChild(conf)

        if (gate.getLogicIO.isInstanceOf[TFreqIOICGateLogic])
        {
            val minus = new NodeButtonMC
            minus.position = Point(52, 15)
            minus.size = Size(14, 14)
            minus.text = "-"
            minus.clickDelegate = {() => gate.sendClientPacket(_.writeByte(6))}
            addChild(minus)

            val plus = new NodeButtonMC
            plus.position = Point(117, 15)
            plus.size = Size(14, 14)
            plus.text = "+"
            plus.clickDelegate = {() => gate.sendClientPacket(_.writeByte(5))}
            addChild(plus)
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        super.drawBack_Impl(mouse, rframe)
        fontRenderer.drawString(gate.shape match
        {
            case 0 => "input"
            case 1 => "output"
            case 2 => "inout"
        }, position.x+100, position.y+36, 0xFFFFFFFF, true)

        if (gate.getLogicIO.isInstanceOf[TFreqIOICGateLogic])
        {
            GuiDraw.drawStringC("freq", position.x+66, position.y+4, 50, 14, 0xFFFFFFFF, true)
            GuiDraw.drawStringC(gate.getLogic[TFreqIOICGateLogic].getFreqName, position.x+66, position.y+15, 50, 14, 0xFFFFFFFF, true)
        }
    }
}