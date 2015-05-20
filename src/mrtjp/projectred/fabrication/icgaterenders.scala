/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.vec.Transformation
import mrtjp.core.color.Colors
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraft.client.renderer.texture.IIconRegister

object RenderICGate
{
    var renderers = buildRenders()

    def buildRenders() = Seq[ICGateRenderer[_]](
        new RenderSimpleIO,
        new RenderAnalogIO,
        new RenderBundledIO,
        new RenderOR
    )

    def registerIcons(reg:IIconRegister){}

    def renderDynamic(gate:GateICPart, t:Transformation, ortho:Boolean, frame:Float)
    {
        val r = renderers(gate.subID).asInstanceOf[ICGateRenderer[GateICPart]]
        r.prepareDynamic(gate, frame)
        r.renderDynamic(gate.rotationT `with` t, ortho)
    }

    def renderInv(t:Transformation, id:Int)
    {
        val r = renderers(id)
        r.prepareInv()
        r.renderDynamic(t, true)
    }
}

abstract class ICGateRenderer[T <: GateICPart]
{
    var reflect = false

    def coreModels:Seq[ICComponentModel]
    def switchModels = Seq[ICComponentModel]()

    def prepareInv(){}
    def prepareDynamic(gate:T, frame:Float){}

    def renderDynamic(t:Transformation, ortho:Boolean)
    {
        renderModels(t, if (reflect) 1 else 0, ortho)
    }

    def renderModels(t:Transformation, orient:Int, ortho:Boolean)
    {
        prepairRender()
        for (m <- coreModels++switchModels) m.renderModel(t, orient, ortho)
        finishRender()
    }
}

abstract class RenderIO extends ICGateRenderer[IOGateICPart]
{
    val wires = generateWireModels("IOSIMP", 1)
    val iosig = new IOSigModel

    override val coreModels = Seq(new BaseComponentModel("IOSIMP"))++wires:+iosig

    override def prepareInv()
    {
        wires(0).on = false
        iosig.on = false
        iosig.colour = invColour
    }

    override def prepareDynamic(gate:IOGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x44) != 0
        iosig.on = wires(0).on
        iosig.colour = dynColour(gate)
    }


    def invColour:Int
    def dynColour(gate:IOGateICPart):Int
}

class RenderSimpleIO extends RenderIO
{
    override def invColour = signalColour(0.toByte)
    override def dynColour(gate:IOGateICPart) = signalColour((if (iosig.on) 255 else 0).toByte)
}

class RenderAnalogIO extends RenderIO
{
    override def invColour = signalColour(0.toByte)
    override def dynColour(gate:IOGateICPart) = signalColour((gate.getLogic[AnalogIOICGateLogic].freq*17).toByte)
}

class RenderBundledIO extends RenderIO
{
    override def invColour = Colors(0).rgba
    override def dynColour(gate:IOGateICPart) = Colors(gate.getLogic[AnalogIOICGateLogic].freq).rgba
}

class RenderOR extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("OR", 4)
    val torches = Seq(new RedstoneTorchModel(8, 9), new RedstoneTorchModel(8, 2.5))

    override val coreModels = Seq(new BaseComponentModel("OR"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(1).disabled = false
        wires(2).disabled = false
        wires(3).disabled = false
        torches(0).on = true
        torches(1).on = false
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x10) == 0
        wires(1).on = (gate.state&2) != 0
        wires(2).on = (gate.state&4) != 0
        wires(3).on = (gate.state&8) != 0
        wires(1).disabled = (gate.shape&1) != 0
        wires(2).disabled = (gate.shape&2) != 0
        wires(3).disabled = (gate.shape&4) != 0
        torches(0).on = (gate.state&0xE) == 0
        torches(1).on = !wires(0).on
    }
}