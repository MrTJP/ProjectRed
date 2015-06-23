/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.vec.Transformation
import mrtjp.core.color.Colors
import mrtjp.projectred.fabrication.ICComponentStore.{generateWireModels, _}
import net.minecraft.client.renderer.texture.IIconRegister

object RenderICGate
{
    var renderers = buildRenders()

    def buildRenders() = Seq[ICGateRenderer[_]](
        new RenderSimpleIO,
        new RenderAnalogIO,
        new RenderBundledIO,
        new RenderOR,
        new RenderNOR,
        new RenderNOT,
        new RenderAND,
        new RenderNAND,
        new RenderXOR,
        new RenderXNOR,
        new RenderBuffer,
        new RenderMultiplexer,
        new RenderPulse,
        new RenderRepeater
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

class RenderNOR extends ICGateRenderer[ComboGateICPart]
{
    var wires = generateWireModels("NOR", 4)
    var torch = new RedstoneTorchModel(8, 9)

    override val coreModels = Seq(new BaseComponentModel("NOR"))++wires:+torch

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(1).disabled = false
        wires(2).disabled = false
        wires(3).disabled = false
        torch.on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x11) != 0
        wires(1).on = (gate.state&2) != 0
        wires(2).on = (gate.state&4) != 0
        wires(3).on = (gate.state&8) != 0
        wires(1).disabled = (gate.shape&1) != 0
        wires(2).disabled = (gate.shape&2) != 0
        wires(3).disabled = (gate.shape&4) != 0
        torch.on = (gate.state&0xE) == 0
    }
}

class RenderNOT extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("NOT", 4)
    val torch = new RedstoneTorchModel(8, 8)

    override val coreModels = Seq(new BaseComponentModel("NOT"))++wires:+torch

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = true
        wires(2).on = false
        wires(3).on = true
        wires(0).disabled = false
        wires(1).disabled = false
        wires(3).disabled = false
        torch.on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x11) != 0
        wires(1).on = (gate.state&0x22) != 0
        wires(2).on = (gate.state&4) != 0
        wires(3).on = (gate.state&0x88) != 0
        wires(0).disabled = (gate.shape&2) != 0
        wires(1).disabled = (gate.shape&1) != 0
        wires(3).disabled = (gate.shape&4) != 0
        torch.on = (gate.state&0xF0) != 0
    }
}

class RenderAND extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("AND", 4)
    val torches = Seq(new RedstoneTorchModel(4, 8), new RedstoneTorchModel(12, 8),
        new RedstoneTorchModel(8, 8), new RedstoneTorchModel(8, 2))

    override val coreModels = Seq(new BaseComponentModel("AND"))++wires++torches

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
        torches(1).on = true
        torches(2).on = true
        torches(3).on = false
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x11) == 0
        wires(3).on = (gate.state&2) != 0
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&8) != 0
        wires(3).disabled = (gate.shape&1) != 0
        wires(1).disabled = (gate.shape&2) != 0
        wires(2).disabled = (gate.shape&4) != 0
        torches(2).on = !wires(1).on && !wires(1).disabled
        torches(0).on = !wires(2).on && !wires(2).disabled
        torches(1).on = !wires(3).on && !wires(3).disabled
        torches(3).on = !wires(0).on
    }
}

class RenderNAND extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("NAND", 4)
    val torches = Seq(new RedstoneTorchModel(4, 8), new RedstoneTorchModel(12, 8),
        new RedstoneTorchModel(8, 8))

    override val coreModels = Seq(new BaseComponentModel("NAND"))++wires++torches

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
        torches(1).on = true
        torches(2).on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x11) != 0
        wires(3).on = (gate.state&2) != 0
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&8) != 0
        wires(3).disabled = (gate.shape&1) != 0
        wires(1).disabled = (gate.shape&2) != 0
        wires(2).disabled = (gate.shape&4) != 0
        torches(0).on = !wires(2).on && !wires(2).disabled
        torches(1).on = !wires(3).on && !wires(3).disabled
        torches(2).on = !wires(1).on && !wires(1).disabled
    }
}

class RenderXOR extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("XOR", 4)
    val torches = Seq(new RedstoneTorchModel(4.5, 8), new RedstoneTorchModel(11.5, 8),
        new RedstoneTorchModel(8, 12))

    override val coreModels = Seq(new BaseComponentModel("XOR"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = false
        wires(3).on = false
        wires(2).on = false
        wires(1).on = true
        torches(0).on = false
        torches(1).on = false
        torches(2).on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x11) != 0
        wires(3).on = (gate.state&2) != 0
        wires(2).on = (gate.state&8) != 0
        wires(1).on = !wires(3).on && !wires(2).on
        torches(0).on = !wires(2).on && !wires(1).on
        torches(1).on = !wires(3).on && !wires(1).on
        torches(2).on = wires(1).on
    }
}

class RenderXNOR extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("XNOR", 5)
    val torches = Seq(new RedstoneTorchModel(8, 2), new RedstoneTorchModel(4.5, 8),
        new RedstoneTorchModel(11.5, 8), new RedstoneTorchModel(8, 12))

    override val coreModels = Seq(new BaseComponentModel("XNOR"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = false
        wires(3).on = false
        wires(2).on = false
        wires(1).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = false
        torches(3).on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&2) != 0 && (gate.state&8) == 0
        wires(1).on = (gate.state&8) != 0 && (gate.state&2) == 0
        wires(2).on = (gate.state&8) != 0
        wires(3).on = (gate.state&2) != 0
        wires(4).on = !wires(3).on && !wires(2).on
        torches(0).on = (gate.state&0x11) != 0
        torches(1).on = !wires(4).on && (gate.state&8) == 0
        torches(2).on = !wires(4).on && (gate.state&2) == 0
        torches(3).on = (gate.state&2) == 0 && (gate.state&8) == 0
    }
}

class RenderBuffer extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("BUFFER", 4)
    val torches = Seq(new RedstoneTorchModel(8, 3.5), new RedstoneTorchModel(8, 9))

    override val coreModels = Seq(new BaseComponentModel("BUFFER"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(1).disabled = false
        wires(3).disabled = false
        torches(0).on = false
        torches(1).on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&4) == 0
        wires(1).on = (gate.state&0x22) != 0
        wires(2).on = (gate.state&0x44) != 0
        wires(3).on = (gate.state&0x88) != 0
        wires(1).disabled = (gate.shape&1) != 0
        wires(3).disabled = (gate.shape&2) != 0
        torches(0).on = (gate.state&4) != 0
        torches(1).on = (gate.state&4) == 0
    }
}

class RenderMultiplexer extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("MULTIPLEXER", 6)
    val torches = Seq(new RedstoneTorchModel(8, 2), new RedstoneTorchModel(9, 10.5),
        new RedstoneTorchModel(4.5, 8), new RedstoneTorchModel(11.5, 8))

    override val coreModels = Seq(new BaseComponentModel("MULTIPLEXER"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = true
        wires(2).on = true
        wires(3).on = false
        wires(4).on = false
        wires(5).on = false
        torches(0).on = false
        torches(1).on = true
        torches(2).on = false
        torches(3).on = true
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(2).on = (gate.state&4) == 0
        wires(3).on = (gate.state&4) != 0
        wires(4).on = (gate.state&8) != 0
        wires(5).on = (gate.state&2) != 0
        torches(0).on = (gate.state&0x10) != 0
        torches(1).on = !wires(3).on
        torches(2).on = (gate.state&8) == 0 && wires(3).on
        torches(3).on = (gate.state&4) == 0 && !wires(5).on
        wires(0).on = torches(2).on
        wires(1).on = torches(1).on
    }
}

class RenderPulse extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("PULSE", 3)
    val torches = Seq(new RedstoneTorchModel(4, 9.5), new RedstoneTorchModel(11, 9.5),
        new RedstoneTorchModel(8, 3.5))

    override val coreModels = Seq(new BaseComponentModel("PULSE"))++wires++torches

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = false
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&4) == 0
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&0x14) == 4
        torches(0).on = wires(0).on
        torches(1).on = wires(1).on
        torches(2).on = (gate.state&0x10) != 0
    }
}

class RenderRepeater extends ICGateRenderer[ComboGateICPart]
{
    val wires = generateWireModels("REPEATER", 2)
    val endTorch = new RedstoneTorchModel(8, 2)
    val varTorches = Seq(new RedstoneTorchModel(12.5, 12), new RedstoneTorchModel(12.5, 11),
        new RedstoneTorchModel(12.5, 10), new RedstoneTorchModel(12.5, 9), new RedstoneTorchModel(12.5, 8),
        new RedstoneTorchModel(12.5, 7), new RedstoneTorchModel(12.5, 6), new RedstoneTorchModel(12.5, 5),
        new RedstoneTorchModel(12.5, 4))

    var shape = 0

    override val coreModels = Seq(new BaseComponentModel("REPEATER"))++wires:+endTorch

    override def switchModels = Seq(varTorches(shape))

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        endTorch.on = true
        shape = 0
        varTorches(0).on = false
    }

    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
    {
        wires(0).on = (gate.state&0x10) == 0
        wires(1).on = (gate.state&4) != 0
        endTorch.on = (gate.state&0x10) != 0
        shape = gate.shape
        varTorches(shape).on = (gate.state&4) == 0
    }
}