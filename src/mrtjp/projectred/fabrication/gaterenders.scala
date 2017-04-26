/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.math.MathHelper
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Transformation
import mrtjp.projectred.core.TFaceOrient._
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraft.client.renderer.texture.TextureMap

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
        new RenderBuffer
//        new RenderMultiplexer,
//        new RenderPulse,
//        new RenderRepeater,
//        new RenderRandomizer,
//        new RenderSRLatch,
//        new RenderToggleLatch,
//        new RenderTransparentLatch,
//        new RenderTimer,
//        new RenderSequencer,
//        new RenderCounter,
//        new RenderStateCell,
//        new RenderSynchronizer,
//        new RenderDecRandomizer,
//        new RenderNullCell,
//        new RenderInvertCell,
//        new RenderBufferCell
    )

    def registerIcons(reg:TextureMap){}

    def renderDynamic(ccrs:CCRenderState, gate:GateICTile, t:Transformation, ortho:Boolean, frame:Float)
    {
        val r = renderers(gate.subID).asInstanceOf[ICGateRenderer[GateICTile]]
        r.prepareDynamic(gate, frame)
        r.renderDynamic(ccrs, gate.rotationT `with` t, ortho)
    }

    def renderInv(ccrs:CCRenderState, t:Transformation, id:Int)
    {
        val r = renderers(id)
        r.prepareInv()
        r.renderDynamic(ccrs, t, true)
    }
}

abstract class ICGateRenderer[T <: GateICTile]
{
    var reflect = false

    def coreModels:Seq[ICComponentModel]
    def switchModels = Seq[ICComponentModel]()

    def prepareInv(){}
    def prepareDynamic(gate:T, frame:Float){}

    def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean)
    {
        renderModels(ccrs, t, if (reflect) 1 else 0, ortho)
    }

    def renderModels(ccrs:CCRenderState, t:Transformation, orient:Int, ortho:Boolean)
    {
        prepairRender(ccrs)
        for (m <- coreModels++switchModels) m.renderModel(ccrs, t, orient, ortho)
        finishRender(ccrs)
    }
}

abstract class RenderIO extends ICGateRenderer[IOGateICTile]
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

    override def prepareDynamic(gate:IOGateICTile, frame:Float)
    {
        wires(0).on = (gate.state&0x44) != 0
        iosig.on = wires(0).on
        iosig.colour = dynColour(gate)
    }


    def invColour:Int
    def dynColour(gate:IOGateICTile):Int
}

class RenderSimpleIO extends RenderIO
{
    override def invColour = signalColour(0.toByte)
    override def dynColour(gate:IOGateICTile) = signalColour((if (iosig.on) 255 else 0).toByte)
}

class RenderAnalogIO extends RenderIO
{
    override def invColour = signalColour(0.toByte)
    override def dynColour(gate:IOGateICTile) = signalColour((gate.getLogic[AnalogIOICGateLogic].freq*17).toByte)
}

class RenderBundledIO extends RenderIO
{
    override def invColour = EnumColour.WHITE.rgba
    override def dynColour(gate:IOGateICTile) = EnumColour.values()(gate.getLogic[AnalogIOICGateLogic].freq).rgba
}

class RenderOR extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderNOR extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderNOT extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderAND extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderNAND extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderXOR extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderXNOR extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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

class RenderBuffer extends ICGateRenderer[ComboGateICTile]
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

    override def prepareDynamic(gate:ComboGateICTile, frame:Float)
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
//
//class RenderMultiplexer extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("MULTIPLEXER", 6)
//    val torches = Seq(new RedstoneTorchModel(8, 2), new RedstoneTorchModel(9, 10.5),
//        new RedstoneTorchModel(4.5, 8), new RedstoneTorchModel(11.5, 8))
//
//    override val coreModels = Seq(new BaseComponentModel("MULTIPLEXER"))++wires++torches
//
//    override def prepareInv()
//    {
//        wires(0).on = false
//        wires(1).on = true
//        wires(2).on = true
//        wires(3).on = false
//        wires(4).on = false
//        wires(5).on = false
//        torches(0).on = false
//        torches(1).on = true
//        torches(2).on = false
//        torches(3).on = true
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        wires(2).on = (gate.state&4) == 0
//        wires(3).on = (gate.state&4) != 0
//        wires(4).on = (gate.state&8) != 0
//        wires(5).on = (gate.state&2) != 0
//        torches(0).on = (gate.state&0x10) != 0
//        torches(1).on = !wires(3).on
//        torches(2).on = (gate.state&8) == 0 && wires(3).on
//        torches(3).on = (gate.state&4) == 0 && !wires(5).on
//        wires(0).on = torches(2).on
//        wires(1).on = torches(1).on
//    }
//}
//
//class RenderPulse extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("PULSE", 3)
//    val torches = Seq(new RedstoneTorchModel(4, 9.5), new RedstoneTorchModel(11, 9.5),
//        new RedstoneTorchModel(8, 3.5))
//
//    override val coreModels = Seq(new BaseComponentModel("PULSE"))++wires++torches
//
//    override def prepareInv()
//    {
//        wires(0).on = true
//        wires(1).on = false
//        wires(2).on = false
//        torches(0).on = true
//        torches(1).on = false
//        torches(2).on = false
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        wires(0).on = (gate.state&4) == 0
//        wires(1).on = (gate.state&4) != 0
//        wires(2).on = (gate.state&0x14) == 4
//        torches(0).on = wires(0).on
//        torches(1).on = wires(1).on
//        torches(2).on = (gate.state&0x10) != 0
//    }
//}
//
//class RenderRepeater extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("REPEATER", 2)
//    val endTorch = new RedstoneTorchModel(8, 2)
//    val varTorches = Seq(new RedstoneTorchModel(12.5, 12), new RedstoneTorchModel(12.5, 11),
//        new RedstoneTorchModel(12.5, 10), new RedstoneTorchModel(12.5, 9), new RedstoneTorchModel(12.5, 8),
//        new RedstoneTorchModel(12.5, 7), new RedstoneTorchModel(12.5, 6), new RedstoneTorchModel(12.5, 5),
//        new RedstoneTorchModel(12.5, 4))
//
//    var shape = 0
//
//    override val coreModels = Seq(new BaseComponentModel("REPEATER"))++wires:+endTorch
//
//    override def switchModels = Seq(varTorches(shape))
//
//    override def prepareInv()
//    {
//        wires(0).on = true
//        wires(1).on = false
//        endTorch.on = false
//        shape = 0
//        varTorches(0).on = true
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        wires(0).on = (gate.state&0x10) == 0
//        wires(1).on = (gate.state&4) != 0
//        endTorch.on = (gate.state&0x10) != 0
//        shape = gate.shape
//        varTorches(shape).on = (gate.state&4) == 0
//    }
//}
//
//class RenderRandomizer extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("RAND", 7)
//    val chips = Seq(new YellowChipModel(8, 5.5), new YellowChipModel(11.5, 11.5), new YellowChipModel(4.5, 11.5))
//
//    override val coreModels = Seq(new BaseComponentModel("RAND"))++wires++chips
//
//    override def prepareInv()
//    {
//        wires(0).on = false
//        wires(1).on = false
//        wires(2).on = false
//        wires(3).on = false
//        wires(4).on = false
//        wires(5).on = false
//        wires(6).on = false
//        wires(0).disabled = false
//        wires(1).disabled = false
//        wires(3).disabled = false
//        wires(4).disabled = false
//        wires(5).disabled = false
//        wires(6).disabled = false
//        chips(0).on = false
//        chips(1).on = false
//        chips(2).on = false
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        wires(2).on = (gate.state&4) != 0
//        wires(0).on = (gate.state&0x11) != 0
//        wires(1).on = (gate.state&0x22) != 0
//        wires(3).on = (gate.state&0x88) != 0
//        wires(4).on = wires(2).on
//        wires(5).on = wires(2).on
//        wires(6).on = wires(2).on
//        wires(1).disabled = (gate.shape&1) != 0
//        wires(0).disabled = (gate.shape&2) != 0
//        wires(3).disabled = (gate.shape&4) != 0
//        wires(5).disabled = wires(1).disabled
//        wires(4).disabled = wires(0).disabled
//        wires(6).disabled = wires(3).disabled
//        chips(0).on = (gate.state&0x10) != 0
//        chips(1).on = (gate.state&0x20) != 0
//        chips(2).on = (gate.state&0x80) != 0
//    }
//}
//
//class RenderSRLatch extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires1 = generateWireModels("RSLATCH", 2)
//    val wires2 = generateWireModels("RSLATCH2", 4)
//    val torches1 = Seq(new RedstoneTorchModel(8, 3), new RedstoneTorchModel(8, 13))
//    val torches2 = Seq(new RedstoneTorchModel(9.5, 3), new RedstoneTorchModel(6.5, 13))
//    val base1 = new BaseComponentModel("RSLATCH")
//    val base2 = new BaseComponentModel("RSLATCH2")
//
//    val m1 = Seq(base1)++wires1++torches1
//    val m2 = Seq(base2)++wires2++torches2
//
//    var shape = 0
//
//    override val coreModels = Seq()
//    override def switchModels = if (shape == 0) m1 else m2
//
//    override def prepareInv()
//    {
//        reflect = false
//        shape = 0
//        wires1(0).on = false
//        wires1(1).on = true
//        torches1(0).on = false
//        torches1(1).on = true
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        reflect = (gate.shape&1) != 0
//        shape = gate.shape>>1
//        var state = gate.state
//        if (reflect) state = flipMaskZ(state>>4)<<4|flipMaskZ(state)
//        if (shape == 0)
//        {
//            wires1(0).on = (state&0x88) != 0
//            wires1(1).on = (state&0x22) != 0
//            torches1(0).on = (state&0x10) != 0
//            torches1(1).on = (state&0x40) != 0
//        }
//        else
//        {
//            wires2(1).on = (state&2) != 0
//            wires2(3).on = (state&8) != 0
//            torches2(0).on = (state&0x10) != 0
//            torches2(1).on = (state&0x40) != 0
//            wires2(0).on = torches2(1).on
//            wires2(2).on = torches2(0).on
//        }
//    }
//}
//
//class RenderToggleLatch extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires = generateWireModels("TOGLATCH", 2)
//    val torches = Seq(new RedstoneTorchModel(4, 4), new RedstoneTorchModel(4, 12))
//    val lever = new LeverModel(11, 8)
//
//    override val coreModels = Seq(new BaseComponentModel("TOGLATCH"))++wires++torches:+lever
//
//    override def prepareInv()
//    {
//        wires(0).on = false
//        wires(1).on = false
//        torches(0).on = true
//        torches(1).on = false
//        lever.on = true
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        wires(0).on = (gate.state&8) != 0
//        wires(1).on = (gate.state&2) != 0
//        torches(0).on = (gate.state&0x10) != 0
//        torches(1).on = (gate.state&0x40) != 0
//        lever.on = (gate.state&0x10) != 0
//    }
//}
//
//class RenderTransparentLatch extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("TRANSLATCH", 5)
//    val torches = Seq(new RedstoneTorchModel(4, 12.5), new RedstoneTorchModel(4, 8),
//        new RedstoneTorchModel(8, 8), new RedstoneTorchModel(8, 2), new RedstoneTorchModel(14, 8))
//
//    override val coreModels = Seq(new BaseComponentModel("TRANSLATCH"))++wires++torches
//
//    override def prepareInv()
//    {
//        reflect = false
//        wires(0).on = true
//        wires(1).on = false
//        wires(2).on = true
//        wires(3).on = false
//        wires(4).on = false
//        torches(0).on = true
//        torches(1).on = false
//        torches(2).on = true
//        torches(3).on = false
//        torches(4).on = false
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        reflect = gate.shape == 1
//        val on = (gate.state&0x10) != 0
//        wires(0).on = !on
//        wires(1).on = (gate.state&4) != 0
//        wires(2).on = (gate.state&4) == 0
//        wires(3).on = on
//        wires(4).on = (gate.state&0xA) != 0
//        torches(0).on = wires(2).on
//        torches(1).on = !wires(2).on && !wires(4).on
//        torches(2).on = !wires(1).on && !wires(3).on
//        torches(3).on = on
//        torches(4).on = on
//    }
//}
//
//class RenderTimer extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires = generateWireModels("TIME", 3)
//    val torches = Seq(new RedstoneTorchModel(8, 3), new RedstoneTorchModel(8, 8))
//    val pointer = new PointerModel(8, 8)
//
//    override val coreModels = Seq(new BaseComponentModel("TIME"))++wires++Seq(pointer)++torches
//
//    override def prepareInv()
//    {
//        wires(0).on = false
//        wires(1).on = false
//        wires(2).on = false
//        torches(0).on = false
//        pointer.angle = 0
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        torches(0).on = (gate.state&0x10) != 0
//        wires(0).on = (gate.state&0x88) != 0
//        wires(1).on = (gate.state&0x22) != 0
//        wires(2).on = (gate.state&4) != 0
//        val ang = gate.getLogic[TTimerGateLogic].interpPointer(frame)*MathHelper.pi*2
//        pointer.angle = ang
//    }
//}
//
//class RenderSequencer extends ICGateRenderer[SequentialGateICPart]
//{
//    val torches = Seq(new RedstoneTorchModel(8, 8), new RedstoneTorchModel(8, 3),
//        new RedstoneTorchModel(13, 8), new RedstoneTorchModel(8, 13), new RedstoneTorchModel(3, 8))
//    val pointer = new PointerModel(8, 8)
//
//    torches(0).on = true
//
//    override val coreModels = Seq(new BaseComponentModel("SEQUENCER"), pointer)++torches
//
//    override def prepareInv()
//    {
//        torches(1).on = true
//        torches(2).on = false
//        torches(3).on = false
//        torches(4).on = false
//
//        pointer.angle = 0
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        torches(1).on = (gate.state&0x10) != 0
//        torches(2).on = (gate.state&0x20) != 0
//        torches(3).on = (gate.state&0x40) != 0
//        torches(4).on = (gate.state&0x80) != 0
//
//        val max = gate.getLogic[Sequencer].pointer_max*4
//        pointer.angle = (gate.getLogic[Sequencer].getWorldTime%max+frame)/max*2*MathHelper.pi
//        if (gate.shape == 1) pointer.angle *= -1
//    }
//}
//
//class RenderCounter extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires = generateWireModels("COUNT", 2)
//    val torches = Seq(new RedstoneTorchModel(11, 8), new RedstoneTorchModel(8, 3),
//        new RedstoneTorchModel(8, 13))
//    val pointer = new PointerModel(11, 8, 1.2D)
//
//    torches(0).on = true
//
//    override val coreModels = Seq(new BaseComponentModel("COUNT"))++wires++Seq(pointer)++torches
//
//    override def prepareInv()
//    {
//        reflect = false
//        wires(0).on = false
//        wires(1).on = false
//        torches(1).on = false
//        torches(2).on = true
//        pointer.angle = 220*MathHelper.torad
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        reflect = gate.shape == 1
//        wires(0).on = (gate.state&8) != 0
//        wires(1).on = (gate.state&2) != 0
//        torches(1).on = (gate.state&0x10) != 0
//        torches(2).on = (gate.state&0x40) != 0
//
//        val max = gate.getLogic[Counter].max
//        val value = gate.getLogic[Counter].value
//        pointer.angle = (value/max.toDouble*(340-220)+210)*MathHelper.torad
//        if (gate.shape == 1) reflect = true
//    }
//}
//
//class RenderStateCell extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires = generateWireModels("STATECELL", 5)
//    val torches = Seq(new RedstoneTorchModel(10, 3.5), new RedstoneTorchModel(13, 8))
//    val chip = new RedChipModel(6.5, 10)
//    val pointer = new PointerModel(13, 8)
//
//    override val coreModels = Seq(new BaseComponentModel("STATECELL"))++wires++Seq(chip, pointer)++torches
//
//    override def prepareInv()
//    {
//        reflect = false
//        wires(0).on = false
//        wires(1).on = false
//        wires(2).on = false
//        wires(3).on = false
//        wires(4).on = false
//        torches(0).on = false
//        torches(1).on = true
//        chip.on = false
//        pointer.angle = -MathHelper.pi/2
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        reflect = gate.shape == 1
//        val logic = gate.getLogic[StateCell]
//        var state = gate.state
//        if (reflect) state = flipMaskZ(state>>4)<<4|flipMaskZ(state)
//
//        wires(0).on = (state&0x10) != 0
//        wires(1).on = (state&4) != 0
//        wires(2).on = logic.state2 == 0 || (state&4) != 0
//        wires(3).on = (state&0x88) != 0
//        wires(4).on = (state&2) != 0
//        torches(0).on = (state&0x10) != 0
//        torches(1).on = logic.pointer_start >= 0
//        chip.on = logic.state2 != 0
//
//        reflect = gate.shape == 1
//        pointer.angle = gate.getLogic[StateCell].interpPointer(frame)-MathHelper.pi/2
//    }
//}
//
//class RenderSynchronizer extends ICGateRenderer[SequentialGateICPart]
//{
//    val wires = generateWireModels("SYNC", 6)
//    val torch = new RedstoneTorchModel(8, 3)
//    val chips = Seq(new RedChipModel(4.5, 9), new RedChipModel(11.5, 9))
//
//    override val coreModels = Seq(new BaseComponentModel("SYNC"))++wires++chips++Seq(torch)
//
//    override def prepareInv()
//    {
//        wires(0).on = true
//        wires(1).on = true
//        wires(2).on = false
//        wires(3).on = false
//        wires(4).on = false
//        wires(5).on = false
//        chips(0).on = false
//        chips(1).on = false
//        torch.on = false
//    }
//
//    override def prepareDynamic(gate:SequentialGateICPart, frame:Float)
//    {
//        val logic = gate.getLogic[Synchronizer]
//        wires(0).on = !logic.left
//        wires(1).on = !logic.right
//        wires(2).on = (gate.state&4) != 0
//        wires(3).on = logic.left && logic.right
//        wires(4).on = (gate.state&8) != 0
//        wires(5).on = (gate.state&2) != 0
//        chips(0).on = logic.left
//        chips(1).on = logic.right
//        torch.on = (gate.state&0x10) != 0
//    }
//}
//
//class RenderDecRandomizer extends ICGateRenderer[ComboGateICPart]
//{
//    val wires = generateWireModels("DECRAND", 6)
//    val chips = Seq(new YellowChipModel(5, 13), new YellowChipModel(11, 13), new RedChipModel(5.5, 8))
//    val torches = Seq(new RedstoneTorchModel(8, 2.5), new RedstoneTorchModel(14, 8), new RedstoneTorchModel(2, 8),
//        new RedstoneTorchModel(9, 8))
//
//    override val coreModels = Seq(new BaseComponentModel("DECRAND"))++wires++chips++torches
//
//    override def prepareInv()
//    {
//        wires(0).on = false
//        wires(1).on = false
//        wires(2).on = false
//        wires(3).on = false
//        wires(4).on = true
//        wires(5).on = true
//        wires(0).disabled = false
//        wires(3).disabled = false
//        torches(0).on = true
//        torches(1).on = false
//        torches(2).on = false
//        torches(3).on = false
//        chips(0).on = false
//        chips(1).on = true
//        chips(2).on = true
//    }
//
//    override def prepareDynamic(gate:ComboGateICPart, frame:Float)
//    {
//        val state = gate.state
//        wires(0).on = (state>>4) == 2
//        wires(1).on = (state>>4) == 8
//        wires(2).on = (state&4) != 0
//        wires(3).on = (state&4) != 0
//        wires(4).on = (state>>4) == 1 || (state>>4) == 2
//        wires(5).on = (state>>4) == 1
//        wires(0).disabled = gate.shape != 0
//        wires(3).disabled = gate.shape != 0
//        torches(0).on = (state>>4) == 1
//        torches(1).on = (state>>4) == 2
//        torches(2).on = (state>>4) == 8
//        torches(3).on = !wires(4).on
//        chips(0).on = (state>>4) == 2
//        chips(1).on = (state>>4) == 1 || (state>>4) == 2
//        chips(2).on = true
//    }
//}
//
//class RenderNullCell extends ICGateRenderer[ArrayGateICPart]
//{
//    val top = new CellTopWireModel
//    val bottom = new NullCellBottomWireModel
//
//    override val coreModels = Seq(new BaseComponentModel("NULLCELL"), bottom, new CellStandModel, top)
//
//    override def prepareInv()
//    {
//        bottom.signal = 0
//        top.signal = 0
//    }
//
//    override def prepareDynamic(gate:ArrayGateICPart, frame:Float)
//    {
//        bottom.signal = gate.getLogic[NullCell].signal1
//        top.signal = gate.getLogic[NullCell].signal2
//    }
//}
//
//class RenderInvertCell extends ICGateRenderer[ArrayGateICPart]
//{
//    val wires = generateWireModels("INVCELL", 1)
//    val torch = new RedstoneTorchModel(8, 8)
//    val top = new CellTopWireModel
//    val bottom = new InvertCellBottomWireModel
//
//    override val coreModels = Seq(new BaseComponentModel("INVCELL"))++wires++Seq(bottom, torch, new CellStandModel, top)
//
//    override def prepareInv()
//    {
//        bottom.signal = 0
//        top.signal = 255.toByte
//        wires(0).on = false
//        torch.on = true
//    }
//
//    override def prepareDynamic(gate:ArrayGateICPart, frame:Float)
//    {
//        val logic = gate.getLogic[InvertCell]
//        bottom.signal = logic.signal1
//        top.signal = logic.signal2
//        wires(0).on = logic.signal1 != 0
//        torch.on = logic.signal1 == 0
//    }
//}
//
//class RenderBufferCell extends ICGateRenderer[ArrayGateICPart]
//{
//    val wires = generateWireModels("BUFFCELL", 2)
//    val torches = Seq(new RedstoneTorchModel(11, 13), new RedstoneTorchModel(8, 8))
//    val top = new CellTopWireModel
//    val bottom = new InvertCellBottomWireModel
//
//    override val coreModels = Seq(new BaseComponentModel("BUFFCELL"))++wires++Seq(bottom)++torches++Seq(new CellStandModel, top)
//
//    override def prepareInv()
//    {
//        bottom.signal = 0
//        top.signal = 0
//        wires(0).on = false
//        wires(1).on = true
//        torches(0).on = true
//        torches(1).on = false
//    }
//
//    override def prepareDynamic(gate:ArrayGateICPart, frame:Float)
//    {
//        val logic = gate.getLogic[BufferCell]
//        bottom.signal = logic.signal1
//        top.signal = logic.signal2
//        torches(0).on = logic.signal1 == 0
//        torches(1).on = logic.signal1 != 0
//        wires(0).on = logic.signal1 != 0
//        wires(1).on = logic.signal1 == 0
//    }
//}
//
