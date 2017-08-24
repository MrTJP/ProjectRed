/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import java.util.Random

import codechicken.lib.colour.EnumColour
import codechicken.lib.math.MathHelper
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.lib.vec.{RedundantTransformation, Transformation, Vector3}
import mrtjp.projectred.core.TFaceOrient.flipMaskZ
import mrtjp.projectred.integration.ComponentStore._
import net.minecraft.client.renderer.texture.TextureMap
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.item.ItemStack
import net.minecraft.util.EnumParticleTypes
import org.lwjgl.opengl.GL11

object RenderGate extends IIconRegister
{
    var renderers = buildRenders()

    def buildRenders() = Seq[GateRenderer[_]](
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
        new RenderRepeater,
        new RenderRandomizer,
        new RenderSRLatch,
        new RenderToggleLatch,
        new RenderTransparentLatch,
        new RenderLightSensor,
        new RenderRainSensor,
        new RenderTimer,
        new RenderSequencer,
        new RenderCounter,
        new RenderStateCell,
        new RenderSynchronizer,
        new RenderBusXcvr,
        new RenderNullCell,
        new RenderInvertCell,
        new RenderBufferCell,
        new RenderComparator,
        new RenderANDCell,
        new RenderBusRandomizer,
        new RenderBusConverter,
        new RenderBusInputPanel,
        new RenderStackingLatch,
        new RenderSegmentDisplay,
        new RenderDecodingRand,
        GateRenderer.blank//circuit gate renderer will be injected.
    )


    override def registerIcons(map:TextureMap)
    {
        ComponentStore.registerIcons(map)
        for (r <- renderers) r.registerIcons(map)
    }

    def renderStatic(gate:GatePart, pos:Vector3, ccrs:CCRenderState)
    {
        val r = renderers(gate.subID).asInstanceOf[GateRenderer[GatePart]]
        r.prepare(gate)
        r.renderStatic(pos.translation(), gate.orientation&0xFF, ccrs)
    }

    def renderDynamic(gate:GatePart, pos:Vector3, frame:Float, ccrs:CCRenderState)
    {
        val r = renderers(gate.subID).asInstanceOf[GateRenderer[GatePart]]
        if (r.hasSpecials)
        {
            r.prepareDynamic(gate, frame)
            r.renderDynamic(gate.rotationT.`with`(pos.translation()), ccrs)
        }
    }

    def renderInv(stack:ItemStack, t:Transformation, id:Int, ccrs:CCRenderState)
    {
        val r = renderers(id)
        TextureUtils.bindBlockTexture()
        r.prepareInv(stack)
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        r.renderStatic(t, 0, ccrs)
        ccrs.draw()
        if (r.hasSpecials) r.renderDynamic(t, ccrs)
    }

    def spawnParticles(gate:GatePart, rand:Random)
    {
        renderers(gate.subID).asInstanceOf[GateRenderer[GatePart]].spawnParticles(gate, rand)
    }

    def hotswap(r:GateRenderer[_], meta:Int)
    {
        val ar = renderers.toArray
        ar(meta) = r
        renderers = ar.toSeq
    }
}

abstract class GateRenderer[T <: GatePart]
{
    var reflect = false

    def coreModels:Seq[ComponentModel]
    def switchModels = Seq[ComponentModel]()
    def allSwitchModels = Seq[ComponentModel]()

    private def enabledModels = coreModels++switchModels
    private def allModels = coreModels++allSwitchModels

    def registerIcons(map:TextureMap)
    {
        for (m <- allModels) if (m != null) m.registerIcons(map)
    }

    def renderModels(t:Transformation, orient:Int, ccrs:CCRenderState)
    {
        for (m <- enabledModels) m.renderModel(t, orient, ccrs)
    }

    def renderStatic(t:Transformation, orient:Int, ccrs:CCRenderState)
    {
        renderModels(t, if (reflect) orient+24 else orient, ccrs)
    }

    def hasSpecials = false
    def renderDynamic(t: Transformation, ccrs: CCRenderState){}

    def prepareInv(stack:ItemStack){prepareInv()}
    def prepareInv(){}
    def prepare(gate:T){}
    def prepareDynamic(gate:T, frame:Float){}

    def spawnParticles(gate:T, rand:Random)
    {
        prepare(gate)
        val torches = enabledModels.collect
        {
            case t:TRedstoneTorchModel if t.on => t
        }

        for (t <- torches) if (rand.nextInt(torches.length) == 0)
        {
            val pos = new Vector3(rand.nextFloat, rand.nextFloat, rand.nextFloat).add(-0.5).multiply(0.05, 0.1, 0.05)
            pos.add(t.getLightPos)
            pos.apply(gate.rotationT).add(gate.pos)
            gate.world.spawnParticle(EnumParticleTypes.REDSTONE, pos.x, pos.y, pos.z, 0, 0, 0)
        }
    }
}

object GateRenderer
{
    val blank = new GateRenderer[GatePart]{
        override def coreModels = Seq()
    }
}

class RenderOR extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("or", 4)
    val torches = Seq(new RedstoneTorchModel(8, 9, 6), new RedstoneTorchModel(8, 2.5, 8))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderNOR extends GateRenderer[ComboGatePart]
{
    var wires = generateWireModels("nor", 4)
    var torch = new RedstoneTorchModel(8, 9, 6)

    override val coreModels = wires:+torch:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderNOT extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("not", 4)
    val torch = new RedstoneTorchModel(8, 8, 6)

    override val coreModels = wires:+torch:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderAND extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("and", 4)
    val torches = Seq(new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderNAND extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("nand", 4)
    val torches = Seq(new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
        new RedstoneTorchModel(8, 8, 6))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderXOR extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("xor", 4)
    val torches = Seq(new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6),
        new RedstoneTorchModel(8, 12, 6))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderXNOR extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("xnor", 5)
    val torches = Seq(new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(4.5, 8, 6),
        new RedstoneTorchModel(11.5, 8, 6), new RedstoneTorchModel(8, 12, 6))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderBuffer extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("buffer", 4)
    val torches = Seq(new RedstoneTorchModel(8, 3.5, 8), new RedstoneTorchModel(8, 9, 6))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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

class RenderMultiplexer extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("multiplexer", 6)
    val torches = Seq(new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(9, 10.5, 6),
        new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6))

    override val coreModels = wires++torches:+new BaseComponentModel

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

    override def prepare(gate:ComboGatePart)
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
        wires(1).on = torches(3).on
    }
}

class RenderPulse extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("pulse", 3)
    val torches = Seq(new RedstoneTorchModel(4, 9.5, 6), new RedstoneTorchModel(11, 9.5, 6),
        new RedstoneTorchModel(8, 3.5, 8))

    override val coreModels = wires++torches:+new BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = false
    }

    override def prepare(gate:ComboGatePart)
    {
        wires(0).on = (gate.state&4) == 0
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&0x14) == 4
        torches(0).on = wires(0).on
        torches(1).on = wires(1).on
        torches(2).on = (gate.state&0x10) != 0
    }
}

class RenderRepeater extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("repeater", 2)
    val endTorch = new RedstoneTorchModel(8, 2, 6)
    val varTorches = Seq(new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(12.5, 11, 6),
        new RedstoneTorchModel(12.5, 10, 6), new RedstoneTorchModel(12.5, 9, 6), new RedstoneTorchModel(12.5, 8, 6),
        new RedstoneTorchModel(12.5, 7, 6), new RedstoneTorchModel(12.5, 6, 6), new RedstoneTorchModel(12.5, 5, 6),
        new RedstoneTorchModel(12.5, 4, 6))

    var shape = 0

    override val coreModels = wires++Seq(endTorch, new BaseComponentModel)

    override def switchModels = Seq(varTorches(shape))
    override def allSwitchModels = varTorches

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        endTorch.on = false
        shape = 0
        varTorches(0).on = true
    }

    override def prepare(gate:ComboGatePart)
    {
        wires(0).on = (gate.state&0x10) == 0
        wires(1).on = (gate.state&4) != 0
        endTorch.on = (gate.state&0x10) != 0
        shape = gate.shape
        varTorches(shape).on = (gate.state&4) == 0
    }
}

class RenderRandomizer extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("rand", 7)
    val chips = Seq(new YellowChipModel(8, 5.5), new YellowChipModel(11.5, 11.5), new YellowChipModel(4.5, 11.5))

    override val coreModels = wires++chips:+new BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(4).on = false
        wires(5).on = false
        wires(6).on = false
        wires(0).disabled = false
        wires(1).disabled = false
        wires(3).disabled = false
        wires(4).disabled = false
        wires(5).disabled = false
        wires(6).disabled = false
        chips(0).on = false
        chips(1).on = false
        chips(2).on = false
    }

    override def prepare(gate:ComboGatePart)
    {
        wires(2).on = (gate.state&4) != 0
        wires(0).on = (gate.state&0x11) != 0
        wires(1).on = (gate.state&0x22) != 0
        wires(3).on = (gate.state&0x88) != 0
        wires(4).on = wires(2).on
        wires(5).on = wires(2).on
        wires(6).on = wires(2).on
        wires(1).disabled = (gate.shape&1) != 0
        wires(0).disabled = (gate.shape&2) != 0
        wires(3).disabled = (gate.shape&4) != 0
        wires(5).disabled = wires(1).disabled
        wires(4).disabled = wires(0).disabled
        wires(6).disabled = wires(3).disabled
        chips(0).on = (gate.state&0x10) != 0
        chips(1).on = (gate.state&0x20) != 0
        chips(2).on = (gate.state&0x80) != 0
    }
}

class RenderSRLatch extends GateRenderer[SequentialGatePart]
{
    val wires1 = generateWireModels("rslatch", 2)
    val wires2 = generateWireModels("rslatch2", 4)
    val torches1 = Seq(new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 13, 6))
    val torches2 = Seq(new RedstoneTorchModel(9.5, 3, 6), new RedstoneTorchModel(6.5, 13, 6))
    var shape = 0

    override val coreModels = Seq(new BaseComponentModel)
    override def switchModels = if (shape == 0) wires1++torches1 else wires2++torches2
    override val allSwitchModels = wires1++wires2++torches1++torches2

    override def prepareInv()
    {
        reflect = false
        shape = 0
        wires1(0).on = false
        wires1(1).on = true
        torches1(0).on = false
        torches1(1).on = true
    }

    override def prepare(gate:SequentialGatePart)
    {
        reflect = (gate.shape&1) != 0
        shape = gate.shape>>1
        var state = gate.state
        if (reflect) state = flipMaskZ(state>>4)<<4|flipMaskZ(state)
        if (shape == 0)
        {
            wires1(0).on = (state&0x88) != 0
            wires1(1).on = (state&0x22) != 0
            torches1(0).on = (state&0x10) != 0
            torches1(1).on = (state&0x40) != 0
        }
        else
        {
            wires2(1).on = (state&2) != 0
            wires2(3).on = (state&8) != 0
            torches2(0).on = (state&0x10) != 0
            torches2(1).on = (state&0x40) != 0
            wires2(0).on = torches2(1).on
            wires2(2).on = torches2(0).on
        }
    }
}

class RenderToggleLatch extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("toglatch", 2)
    val torches = Seq(new RedstoneTorchModel(4, 4, 6), new RedstoneTorchModel(4, 12, 6))
    val lever = new LeverModel(11, 8)

    override val coreModels = wires++torches++Seq(lever, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        torches(0).on = true
        torches(1).on = false
        lever.state = 0
    }

    override def prepare(gate:SequentialGatePart)
    {
        wires(0).on = (gate.state&8) != 0
        wires(1).on = (gate.state&2) != 0
        torches(0).on = (gate.state&0x10) != 0
        torches(1).on = (gate.state&0x40) != 0
        lever.state = if ((gate.state&0x10) != 0) 0 else 1
    }
}

class RenderTransparentLatch extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("translatch", 5)
    val torches = Seq(new RedstoneTorchModel(4, 12.5, 6), new RedstoneTorchModel(4, 8, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(14, 8, 8))

    override val coreModels = wires++torches:+new BaseComponentModel

    override def prepareInv()
    {
        reflect = false
        wires(0).on = true
        wires(1).on = false
        wires(2).on = true
        wires(3).on = false
        wires(4).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = true
        torches(3).on = false
        torches(4).on = false
    }

    override def prepare(gate:ComboGatePart)
    {
        reflect = gate.shape == 1
        val on = (gate.state&0x10) != 0
        wires(0).on = !on
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&4) == 0
        wires(3).on = on
        wires(4).on = (gate.state&0xA) != 0
        torches(0).on = wires(2).on
        torches(1).on = !wires(2).on && !wires(4).on
        torches(2).on = !wires(1).on && !wires(3).on
        torches(3).on = on
        torches(4).on = on
    }
}

class RenderLightSensor extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("lightsensor", 1)
    val solar = new SolarModel(8, 5.5)

    override val coreModels = wires++Seq(solar, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        solar.state = 0
    }

    override def prepare(gate:ComboGatePart)
    {
        wires(0).on = (gate.state&0xF4) != 0
        solar.state = gate.shape
    }
}

class RenderRainSensor extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("rainsensor", 1)
    val sensor = new RainSensorModel(8, 6)

    override val coreModels = wires++Seq(sensor, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
    }

    override def prepare(gate:ComboGatePart)
    {
        wires(0).on = (gate.state&0x44) != 0
    }
}

class RenderTimer extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("time", 3)
    val torches = Seq(new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 8, 12))
    val pointer = new PointerModel(8, 8, 8)

    override val coreModels = wires++torches:+new BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        torches(0).on = false
        pointer.angle = 0
    }

    override def prepare(gate:SequentialGatePart)
    {
        torches(0).on = (gate.state&0x10) != 0
        wires(0).on = (gate.state&0x88) != 0
        wires(1).on = (gate.state&0x22) != 0
        wires(2).on = (gate.state&4) != 0
    }

    override def hasSpecials = true

    override def prepareDynamic(part:SequentialGatePart, frame:Float)
    {
        pointer.angle = part.getLogic[TTimerGateLogic].interpPointer(frame)*MathHelper.pi*2
    }

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
        pointer.renderModel(t, 0, ccrs)
        ccrs.draw()
    }
}

class RenderSequencer extends GateRenderer[SequentialGatePart]
{
    val torches = Seq(new RedstoneTorchModel(8, 8, 12), new RedstoneTorchModel(8, 3, 6),
        new RedstoneTorchModel(13, 8, 6), new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(3, 8, 6))
    val pointer = new PointerModel(8, 8, 8)

    torches(0).on = true

    override val coreModels = torches:+new BaseComponentModel

    override def prepare(gate:SequentialGatePart)
    {
        torches(1).on = (gate.state&0x10) != 0
        torches(2).on = (gate.state&0x20) != 0
        torches(3).on = (gate.state&0x40) != 0
        torches(4).on = (gate.state&0x80) != 0
    }

    override def prepareInv()
    {
        torches(1).on = true
        torches(2).on = false
        torches(3).on = false
        torches(4).on = false
        pointer.angle = 0
    }

    override def prepareDynamic(gate:SequentialGatePart, frame:Float)
    {
        val max = gate.getLogic[Sequencer].pointer_max*4
        pointer.angle = (gate.world.getWorldTime%max+frame)/max*2*MathHelper.pi
        if (gate.shape == 1) pointer.angle = -pointer.angle
    }

    override def hasSpecials = true

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
        pointer.renderModel(t, 0, ccrs)
        ccrs.draw()
    }
}

class RenderCounter extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("count", 2)
    val torches = Seq(new RedstoneTorchModel(11, 8, 12), new RedstoneTorchModel(8, 3, 6),
        new RedstoneTorchModel(8, 13, 6))
    val pointer = new PointerModel(11, 8, 8, 1.2D)

    torches(0).on = true

    override val coreModels = wires++torches:+new BaseComponentModel

    override def prepare(gate:SequentialGatePart)
    {
        reflect = gate.shape == 1
        wires(0).on = (gate.state&8) != 0
        wires(1).on = (gate.state&2) != 0
        torches(1).on = (gate.state&0x10) != 0
        torches(2).on = (gate.state&0x40) != 0
    }

    override def prepareInv()
    {
        reflect = false
        wires(0).on = false
        wires(1).on = false
        torches(1).on = false
        torches(2).on = true
        pointer.angle = 220*MathHelper.torad
    }

    override def prepareDynamic(gate:SequentialGatePart, frame:Float)
    {
        val max = gate.getLogic[Counter].max
        val value = gate.getLogic[Counter].value
        pointer.angle = (value/max.toDouble*(340-220)+210)*MathHelper.torad
        if (gate.shape == 1) reflect = true
    }

    override def hasSpecials = true

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
        pointer.renderModel(t, if (reflect) 1 else 0, ccrs)
        ccrs.draw()
    }
}

class RenderStateCell extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("statecell", 5)
    val torches = Seq(new RedstoneTorchModel(10, 3.5, 6), new RedstoneTorchModel(13, 8, 12))
    val chip = new RedChipModel(6.5, 10)
    val pointer = new PointerModel(13, 8, 8)

    override val coreModels = wires++torches++Seq(chip, new BaseComponentModel)

    override def prepareInv()
    {
        reflect = false
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(4).on = false
        torches(0).on = false
        torches(1).on = true
        chip.on = false
        pointer.angle = -MathHelper.pi/2
    }

    override def prepare(part:SequentialGatePart)
    {
        reflect = part.shape == 1
        val logic = part.getLogic[StateCell]
        var state = part.state
        if (reflect) state = flipMaskZ(state>>4)<<4|flipMaskZ(state)

        wires(0).on = (state&0x10) != 0
        wires(1).on = (state&4) != 0
        wires(2).on = logic.state2 == 0 || (state&4) != 0
        wires(3).on = (state&0x88) != 0
        wires(4).on = (state&2) != 0
        torches(0).on = (state&0x10) != 0
        torches(1).on = logic.pointer_start >= 0
        chip.on = logic.state2 != 0
    }

    override def hasSpecials = true

    override def prepareDynamic(part:SequentialGatePart, frame:Float)
    {
        reflect = part.shape == 1
        pointer.angle = part.getLogic[StateCell].interpPointer(frame)-MathHelper.pi/2
    }

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
        pointer.renderModel(t, if (reflect) 1 else 0, ccrs)
        ccrs.draw()
    }
}

class RenderSynchronizer extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("sync", 6)
    val torch = new RedstoneTorchModel(8, 3, 6)
    val chips = Seq(new RedChipModel(4.5, 9), new RedChipModel(11.5, 9))

    override val coreModels = wires++chips++Seq(torch, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = true
        wires(2).on = false
        wires(3).on = false
        wires(4).on = false
        wires(5).on = false
        chips(0).on = false
        chips(1).on = false
        torch.on = false
    }

    override def prepare(gate:SequentialGatePart)
    {
        val logic = gate.getLogic[Synchronizer]
        wires(0).on = !logic.left
        wires(1).on = !logic.right
        wires(2).on = (gate.state&4) != 0
        wires(3).on = logic.left && logic.right
        wires(4).on = (gate.state&8) != 0
        wires(5).on = (gate.state&2) != 0
        chips(0).on = logic.left
        chips(1).on = logic.right
        torch.on = (gate.state&0x10) != 0
    }
}

class RenderBusXcvr extends GateRenderer[BundledGatePart]
{
    val wires = generateWireModels("busxcvr", 2)
    val panels = Seq(new SigLightPanelModel(4, 8, false), new SigLightPanelModel(12, 8, true))
    val cable = new BusXcvrCableModel

    override val coreModels = wires++panels++Seq(cable, new BaseComponentModel)

    override def prepareInv()
    {
        reflect = false
        wires(0).on = false
        wires(1).on = false
        panels(0).signal = 0
        panels(1).signal = 0
    }

    override def prepare(gate:BundledGatePart)
    {
        reflect = gate.shape != 0
        var state = gate.state
        if (reflect) state = flipMaskZ(state)

        wires(0).on = (state&2) != 0
        wires(1).on = (state&8) != 0
        val logic = gate.getLogic[BusTransceiver]
        val packed = logic.packClientData
        panels(0).signal = packed>>>16
        panels(1).signal = packed&0xFFFF
    }
}

class RenderComparator extends GateRenderer[SequentialGatePart]
{
    val wires = generateWireModels("comparator", 4)
    val torch = new RedstoneTorchModel(8, 2, 6)
    val chips = Seq(new MinusChipModel(5, 8), new PlusChipModel(11, 8))

    override val coreModels = wires++Seq(torch, new BaseComponentModel)

    override def prepareInv()
    {
        reflect = false
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        chips(0).on = false
        chips(1).on = false
        torch.on = false
    }

    override def prepare(gate:SequentialGatePart)
    {
        reflect = gate.shape != 0
        wires(0).on = (gate.state&0x10) == 0
        wires(1).on = (gate.state&2) != 0
        wires(2).on = (gate.state&4) != 0
        wires(3).on = (gate.state&8) != 0
        chips(0).on = (gate.state&1) != 0 && gate.shape == 1
        chips(1).on = (gate.state&1) != 0 && gate.shape != 1
        torch.on = (gate.state&0x10) != 0
        if (gate.shape != 0)
        {
            val a = wires(1).on
            val b = wires(3).on
            wires(3).on = a
            wires(1).on = b
        }
    }

    override def renderModels(t: Transformation, orient:Int, ccrs:CCRenderState)
    {
        super.renderModels(t, orient, ccrs)
        chips.foreach(_.renderModel(t, orient%24, ccrs))
    }
}

class RenderBusRandomizer extends GateRenderer[BundledGatePart]
{
    val cable = new BusRandCableModel
    val panel = new SigLightPanelModel(8, 8, true)
    val wires1 = generateWireModels("busrand1", 2)
    val wires2 = generateWireModels("busrand2", 2)

    var shape = 0

    panel.offColour = 0x756900FF
    panel.onColour = 0xe1d600FF

    override val coreModels = Seq(cable, panel, new BaseComponentModel)

    override def switchModels = if (shape == 0) wires1 else wires2
    override def allSwitchModels = wires1++wires2

    override def prepareInv()
    {
        shape = 0
        panel.signal = 0
        panel.disableMask = 0

        wires1(0).on = false
        wires2(0).on = false
        wires1(1).on = false
        wires2(1).on = false
    }

    override def prepare(part:BundledGatePart)
    {
        shape = part.shape
        val logic = part.getLogic[BusRandomizer]
        panel.signal = logic.output
        panel.disableMask = ~logic.mask

        wires1(0).on = (part.state&2) != 0
        wires2(0).on = (part.state&2) != 0
        wires1(1).on = (part.state&8) != 0
        wires2(1).on = (part.state&8) != 0
    }
}

class RenderBusConverter extends GateRenderer[BundledGatePart]
{
    val wires = generateWireModels("busconv", 3)
    val cable = new BusConvCableModel
    val bar = new SignalBarModel(8, 8)

    override val coreModels = wires++Seq(cable, bar, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        bar.signal = 0
        bar.inverted = false
    }

    override def prepare(gate:BundledGatePart)
    {
        val logic = gate.getLogic[BusConverter]
        wires(0).on = (gate.state&0x20) != 0
        wires(1).on = (gate.state&0x80) != 0
        wires(2).on = (logic.rsIn|logic.rsOut) != 0
        bar.inverted = gate.shape != 0
        bar.signal = logic.rsIn|logic.rsOut
    }
}

class RenderBusInputPanel extends GateRenderer[BundledGatePart]
{
    val wires = generateWireModels("businput", 1)
    val buttons = new InputPanelButtonsModel
    val cable = new BusInputPanelCableModel

    override val coreModels = wires++Seq(buttons, cable, new BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        buttons.pressMask = 0
        buttons.pos.setPos(0, 0, 0)
        buttons.orientationT = new RedundantTransformation
    }

    override def prepare(gate:BundledGatePart)
    {
        wires(0).on = (gate.state&1) != 0
        buttons.pressMask = gate.getLogic[BusInputPanel].pressMask
    }

    override def hasSpecials = true

    override def prepareDynamic(gate:BundledGatePart, frame:Float)
    {
        buttons.pressMask = gate.getLogic[BusInputPanel].pressMask
        buttons.pos.setPos(gate.pos)
        buttons.orientationT = gate.rotationT
    }

    override def renderDynamic(t: Transformation, ccrs: CCRenderState)
    {
        buttons.renderLights()
    }
}

abstract class RenderArrayCell extends GateRenderer[ArrayGatePart]
{
    val topWire:CellTopWireModel
    val bottomWire:CellBottomWireModel

    override def prepareInv()
    {
        bottomWire.signal = 0
        topWire.signal = 0
        topWire.conn = 0
    }

    override def prepare(gate:ArrayGatePart)
    {
        val logic = gate.getLogic[ArrayGateLogicCrossing]
        bottomWire.signal = logic.signal1
        topWire.signal = logic.signal2
        topWire.conn = IGateWireRenderConnect.getConnsAtHeight(gate, 10.0D)
    }
}

class RenderNullCell extends RenderArrayCell
{
    override val topWire:CellTopWireModel = new CellTopWireModel(nullCellWireTop)
    override val bottomWire:CellBottomWireModel = new CellBottomWireModel(nullCellWireBottom)
    override val coreModels = Seq(bottomWire, topWire, new CellFrameModel, new NullCellBaseModel)
}

class RenderInvertCell extends RenderArrayCell
{
    val wires = generateWireModels("invcell", 1)
    val torch = new RedstoneTorchModel(8, 8, 6)

    override val topWire:CellTopWireModel = new CellTopWireModel(extendedCellWireTop)
    override val bottomWire:CellBottomWireModel = new CellBottomWireModel(extendedCellWireBottom)
    override val coreModels = wires++Seq(torch, bottomWire, topWire, new CellFrameModel, new CellPlateModel, new ExtendedCellBaseModel)

    override def prepareInv()
    {
        super.prepareInv()
        topWire.signal = 255.toByte
        wires(0).on = false
        torch.on = true
    }

    override def prepare(gate:ArrayGatePart)
    {
        super.prepare(gate)
        val logic = gate.getLogic[ArrayGateLogicCrossing]
        torch.on = logic.signal1 == 0
        wires(0).on = logic.signal1 != 0
    }
}

class RenderBufferCell extends RenderArrayCell
{
    val wires = generateWireModels("buffcell", 2)
    val torches = Seq(new RedstoneTorchModel(11, 13, 6), new RedstoneTorchModel(8, 8, 6))

    override val topWire:CellTopWireModel = new CellTopWireModel(extendedCellWireTop)
    override val bottomWire:CellBottomWireModel = new CellBottomWireModel(extendedCellWireBottom)
    override val coreModels = wires++torches++Seq(topWire, bottomWire, new CellFrameModel, new CellPlateModel, new ExtendedCellBaseModel)

    override def prepareInv()
    {
        super.prepareInv()
        wires(0).on = false
        wires(1).on = true
        torches(0).on = true
        torches(1).on = false
    }

    override def prepare(gate:ArrayGatePart)
    {
        super.prepare(gate)
        val logic = gate.getLogic[ArrayGateLogicCrossing]
        torches(0).on = logic.signal1 == 0
        torches(1).on = logic.signal1 != 0
        wires(0).on = logic.signal1 != 0
        wires(1).on = logic.signal1 == 0
    }
}

class RenderANDCell extends GateRenderer[ArrayGatePart]
{
    val wires = generateWireModels("andcell", 2)
    val torches = Seq(new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(8, 2, 8), new FlippedRSTorchModel(8, 8))
    val topWire = new CellTopWireModel(nullCellWireTop)

    override val coreModels = wires++torches++Seq(topWire, new CellFrameModel, new BaseComponentModel)

    override def prepareInv()
    {
        topWire.signal = 0
        topWire.conn = 0
        torches(0).on = true
        torches(1).on = false
        torches(2).on = true
        wires(0).on = true
        wires(1).on = false
    }

    override def prepare(gate:ArrayGatePart)
    {
        val logic = gate.getLogic[ANDCell]
        topWire.signal = logic.signal
        topWire.conn = IGateWireRenderConnect.getConnsAtHeight(gate, 10.0D)
        torches(0).on = (gate.state&4) == 0
        torches(1).on = (gate.state&0x10) != 0
        torches(2).on = logic.signal == 0
        wires(0).on = torches(0).on || torches(2).on
        wires(1).on = !torches(0).on
    }
}

class RenderStackingLatch extends GateRenderer[ArrayGatePart]
{
    var wires = generateWireModels("stacklatch", 5)
    var clkwire = new CellBottomWireModel(stackLatchWireBottom)
    var torches = Seq(new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(8, 12, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8))

    override val coreModels = wires++torches++Seq(clkwire, new StackLatchStandModel(3.5, 5),
        new StackLatchStandModel(12.5, 5), new BaseComponentModel)

    override def prepareInv()
    {
        clkwire.signal = 0
        wires(0).on = true
        wires(1).on = false
        wires(2).on = true
        wires(3).on = false
        wires(4).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = true
        torches(3).on = false
    }

    override def prepare(gate:ArrayGatePart)
    {
        val on = (gate.state&0x10) != 0
        val sig = gate.getLogic[StackingLatch].signal
        clkwire.signal = sig
        wires(0).on = !on
        wires(1).on = sig != 0
        wires(2).on = sig == 0
        wires(3).on = on
        wires(4).on = (gate.state&4) != 0
        torches(0).on = wires(2).on
        torches(1).on = !wires(2).on && !wires(4).on
        torches(2).on = !wires(1).on && !wires(3).on
        torches(3).on = on
    }
}

class RenderSegmentDisplay extends GateRenderer[BundledGatePart]
{
    val sevenSeg1 = new SevenSegModel(4.5, 8)
    val sevenSeg0 = new SevenSegModel(11.5, 8)
    val sixteenSeg = new SixteenSegModel(8, 8)
    var shape = 0

    override val coreModels = Seq(new SegmentBusCableModel, new BaseComponentModel)
    override def switchModels = if (shape == 0) Seq(sevenSeg0, sevenSeg1) else Seq(sixteenSeg)
    override def allSwitchModels = Seq(sevenSeg0, sevenSeg1, sixteenSeg)

    override def prepareInv()
    {
        shape = 0
        sevenSeg1.signal = 64
        sevenSeg0.signal = 64
        sixteenSeg.signal = 0
        Seq(sevenSeg0, sevenSeg1, sixteenSeg).foreach(_.setColourOn(EnumColour.RED.ordinal.toByte))
    }

    override def prepare(gate:BundledGatePart)
    {
        shape = gate.shape
        val logic = gate.getLogic[SegmentDisplay]
        val sig1 = logic.bInH
        val sig0 = gate.state
        sevenSeg1.signal = sig1
        sevenSeg0.signal = sig0
        sixteenSeg.signal = sig1<<8|sig0
        Seq(sevenSeg0, sevenSeg1, sixteenSeg).foreach(_.setColourOn(logic.colour))
    }
}

class RenderDecodingRand extends GateRenderer[ComboGatePart]
{
    val wires = generateWireModels("decrand", 6)
    val chips = Seq(new YellowChipModel(5, 13), new YellowChipModel(11, 13), new RedChipModel(5.5, 8))
    val torches = Seq(new RedstoneTorchModel(8, 2.5, 8), new RedstoneTorchModel(14, 8, 8), new RedstoneTorchModel(2, 8, 8), new RedstoneTorchModel(9, 8, 6))

    override val coreModels = wires++chips++torches:+new BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        wires(3).on = false
        wires(4).on = true
        wires(5).on = true
        wires(0).disabled = false
        wires(3).disabled = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = false
        torches(3).on = false
        chips(0).on = false
        chips(1).on = true
        chips(2).on = true
    }

    override def prepare(gate:ComboGatePart)
    {
        val state = gate.state
        wires(0).on = (state>>4) == 2
        wires(1).on = (state>>4) == 8
        wires(2).on = (state&4) != 0
        wires(3).on = (state&4) != 0
        wires(4).on = (state>>4) == 1 || (state>>4) == 2
        wires(5).on = (state>>4) == 1
        wires(0).disabled = gate.shape != 0
        wires(3).disabled = gate.shape != 0
        torches(0).on = (state>>4) == 1
        torches(1).on = (state>>4) == 2
        torches(2).on = (state>>4) == 8
        torches(3).on = !wires(4).on
        chips(0).on = (state>>4) == 2
        chips(1).on = (state>>4) == 1 || (state>>4) == 2
        chips(2).on = true
    }
}
