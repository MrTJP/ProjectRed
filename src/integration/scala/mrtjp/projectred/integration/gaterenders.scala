/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.colour.EnumColour
import codechicken.lib.math.MathHelper
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.{AtlasRegistrar, IIconRegister}
import codechicken.lib.vec.{RedundantTransformation, Transformation}
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.vec.VecLib
import mrtjp.projectred.core.TFaceOrient.flipMaskZ
import mrtjp.projectred.integration.ComponentStore._
import net.minecraft.client.renderer.IRenderTypeBuffer
import net.minecraft.item.ItemStack
import net.minecraft.particles.RedstoneParticleData
import net.minecraft.resources.IResourceManager
import net.minecraftforge.resource.{IResourceType, ISelectiveResourceReloadListener, VanillaResourceType}

import java.util.Random
import java.util.function.Predicate

class RenderGate
{
    private val partRenderers = createPartRenderers()
    private val nonPartRenderers = createNonPartRenderers()

    private def createPartRenderers():Array[GateRenderer] = Array(
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
        new RenderFabricatedGate
    )

    private def createNonPartRenderers():Array[GateRenderer] = Array(
        new RenderIOGate
    )

    private def getRenderer(renderIndex:Int):GateRenderer = {
        if ((renderIndex & 0x100) != 0)
            nonPartRenderers(renderIndex&0xFF)
        else
            partRenderers(renderIndex&0xFF)
    }

    def renderStatic(gate:GatePart, ccrs:CCRenderState):Unit =
        renderStatic(gate.getGateType, gate, gate.orientation, new RedundantTransformation, ccrs)

    def renderDynamic(gate:GatePart, partialTicks:Float, ccrs:CCRenderState):Unit =
        renderDynamic(gate.getGateType, gate, gate.orientation, new RedundantTransformation, partialTicks, ccrs)

    def renderCustomDynamic(gate:GatePart, mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float, ccrs:CCRenderState):Unit =
        renderCustom(gate.getGateType, gate, gate.orientation, new RedundantTransformation, mStack, buffers, packedLight, packedOverlay, partialTicks, ccrs)

    def spawnParticles(gate:GatePart, rand:Random):Unit = {
        val r = partRenderers(gate.getGateType.ordinal)
        r.prepare(gate)
        r.spawnParticles(gate, rand)
    }

    def renderInv(stack:ItemStack, t:Transformation, gateType:GateType, ccrs:CCRenderState):Unit =
        renderInv(stack, gateType, 0, t, ccrs)

    def renderStatic(gateType:GateType, key:IGateRenderKey, orientation:Int, t:Transformation, ccrs:CCRenderState):Unit =
        renderStatic(gateType.ordinal(), key, orientation, t, ccrs)

    def renderDynamic(gateType:GateType, key:IGateRenderKey, orientation:Int, t:Transformation, partialTicks:Float, ccrs:CCRenderState):Unit =
        renderDynamic(gateType.ordinal(), key, orientation, t, partialTicks, ccrs)

    def renderCustom(gateType:GateType, key: IGateRenderKey, orientation:Int, t:Transformation, mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float, ccrs:CCRenderState):Unit =
        renderCustom(gateType.ordinal(), key, orientation, t, mStack, buffers, packedLight, packedOverlay, partialTicks, ccrs)

    def renderInv(stack:ItemStack, gateType:GateType, orient:Int, t:Transformation, ccrs:CCRenderState):Unit =
        renderInv(gateType.ordinal(), orient, t, ccrs)

    def renderStatic(renderIndex:Int, key:IGateRenderKey, orientation:Int, t:Transformation, ccrs:CCRenderState):Unit = {
        val r = getRenderer(renderIndex)
        r.prepare(key)
        r.renderStatic(t, orientation, ccrs)
    }

    def renderDynamic(renderIndex:Int, key:IGateRenderKey, orientation:Int, t:Transformation, partialTicks:Float, ccrs:CCRenderState):Unit = {
        val r = getRenderer(renderIndex)
        if (r.hasSpecials) {
            r.prepareDynamic(key, partialTicks)
            r.renderDynamic(VecLib.orientT(orientation).`with`(t), ccrs)
        }
    }

    def renderCustom(renderIndex:Int, key: IGateRenderKey, orientation:Int, t:Transformation, mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float, ccrs:CCRenderState):Unit = {
        val r = getRenderer(renderIndex)
        r.renderCustomDynamic(key, VecLib.orientT(orientation).`with`(t), mStack, buffers, packedLight, packedOverlay, partialTicks, ccrs)
    }

    def renderInv(renderIndex:Int, orient:Int, t:Transformation, ccrs:CCRenderState):Unit = {
        val r = getRenderer(renderIndex)
        r.prepareInv()
        r.renderStatic(t, orient, ccrs)
        if (r.hasSpecials) r.renderDynamic(t, ccrs)
    }
}

object RenderGate extends IIconRegister with ISelectiveResourceReloadListener
{
    private val instances = new ThreadLocal[RenderGate]() {
        override def initialValue() = new RenderGate
    }

    def instance():RenderGate = instances.get()

    def getRenderIndex(gate:GatePart):Int = gate.getGateType.ordinal()
    def getRenderIndex(gateType:GateType):Int = gateType.ordinal()
    def getNonPartRenderIndex(id:Int):Int = 0x100 | (id & 0xFF)

    override def registerIcons(map:AtlasRegistrar):Unit = {
        ComponentStore.registerIcons(map)
        for (m <- instance().partRenderers) m.registerIcons(map)
    }

    override def onResourceManagerReload(resourceManager:IResourceManager, resourcePredicate:Predicate[IResourceType]):Unit = {
        if (resourcePredicate.test(VanillaResourceType.TEXTURES))
            WireModel3D.regenerateModels()
    }
}

abstract class GateRenderer
{
    var reflect = false

    def coreModels:Seq[ComponentModel]
    def switchModels = Seq[ComponentModel]()
    def allSwitchModels = Seq[ComponentModel]()

    private def enabledModels = coreModels++switchModels
    private def allModels = coreModels++allSwitchModels

    def registerIcons(map:AtlasRegistrar)
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

    def renderCustomDynamic(gate:IGateRenderKey, t:Transformation, mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float, ccrs:CCRenderState):Unit = {}

    def prepareInv(){}
    def prepare(gate:IGateRenderKey){}
    def prepareDynamic(gate:IGateRenderKey, frame:Float){}

    def spawnParticles(gate:GatePart, rand:Random)
    {
        val torches = enabledModels.collect {
            case t:TRedstoneTorchModel if t.on => t
        }

        for (t <- torches) if (rand.nextInt(torches.length) == 0) {
            val pos = t.getLightPos.copy.add(
                (rand.nextDouble() - 0.5D) * 0.2D, 0, (rand.nextDouble() - 0.5D) * 0.2D)

            pos.apply(gate.rotationT).add(gate.pos)

            val f = 1.0F // redstone strength, 0-1
            val f1 = f * 0.6F + 0.4F
            val f2 = Math.max(0.0F, f * f * 0.7F - 0.5F)
            val f3 = Math.max(0.0F, f * f * 0.6F - 0.7F)

            gate.world.addParticle(new RedstoneParticleData(f1, f2, f3, 1.0F), pos.x, pos.y, pos.z, 0, 0, 0)
        }
    }
}

trait IGateRenderKey {
    // General gates
    def shape:Int
    def state:Int
    def state2:Int = 0

    // Timer-like gates with pointers
    def isPointerStarted:Boolean = false
    def pointerValue:Int = 0
    def pointerMax:Int = 1

    // Bundled gates
    def bOutput0:Short = 0
    def bOutput1:Short = 0
    def bOutput2:Short = 0
    def bOutput3:Short = 0
    def bInput0:Short = 0
    def bInput1:Short = 0
    def bInput2:Short = 0
    def bInput3:Short = 0

    // Bus converter //TODO squash
    def rsIO:Int = 0

    // Segment display
    def bInHigh:Int = 0 //TODO This is just bundled input side 0
    def segmentColour:Byte = 0 // TODO this can be a state2

    // Array cells
    def bottomSignal:Byte = 0
    def topSignal:Byte = 0
    def topSignalConnMask:Int = 0
}

class RenderOR extends GateRenderer
{
    val wires = generateWireModels("or", 4)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 9, 6), new RedstoneTorchModel(8, 2.5, 8))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey):Unit = {
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

class RenderNOR extends GateRenderer
{
    var wires = generateWireModels("nor", 4)
    var torch = new RedstoneTorchModel(8, 9, 6)

    override val coreModels = wires:+torch:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderNOT extends GateRenderer
{
    val wires = generateWireModels("not", 4)
    val torch = new RedstoneTorchModel(8, 8, 6)

    override val coreModels = wires:+torch:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderAND extends GateRenderer
{
    val wires = generateWireModels("and", 4)
    val torches = IndexedSeq(new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderNAND extends GateRenderer
{
    val wires = generateWireModels("nand", 4)
    val torches = IndexedSeq(new RedstoneTorchModel(4, 8, 6), new RedstoneTorchModel(12, 8, 6),
        new RedstoneTorchModel(8, 8, 6))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderXOR extends GateRenderer
{
    val wires = generateWireModels("xor", 4)
    val torches = IndexedSeq(new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6),
        new RedstoneTorchModel(8, 12, 6))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderXNOR extends GateRenderer
{
    val wires = generateWireModels("xnor", 5)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(4.5, 8, 6),
        new RedstoneTorchModel(11.5, 8, 6), new RedstoneTorchModel(8, 12, 6))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderBuffer extends GateRenderer
{
    val wires = generateWireModels("buffer", 4)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 3.5, 8), new RedstoneTorchModel(8, 9, 6))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderMultiplexer extends GateRenderer
{
    val wires = generateWireModels("multiplexer", 6)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(9, 10.5, 6),
        new RedstoneTorchModel(4.5, 8, 6), new RedstoneTorchModel(11.5, 8, 6))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderPulse extends GateRenderer
{
    val wires = generateWireModels("pulse", 3)
    val torches = IndexedSeq(new RedstoneTorchModel(4, 9.5, 6), new RedstoneTorchModel(11, 9.5, 6),
        new RedstoneTorchModel(8, 3.5, 8))

    override val coreModels = wires++torches:+BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = true
        wires(1).on = false
        wires(2).on = false
        torches(0).on = true
        torches(1).on = false
        torches(2).on = false
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&4) == 0
        wires(1).on = (gate.state&4) != 0
        wires(2).on = (gate.state&0x14) == 4
        torches(0).on = wires(0).on
        torches(1).on = wires(1).on
        torches(2).on = (gate.state&0x10) != 0
    }
}

class RenderRepeater extends GateRenderer
{
    val wires = generateWireModels("repeater", 2)
    val endTorch = new RedstoneTorchModel(8, 2, 6)
    val varTorches = IndexedSeq(new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(12.5, 11, 6),
        new RedstoneTorchModel(12.5, 10, 6), new RedstoneTorchModel(12.5, 9, 6), new RedstoneTorchModel(12.5, 8, 6),
        new RedstoneTorchModel(12.5, 7, 6), new RedstoneTorchModel(12.5, 6, 6), new RedstoneTorchModel(12.5, 5, 6),
        new RedstoneTorchModel(12.5, 4, 6))

    var shape = 0

    override val coreModels = wires++Seq(endTorch, BaseComponentModel)

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

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&0x10) == 0
        wires(1).on = (gate.state&4) != 0
        endTorch.on = (gate.state&0x10) != 0
        shape = gate.shape
        varTorches(shape).on = (gate.state&4) == 0
    }
}

class RenderRandomizer extends GateRenderer
{
    val wires = generateWireModels("rand", 7)
    val chips = IndexedSeq(new YellowChipModel(8, 5.5), new YellowChipModel(11.5, 11.5), new YellowChipModel(4.5, 11.5))

    override val coreModels = wires++chips:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderSRLatch extends GateRenderer
{
    val wires1 = generateWireModels("rslatch", 2)
    val wires2 = generateWireModels("rslatch2", 4)
    val torches1 = IndexedSeq(new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 13, 6))
    val torches2 = IndexedSeq(new RedstoneTorchModel(9.5, 3, 6), new RedstoneTorchModel(6.5, 13, 6))
    var shape = 0

    override val coreModels = Seq(BaseComponentModel)
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

    override def prepare(gate:IGateRenderKey)
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

class RenderToggleLatch extends GateRenderer
{
    val wires = generateWireModels("toglatch", 2)
    val torches = IndexedSeq(new RedstoneTorchModel(4, 4, 6), new RedstoneTorchModel(4, 12, 6))
    val lever = new LeverModel(11, 8)

    override val coreModels = wires++torches++Seq(lever, BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        torches(0).on = true
        torches(1).on = false
        lever.state = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&8) != 0
        wires(1).on = (gate.state&2) != 0
        torches(0).on = (gate.state&0x10) != 0
        torches(1).on = (gate.state&0x40) != 0
        lever.state = if ((gate.state&0x10) != 0) 0 else 1
    }
}

class RenderTransparentLatch extends GateRenderer
{
    val wires = generateWireModels("translatch", 5)
    val torches = IndexedSeq(new RedstoneTorchModel(4, 12.5, 6), new RedstoneTorchModel(4, 8, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8), new RedstoneTorchModel(14, 8, 8))

    override val coreModels = wires++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderLightSensor extends GateRenderer
{
    val wires = generateWireModels("lightsensor", 1)
    val solar = new SolarModel(8, 5.5)

    override val coreModels = wires++Seq(solar, BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        solar.state = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&0xF4) != 0
        solar.state = gate.shape
    }
}

class RenderRainSensor extends GateRenderer
{
    val wires = generateWireModels("rainsensor", 1)
    val sensor = new RainSensorModel(8, 6)

    override val coreModels = wires++Seq(sensor, BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&0x44) != 0
    }
}

class RenderTimer extends GateRenderer
{
    val wires = generateWireModels("time", 3)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 3, 6), new RedstoneTorchModel(8, 8, 12))
    val pointer = new PointerModel(8, 8, 8)

    override val coreModels = wires++torches:+BaseComponentModel

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        torches(0).on = false
        pointer.angle = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        torches(0).on = (gate.state&0x10) != 0
        wires(0).on = (gate.state&0x88) != 0
        wires(1).on = (gate.state&0x22) != 0
        wires(2).on = (gate.state&4) != 0
    }

    override def hasSpecials = true

    override def prepareDynamic(part:IGateRenderKey, frame:Float)
    {
        val interpPointer = if (!part.isPointerStarted) 0f else (part.pointerValue+frame)/part.pointerMax
        pointer.angle = interpPointer*MathHelper.pi*2
    }

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
//        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//        ccrs.pullLightmap()
        pointer.renderModel(t, 0, ccrs)
//        ccrs.draw()
    }
}

class RenderSequencer extends GateRenderer
{
    val torches = IndexedSeq(new RedstoneTorchModel(8, 8, 12), new RedstoneTorchModel(8, 3, 6),
        new RedstoneTorchModel(13, 8, 6), new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(3, 8, 6))
    val pointer = new PointerModel(8, 8, 8)

    torches(0).on = true

    override val coreModels = torches:+BaseComponentModel

    override def prepare(gate:IGateRenderKey)
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

    override def prepareDynamic(gate:IGateRenderKey, frame:Float)
    {
        val interpPointer = (gate.pointerValue+frame) / gate.pointerMax
        pointer.angle = interpPointer*MathHelper.pi*2
        if (gate.shape == 1) pointer.angle = -pointer.angle
    }

    override def hasSpecials = true

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
//        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//        ccrs.pullLightmap()
        pointer.renderModel(t, 0, ccrs)
//        ccrs.draw()
    }
}

class RenderCounter extends GateRenderer
{
    val wires = generateWireModels("count", 2)
    val torches = IndexedSeq(new RedstoneTorchModel(11, 8, 12), new RedstoneTorchModel(8, 3, 6),
        new RedstoneTorchModel(8, 13, 6))
    val pointer = new PointerModel(11, 8, 8, 1.2D)

    torches(0).on = true

    override val coreModels = wires++torches:+BaseComponentModel

    override def prepare(gate:IGateRenderKey)
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

    override def prepareDynamic(gate:IGateRenderKey, frame:Float)
    {
        val interpPointer = (gate.pointerValue/gate.pointerMax.toDouble*(340-220)+210)
        pointer.angle = interpPointer * MathHelper.torad
        reflect = gate.shape == 1
    }

    override def hasSpecials = true

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
//        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//        ccrs.pullLightmap()
        pointer.renderModel(t, if (reflect) 1 else 0, ccrs)
//        ccrs.draw()
    }
}

class RenderStateCell extends GateRenderer
{
    val wires = generateWireModels("statecell", 5)
    val torches = IndexedSeq(new RedstoneTorchModel(10, 3.5, 6), new RedstoneTorchModel(13, 8, 12))
    val chip = new RedChipModel(6.5, 10)
    val pointer = new PointerModel(13, 8, 8)

    override val coreModels = wires++torches++Seq(chip, BaseComponentModel)

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

    override def prepare(part:IGateRenderKey)
    {
        reflect = part.shape == 1
        var state = part.state
        if (reflect) state = flipMaskZ(state>>4)<<4|flipMaskZ(state)

        wires(0).on = (state&0x10) != 0
        wires(1).on = (state&4) != 0
        wires(2).on = part.state2 == 0 || (state&4) != 0
        wires(3).on = (state&0x88) != 0
        wires(4).on = (state&2) != 0
        torches(0).on = (state&0x10) != 0
        torches(1).on = part.isPointerStarted
        chip.on = part.state2 != 0
    }

    override def hasSpecials = true

    override def prepareDynamic(part:IGateRenderKey, frame:Float)
    {
        reflect = part.shape == 1
        val interpPointer = if (!part.isPointerStarted) 0f else (part.pointerValue+frame)/part.pointerMax
        pointer.angle = interpPointer - MathHelper.pi/2 //TODO This seems sus
    }

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
//        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//        ccrs.pullLightmap()
        pointer.renderModel(t, if (reflect) 1 else 0, ccrs)
//        ccrs.draw()
    }
}

class RenderSynchronizer extends GateRenderer
{
    val wires = generateWireModels("sync", 6)
    val torch = new RedstoneTorchModel(8, 3, 6)
    val chips = IndexedSeq(new RedChipModel(4.5, 9), new RedChipModel(11.5, 9))

    override val coreModels = wires++chips++Seq(torch, BaseComponentModel)

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

    override def prepare(gate:IGateRenderKey)
    {
        val right = (gate.state2&1) != 0
        val left = (gate.state2&2) != 0

        wires(0).on = !left
        wires(1).on = !right
        wires(2).on = (gate.state&4) != 0
        wires(3).on = left && right
        wires(4).on = (gate.state&8) != 0
        wires(5).on = (gate.state&2) != 0
        chips(0).on = left
        chips(1).on = right
        torch.on = (gate.state&0x10) != 0
    }
}

class RenderBusXcvr extends GateRenderer
{
    val wires = generateWireModels("busxcvr", 2)
    val panels = IndexedSeq(new SigLightPanelModel(4, 8, false), new SigLightPanelModel(12, 8, true))

    override val coreModels = wires++panels++Seq(BusXcvrCableModel, BaseComponentModel)

    override def prepareInv()
    {
        reflect = false
        wires(0).on = false
        wires(1).on = false
        panels(0).signal = 0
        panels(1).signal = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        reflect = gate.shape != 0
        var state = gate.state
        if (reflect) state = flipMaskZ(state)

        wires(0).on = (state&2) != 0
        wires(1).on = (state&8) != 0
        panels(0).signal = gate.bOutput2
        panels(1).signal = gate.bOutput0
    }
}

class RenderComparator extends GateRenderer
{
    val wires = generateWireModels("comparator", 4)
    val torch = new RedstoneTorchModel(8, 2, 6)
    val chips = IndexedSeq(new MinusChipModel(5, 8), new PlusChipModel(11, 8))

    override val coreModels = wires++Seq(torch, BaseComponentModel)

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

    override def prepare(gate:IGateRenderKey)
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

class RenderBusRandomizer extends GateRenderer
{
    val panel = new SigLightPanelModel(8, 8, true)
    val wires1 = generateWireModels("busrand1", 2)
    val wires2 = generateWireModels("busrand2", 2)

    var shape = 0

    panel.offColour = 0x756900FF
    panel.onColour = 0xe1d600FF

    override val coreModels = Seq(BusRandCableModel, panel, BaseComponentModel)

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

    override def prepare(part:IGateRenderKey)
    {
        shape = part.shape
        panel.signal = part.bOutput0
        panel.disableMask = ~part.bInput2

        wires1(0).on = (part.state&2) != 0
        wires2(0).on = (part.state&2) != 0
        wires1(1).on = (part.state&8) != 0
        wires2(1).on = (part.state&8) != 0
    }
}

class RenderBusConverter extends GateRenderer
{
    val wires = generateWireModels("busconv", 3)
    val bar = new SignalBarModel(8, 8)

    override val coreModels = wires++Seq(BusConvCableModel, bar, BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        wires(1).on = false
        wires(2).on = false
        bar.signal = 0
        bar.inverted = false
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&0x20) != 0
        wires(1).on = (gate.state&0x80) != 0
        wires(2).on = gate.rsIO != 0
        bar.inverted = gate.shape != 0
        bar.signal = gate.rsIO
    }
}

class RenderBusInputPanel extends GateRenderer
{
    val wires = generateWireModels("businput", 1)
    val buttons = new InputPanelButtonsModel

    override val coreModels = wires++Seq(buttons, BusInputPanelCableModel, BaseComponentModel)

    override def prepareInv()
    {
        wires(0).on = false
        buttons.pressMask = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        wires(0).on = (gate.state&1) != 0
        buttons.pressMask = gate.bInput0
    }

    override def renderCustomDynamic(gate:IGateRenderKey, t:Transformation, mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float, ccrs:CCRenderState):Unit = {
        buttons.pressMask = gate.bInput0
        buttons.renderLights(ccrs, mStack, buffers, t)
    }
}

abstract class RenderArrayCell extends GateRenderer
{
    val topWire:CellTopWireModel
    val bottomWire:CellBottomWireModel

    override def prepareInv()
    {
        bottomWire.signal = 0
        topWire.signal = 0
        topWire.conn = 0
    }

    override def prepare(gate:IGateRenderKey)
    {
        bottomWire.signal = gate.bottomSignal
        topWire.signal = gate.topSignal
        topWire.conn = gate.topSignalConnMask//IGateWireRenderConnect.getConnsAtHeight(gate, 10.0D)
    }
}

class RenderNullCell extends RenderArrayCell
{
    override val topWire:CellTopWireModel = new NullCellTopWireModel
    override val bottomWire:CellBottomWireModel = new NullCellBottomWireModel
    override val coreModels = Seq(bottomWire, topWire, CellFrameModel, NullCellBaseModel)
}

class RenderInvertCell extends RenderArrayCell
{
    val wires = generateWireModels("invcell", 1)
    val torch = new RedstoneTorchModel(8, 8, 6)

    override val topWire:CellTopWireModel = new NullCellTopWireModel
    override val bottomWire:CellBottomWireModel = new ExtendedCellBottompWireModel
    override val coreModels = wires++Seq(torch, bottomWire, topWire, CellFrameModel, CellPlateModel, ExtendedCellBaseModel)

    override def prepareInv()
    {
        super.prepareInv()
        topWire.signal = 255.toByte
        wires(0).on = false
        torch.on = true
    }

    override def prepare(gate:IGateRenderKey)
    {
        super.prepare(gate)
        torch.on = gate.bottomSignal == 0
        wires(0).on = gate.bottomSignal != 0
    }
}

class RenderBufferCell extends RenderArrayCell
{
    val wires = generateWireModels("buffcell", 2)
    val torches = IndexedSeq(new RedstoneTorchModel(11, 13, 6), new RedstoneTorchModel(8, 8, 6))

    override val topWire:CellTopWireModel = new NullCellTopWireModel
    override val bottomWire:CellBottomWireModel = new ExtendedCellBottompWireModel
    override val coreModels = wires++torches++Seq(topWire, bottomWire, CellFrameModel, CellPlateModel, ExtendedCellBaseModel)

    override def prepareInv()
    {
        super.prepareInv()
        wires(0).on = false
        wires(1).on = true
        torches(0).on = true
        torches(1).on = false
    }

    override def prepare(gate:IGateRenderKey)
    {
        super.prepare(gate)
        torches(0).on = gate.bottomSignal == 0
        torches(1).on = gate.bottomSignal != 0
        wires(0).on = gate.bottomSignal != 0
        wires(1).on = gate.bottomSignal == 0
    }
}

class RenderANDCell extends GateRenderer
{
    val wires = generateWireModels("andcell", 2)
    val torches = IndexedSeq(new RedstoneTorchModel(8, 13, 6), new RedstoneTorchModel(8, 2, 8), new FlippedRSTorchModel(8, 8))
    val topWire = new NullCellTopWireModel

    override val coreModels = wires++torches++Seq(topWire, CellFrameModel, BaseComponentModel)

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

    override def prepare(gate:IGateRenderKey)
    {
        topWire.signal = gate.topSignal
        topWire.conn = gate.topSignalConnMask
        torches(0).on = (gate.state&4) == 0
        torches(1).on = (gate.state&0x10) != 0
        torches(2).on = gate.topSignal == 0
        wires(0).on = torches(0).on || torches(2).on
        wires(1).on = !torches(0).on
    }
}

class RenderStackingLatch extends GateRenderer
{
    var wires = generateWireModels("stacklatch", 5)
    var clkwire = new StackLatchWireModel
    var torches = IndexedSeq(new RedstoneTorchModel(12.5, 12, 6), new RedstoneTorchModel(8, 12, 6),
        new RedstoneTorchModel(8, 8, 6), new RedstoneTorchModel(8, 2, 8))

    override val coreModels = wires++torches++Seq(clkwire, new StackLatchStandModel(3.5, 5),
        new StackLatchStandModel(12.5, 5), StackLatchBaseModel)

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

    override def prepare(gate:IGateRenderKey)
    {
        val on = (gate.state&0x10) != 0
        val sig = gate.topSignal
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

class RenderSegmentDisplay extends GateRenderer
{
    val sevenSeg1 = new SevenSegModel(4.5, 8)
    val sevenSeg0 = new SevenSegModel(11.5, 8)
    val sixteenSeg = new SixteenSegModel(8, 8)
    var shape = 0

    override val coreModels = Seq(SegmentBusCableModel, BaseComponentModel)
    override def switchModels = if (shape == 0) Seq(sevenSeg0, sevenSeg1) else Seq(sixteenSeg)
    override def allSwitchModels = Seq(sevenSeg0, sevenSeg1, sixteenSeg)

    override def prepareInv()
    {
        shape = 0
        sevenSeg1.signal = 64
        sevenSeg0.signal = 64
        sixteenSeg.signal = 0
        Seq(sevenSeg0, sevenSeg1, sixteenSeg).foreach(_.setOnColourIndex(EnumColour.RED.ordinal.toByte))
    }

    override def prepare(gate:IGateRenderKey)
    {
        shape = gate.shape
        val sig1 = gate.bInHigh
        val sig0 = gate.state
        sevenSeg1.signal = sig1
        sevenSeg0.signal = sig0
        sixteenSeg.signal = sig1<<8|sig0
        Seq(sevenSeg0, sevenSeg1, sixteenSeg).foreach(_.setOnColourIndex(gate.segmentColour))
    }
}

class RenderDecodingRand extends GateRenderer
{
    val wires = generateWireModels("decrand", 6)
    val chips = IndexedSeq(new YellowChipModel(5, 13), new YellowChipModel(11, 13), new RedChipModel(5.5, 8))
    val torches = IndexedSeq(new RedstoneTorchModel(8, 2.5, 8), new RedstoneTorchModel(14, 8, 8), new RedstoneTorchModel(2, 8, 8), new RedstoneTorchModel(9, 8, 6))

    override val coreModels = wires++chips++torches:+BaseComponentModel

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

    override def prepare(gate:IGateRenderKey)
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

class RenderFabricatedGate extends GateRenderer
{
    var simp = new SidedWireModel(generateWireModels("ic1", 4))
    var analog = new SidedWireModel(generateWireModels("ic2", 4))
    var bundled = new SidedICBundledCableModel
    var housing = new ICChipHousingModel

    var name = "untitled"

    override val coreModels = Seq(BaseComponentModel, simp, analog, bundled, new ICChipModel, housing)

    override def prepareInv()
    {
//        if (hasICInside(stack)) {
//            name = getICName(stack)
//            val cm = getConnModes(stack)
//            simp.sidemask = 0
//            analog.sidemask = 0
//            bundled.sidemask = 0xF
//        } else {
            name = "ERROR!"
            simp.sidemask = 0
            analog.sidemask = 0
            bundled.sidemask = 0xF
//        }

        simp.wires.foreach(_.on = false)
        analog.wires.foreach(_.on = false)
    }

    override def prepare(gate:IGateRenderKey)
    {
        simp.sidemask = 0//connTypeMask(Simple, gate.getLogicIC.connmodes)
        analog.sidemask = 0//connTypeMask(Analog, gate.getLogicIC.connmodes)
        bundled.sidemask = gate.state2 & 0xF | (gate.state2 >> 4) & 0xF

        simp.wires(0).on = (gate.state&0x11) != 0
        simp.wires(1).on = (gate.state&0x22) != 0
        simp.wires(2).on = (gate.state&0x44) != 0
        simp.wires(3).on = (gate.state&0x88) != 0
        analog.wires.zipWithIndex.foreach(w => w._1.on = simp.wires(w._2).on)
    }

    def connTypeMask(c:Int, conns:Array[Int]) =
    {
        var m = 0
        for (r <- 0 until 4)
            if (conns(r) == c) m |= 1<<r
        m
    }

    override def hasSpecials = true
    override def prepareDynamic(gate:IGateRenderKey, frame:Float)
    {
//        name = gate.getLogicIC.name
    }

    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
//        disableLighting()
//        enableBlend()
//        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
//        pushMatrix()

//        val s = GuiDraw.getStringWidth(name) max 93
//        val f = 9/16D*1.0/s
//        (new Rotation(90.0.toRadians, 1, 0, 0) `with` new Translation(8/16D*1/f, 2.26/16D, 11.25/16D*1/f) `with`
//            new Scale(f, 1, f) `with` t).glApply()
//        GuiDraw.drawStringC(name, 0, 0, 0xFFFFFFFF, false)
//
//        popMatrix()
//        enableLighting()
//        disableBlend()
//
//        //glass
//        enableBlend()
//        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
//        TextureUtils.bindBlockTexture()
//        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM)
//        ccrs.pullLightmap()
        housing.renderDynamic(t, ccrs)
//        ccrs.draw()
//        disableBlend()
    }
}

class RenderIOGate extends GateRenderer
{
    val wires = generateWireModels("fabio", 1)
    val crimpWire = new IOCrimpWireModel
    val colourBox = new IOCrimpColourBoxModel(3, 10.5)

    override val coreModels:Seq[ComponentModel] = wires ++ Seq(crimpWire, colourBox, IOCrimpConnectorModel, BaseComponentModel)

    override def prepareInv():Unit = {
        crimpWire.signal = 0
        colourBox.colour = 0
    }

    override def prepare(gate:IGateRenderKey):Unit = {
        wires(0).on = (gate.state & 0x44) != 0
        crimpWire.signal = if (wires(0).on) 255.toByte else 0
        colourBox.colour = gate.state2 & 0xF
        colourBox.isInput = gate.shape == 0
    }
}
