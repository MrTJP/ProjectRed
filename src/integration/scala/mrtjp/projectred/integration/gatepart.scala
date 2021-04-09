/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.VoxelShapeCache
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.buffer.TransformingVertexBuilder
import codechicken.lib.vec._
import codechicken.microblock.FaceMicroFactory
import codechicken.multipart.api.part.{ITickablePart, TMultiPart, TNormalOcclusionPart}
import codechicken.multipart.block.TileMultiPart
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.collect.ImmutableSet
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.{Configurator, PRLib, TFaceConnectable, TSwitchPacket}
import net.minecraft.block.SoundType
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{ItemStack, ItemUseContext}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.shapes.VoxelShape
import net.minecraft.util.{ActionResultType, Direction, Hand}
import net.minecraft.world.chunk.Chunk
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.{Collections, Collection => JCollection}

abstract class GatePart(gateType:GateType) extends TMultiPart with TNormalOcclusionPart with TFaceConnectable with TSwitchPacket with ITickablePart //with TIconHitEffectsPart
{
//    private var gateSubID:Byte = 0
    private var gateShape:Byte = 0

    var schedTime = 0L

    def getLogic[T]:T
    def getLogicPrimitive = getLogic[GateLogic[GatePart]]

//    def subID = gateSubID&0xFF

    def shape = gateShape&0xFF
    def setShape(s:Int){ gateShape = s.toByte }

    def preparePlacement(player:PlayerEntity, pos:BlockPos, side:Int)
    {
//        gateSubID = gateType.ordinal().toByte
        setSide(side^1)
        setRotation((Rotation.getSidedRotation(player, side)+2)%4)
    }

    override def save(tag:CompoundNBT)
    {
        tag.putByte("orient", orientation)
//        tag.putByte("subID", gateSubID)
        tag.putByte("shape", gateShape)
        tag.putInt("connMap", connMap)
        tag.putLong("schedTime", schedTime)
    }

    override def load(tag:CompoundNBT)
    {
        orientation = tag.getByte("orient")
//        gateSubID = tag.getByte("subID")
        gateShape = tag.getByte("shape")
        connMap = tag.getInt("connMap")
        schedTime = tag.getLong("schedTime")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(orientation)
//        packet.writeByte(gateSubID)
        packet.writeByte(gateShape)
    }

    override def readDesc(packet:MCDataInput)
    {
        orientation = packet.readByte()
//        gateSubID = packet.readByte()
        gateShape = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            orientation = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case 2 =>
            gateShape = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case _ => super.read(packet, key)
    }

    override def setRenderFlag(part:IConnectable) = false

    override def discoverOpen(dir:Int) = true

    override def canConnectPart(part:IConnectable, r:Int) =
        getLogicPrimitive.canConnectTo(this, part, toInternal(r))

    override def canConnectCorner(r:Int) = false

    override def scheduledTick()
    {
        getLogicPrimitive.scheduledTick(this)
    }

    override def scheduleTick(ticks:Int)
    {
        if (schedTime < 0) schedTime = world.getGameTime+ticks
    }

    def processScheduled()
    {
        if (schedTime >= 0 && world.getGameTime >= schedTime)
        {
            schedTime = -1
            scheduledTick()
        }
    }

    def onChange()
    {
        processScheduled()
        getLogicPrimitive.onChange(this)
    }

    override def tick()
    {
        if (!world.isRemote) processScheduled()
        getLogicPrimitive.onTick(this)
    }

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote)
        {
            updateOutward()
            onChange()
        }
    }

    override def onNeighborBlockChanged(from:BlockPos)
    {
        if (!world.isRemote)
        {
            if (dropIfCantStay()) return
            updateExternalConns()
            onChange()
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote)
        {
            getLogicPrimitive.setup(this)
            updateInward()
            onChange()
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) notifyAllExternals()
    }

    override def onWorldJoin()
    {
        super.onWorldJoin()
        if (getLogic == null)
            tile.remPart(this)
    }

    override def onChunkLoad()
    {
        super.onChunkLoad()
        if (tile != null)
            getLogicPrimitive.onWorldLoad(this)
    }

    def canStay = PRLib.canPlaceGateOnSide(world, pos.offset(Direction.byIndex(side)), Direction.byIndex(side^1))

    def dropIfCantStay() =
    {
        if (!canStay)
        {
            drop()
            true
        }
        else false
    }

    def drop() {
        TileMultiPart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
        tile.remPart(this)
    }

    def getGateType:GateType = gateType

    def getItem = gateType.makeStack

    override def getDrops:JCollection[ItemStack] = Collections.singleton(getItem)

    override def pickItem(hit:PartRayTraceResult) = getItem

//    def getGateDef = GateDefinition(subID)

    override def getType = gateType.getPartType

    //    override def getBounds = getLogicPrimitive.getBounds(this)
//
//    override def getSubParts = {
//        Seq(new IndexedCuboid6(-1, getBounds))++getLogicPrimitive.getSubParts(this)
//    }
//
//    override def getOcclusionBoxes = getLogicPrimitive.getOcclusions(this)
//
    override def getOcclusionShape = getLogicPrimitive.getOcclusionShape(this)

    override def getCollisionShape = getLogicPrimitive.getCollisionShape(this)

    override def getOutlineShape = getLogicPrimitive.getOutlineShape(this)

    override def getStrength(player:PlayerEntity, hit:PartRayTraceResult) = 2/30f

    override def getSlotMask = 1<<side

    override def solid(side:Int) = false

    override def getLightValue = getLogicPrimitive.lightLevel

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, held:ItemStack, hand:Hand):ActionResultType =
    {
        if (getLogicPrimitive.activate(this, player, held, hit)) return ActionResultType.SUCCESS

        if (!held.isEmpty && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held)) {
            if (!world.isRemote) {
                if (player.isSneaking) configure()
                else rotate()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            return ActionResultType.SUCCESS
        }
        ActionResultType.PASS
    }

    def configure()
    {
        if (getLogicPrimitive.cycleShape(this)) {
            updateInward()
            tile.markDirty()
            tile.notifyPartChange(this)
            sendShapeUpdate()
            notifyExternals(0xF)
            onChange()
        }
    }

    def rotate()
    {
        setRotation((rotation+1)%4)
        updateInward()
        tile.markDirty()
        tile.notifyPartChange(this)
        sendOrientUpdate()
        notifyExternals(0xF)
        onChange()
    }

    def sendShapeUpdate()
    {
        sendUpdate(2, _.writeByte(gateShape))
    }

    def sendOrientUpdate()
    {
        sendUpdate(1, _.writeByte(orientation))
    }

//    @SideOnly(Side.CLIENT)
//    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
//    {
//        if (layer == BlockRenderLayer.CUTOUT && Configurator.staticGates) {
//            ccrs.setBrightness(world, this.pos)
//            RenderGate.renderStatic(this, pos, ccrs)
//            true
//        }
//        else false
//    }

    @OnlyIn(Dist.CLIENT)
    override def renderStatic(layer:RenderType, ccrs:CCRenderState) = {
        if (layer == null || (layer == RenderType.getCutout && Configurator.staticGates)) {
            ccrs.setBrightness(world, this.pos)
            RenderGate.renderStatic(this, Vector3.ZERO, ccrs)
            true
        } else
            false
    }


    //    @SideOnly(Side.CLIENT)
//    override def renderDynamic(pos:Vector3, pass:Int, frame:Float)
//    {
//        val ccrs = CCRenderState.instance()
//        TextureUtils.bindBlockTexture()
//        if (!Configurator.staticGates) {
//            GL11.glDisable(GL11.GL_LIGHTING)
//            ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
//            RenderGate.renderStatic(this, pos, ccrs)
//            ccrs.draw()
//            GL11.glEnable(GL11.GL_LIGHTING)
//        }
//        RenderGate.renderDynamic(this, pos, frame, ccrs)
//    }
    //    override def canRenderDynamic(pass: Int) = pass == 0
    @OnlyIn(Dist.CLIENT)
    override def renderDynamic(mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.brightness = packedLight
        ccrs.overlay = packedOverlay
        ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.getCutout), mStack), DefaultVertexFormats.BLOCK)
        RenderGate.renderDynamic(this, Vector3.ZERO, partialTicks, ccrs)
    }

//    @SideOnly(Side.CLIENT)
//    override def getBreakingIcon(hit:CuboidRayTraceResult) = getBrokenIcon(hit.sideHit.ordinal)

//    @SideOnly(Side.CLIENT)
//    override def getBrokenIcon(side:Int) = ComponentStore.baseIcon

    override def getPlacementSound(context:ItemUseContext):SoundType = SoundType.GLASS
}

object GatePart
{
    val oBoxes = Array.ofDim[Cuboid6](6, 2)
    val oShapes = new Array[VoxelShape](6)

    for (s <- 0 until 6)  {
        val t = Rotation.sideRotations(s).at(Vector3.CENTER)
        val occlusion1 = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1)
        val occlusion2 = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D)

        oBoxes(s)(0) = occlusion1.apply(t)
        oBoxes(s)(1) = occlusion2.apply(t)
        oShapes(s) = VoxelShapeCache.merge(ImmutableSet.copyOf(oBoxes(s).map(VoxelShapeCache.getShape)))
    }
}

abstract class GateLogic[T <: GatePart]
{
    def canConnectTo(gate:T, part:IConnectable, r:Int):Boolean

    def cycleShape(gate:T) = false

    def onChange(gate:T)

    def scheduledTick(gate:T)

    def onTick(gate:T){}

    def setup(gate:T){}
    def onWorldLoad(gate:T){}

    def activate(gate:T, player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult) = false

//    def getBounds(gate:T) = FaceMicroFactory.aBounds(0x10|gate.side)
//    def getSubParts(gate:T) = Seq[IndexedCuboid6]()


    def getOutlineShape(gate:T):VoxelShape = FaceMicroFactory.aShapes(0x10|gate.side)
    def getCollisionShape(gate:T):VoxelShape = FaceMicroFactory.aShapes(0x10|gate.side)
    def getOcclusionShape(gate:T):VoxelShape = GatePart.oShapes(gate.side)

    def lightLevel = 7
}

trait TComplexGatePart extends GatePart
{
    def getLogicComplex = getLogic[TComplexGateLogic[TComplexGatePart]]

    def assertLogic()

    abstract override def save(tag:CompoundNBT)
    {
        super.save(tag)
        getLogicComplex.save(tag)
    }

    abstract override def load(tag:CompoundNBT)
    {
        super.load(tag)
        assertLogic()
        getLogicComplex.load(tag)
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        getLogicComplex.writeDesc(packet)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        assertLogic()
        getLogicComplex.readDesc(packet)
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case k if k > 10 => getLogicComplex.read(packet, k)
        case _ => super.read(packet, key)
    }

    abstract override def preparePlacement(player:PlayerEntity, pos:BlockPos, side:Int)
    {
        super.preparePlacement(player, pos, side)
        assertLogic()
    }
}

trait TComplexGateLogic[T <: TComplexGatePart] extends GateLogic[T]
{
    def save(tag:CompoundNBT){}
    def load(tag:CompoundNBT){}

    def readDesc(packet:MCDataInput){}
    def writeDesc(packet:MCDataOutput){}

    def read(packet:MCDataInput, key:Int){}
}
