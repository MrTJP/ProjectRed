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
import codechicken.multipart.api.part.{ITickablePart, TIconHitEffectsPart, TMultiPart, TNormalOcclusionPart}
import codechicken.multipart.block.TileMultiPart
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.collect.ImmutableSet
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.{Configurator, PRLib, TFaceConnectable, TSwitchPacket}
import net.minecraft.block.SoundType
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{ItemStack, ItemUseContext}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.shapes.{ISelectionContext, VoxelShape}
import net.minecraft.util.{ActionResultType, Direction, Hand}
import net.minecraft.world.chunk.Chunk
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.{Collections, Collection => JCollection}

abstract class GatePart(gateType:GateType) extends TMultiPart with TNormalOcclusionPart with TFaceConnectable with TSwitchPacket with ITickablePart with TIconHitEffectsPart
{
    private var gateShape:Byte = 0

    var schedTime = 0L

    def shape = gateShape&0xFF
    def setShape(s:Int){ gateShape = s.toByte }

    def preparePlacement(player:PlayerEntity, pos:BlockPos, side:Int)
    {
        setSide(side^1)
        setRotation((Rotation.getSidedRotation(player, side)+2)%4)
    }

    override def save(tag:CompoundNBT)
    {
        tag.putByte("orient", orientation)
        tag.putByte("shape", gateShape)
        tag.putInt("connMap", connMap)
        tag.putLong("schedTime", schedTime)
    }

    override def load(tag:CompoundNBT)
    {
        orientation = tag.getByte("orient")
        gateShape = tag.getByte("shape")
        connMap = tag.getInt("connMap")
        schedTime = tag.getLong("schedTime")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(orientation)
        packet.writeByte(gateShape)
    }

    override def readDesc(packet:MCDataInput)
    {
        orientation = packet.readByte()
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
        gateLogicCanConnectTo(part, toInternal(r))

    override def canConnectCorner(r:Int) = false

    override def scheduledTick()
    {
        gateLogicOnScheduledTick()
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
        gateLogicOnChange()
    }

    override def tick()
    {
        if (!world.isClientSide) processScheduled()
        gateLogicOnTick()
    }

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isClientSide)
        {
            updateOutward()
            onChange()
        }
    }

    override def onNeighborBlockChanged(from:BlockPos)
    {
        if (!world.isClientSide)
        {
            if (dropIfCantStay()) return
            updateExternalConns()
            onChange()
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isClientSide)
        {
            gateLogicSetup()
            updateInward()
            onChange()
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isClientSide) notifyAllExternals()
    }

    override def onChunkLoad(chunk: Chunk)
    {
        super.onChunkLoad(chunk)
        if (tile != null) {
            gateLogicOnWorldLoad()
        }
    }

    def canStay = PRLib.canPlaceGateOnSide(world, pos.relative(Direction.values()(side)), Direction.values()(side^1))

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

    override def getType = gateType.getPartType

    override def getOcclusionShape:VoxelShape = GatePart.oShapes(side)
    override def getCollisionShape(context:ISelectionContext):VoxelShape = FaceMicroFactory.aShapes(0x10|side)
    override def getShape(context: ISelectionContext):VoxelShape = getCollisionShape(context)

    override def getStrength(player:PlayerEntity, hit:PartRayTraceResult) = 2/30f

    override def getSlotMask = 1<<side

    override def solid(side:Int) = false

    override def getLightValue = 7

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, held:ItemStack, hand:Hand):ActionResultType =
    {
        if (gateLogicActivate(player, held, hit)) return ActionResultType.SUCCESS

        if (!held.isEmpty && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held)) {
            if (!world.isClientSide) {
                if (player.isCrouching) configure()
                else rotate()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            return ActionResultType.SUCCESS
        }
        ActionResultType.PASS
    }

    def configure()
    {
        if (gateLogicCycleShape()) {
            updateInward()
            tile.setChanged()
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
        tile.setChanged()
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

    @OnlyIn(Dist.CLIENT)
    override def renderStatic(layer:RenderType, ccrs:CCRenderState) = {
        if (layer == null || (layer == RenderType.cutout() && Configurator.staticGates)) {
            ccrs.setBrightness(world, this.pos)
            RenderGate.renderStatic(this, Vector3.ZERO, ccrs)
            true
        } else
            false
    }

    @OnlyIn(Dist.CLIENT)
    override def renderDynamic(mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float)
    {
        val ccrs = CCRenderState.instance()
        ccrs.reset()
        ccrs.brightness = packedLight
        ccrs.overlay = packedOverlay
        ccrs.bind(new TransformingVertexBuilder(buffers.getBuffer(RenderType.cutout()), mStack), DefaultVertexFormats.BLOCK)
        RenderGate.renderDynamic(this, Vector3.ZERO, partialTicks, ccrs)
        RenderGate.renderCustomDynamic(this, Vector3.ZERO, mStack, buffers, packedLight, packedOverlay, partialTicks)
    }

    override def getBounds:Cuboid6 = new Cuboid6(getShape(ISelectionContext.empty()).bounds())

    override def getBreakingIcon(hit:PartRayTraceResult):TextureAtlasSprite = ComponentStore.baseIcon

    override def getBrokenIcon(side:Int):TextureAtlasSprite = ComponentStore.baseIcon

    override def getPlacementSound(context:ItemUseContext):SoundType = SoundType.GLASS

    def gateLogicCanConnectTo(part:IConnectable, r:Int):Boolean

    def gateLogicCycleShape():Boolean = false

    def gateLogicOnChange():Unit

    def gateLogicOnScheduledTick():Unit

    def gateLogicOnTick():Unit = {}

    def gateLogicSetup():Unit = {}
    def gateLogicOnWorldLoad():Unit = {}

    def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = false
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