/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.{CuboidRayTraceResult, IndexedCuboid6}
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec._
import codechicken.microblock.FaceMicroFactory
import codechicken.multipart._
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.{Configurator, PRLib, TFaceConnectable, TSwitchPacket}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{BlockRenderLayer, EnumFacing, EnumHand, ITickable}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.opengl.GL11

import scala.collection.JavaConversions._

abstract class GatePart extends TMultiPart with TCuboidPart with TNormalOcclusionPart with TFaceConnectable with TSwitchPacket with TIconHitEffectsPart with ITickable
{
    private var gateSubID:Byte = 0
    private var gateShape:Byte = 0

    var schedTime = 0L

    def getLogic[T]:T
    def getLogicPrimitive = getLogic[GateLogic[GatePart]]

    def subID = gateSubID&0xFF

    def shape = gateShape&0xFF
    def setShape(s:Int){ gateShape = s.toByte }

    def preparePlacement(player:EntityPlayer, pos:BlockPos, side:Int, meta:Int)
    {
        gateSubID = meta.toByte
        setSide(side^1)
        setRotation((Rotation.getSidedRotation(player, side)+2)%4)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("orient", orientation)
        tag.setByte("subID", gateSubID)
        tag.setByte("shape", gateShape)
        tag.setInteger("connMap", connMap)
        tag.setLong("schedTime", schedTime)
    }

    override def load(tag:NBTTagCompound)
    {
        orientation = tag.getByte("orient")
        gateSubID = tag.getByte("subID")
        gateShape = tag.getByte("shape")
        connMap = tag.getInteger("connMap")
        schedTime = tag.getLong("schedTime")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(orientation)
        packet.writeByte(gateSubID)
        packet.writeByte(gateShape)
    }

    override def readDesc(packet:MCDataInput)
    {
        orientation = packet.readByte()
        gateSubID = packet.readByte()
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
        if (schedTime < 0) schedTime = world.getTotalWorldTime+ticks
    }

    def processScheduled()
    {
        if (schedTime >= 0 && world.getTotalWorldTime >= schedTime)
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

    override def update()
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

    override def onNeighborChanged()
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
        if (getLogic == null) tile.remPart(this)
    }

    def canStay = PRLib.canPlaceGateOnSide(world, pos.offset(EnumFacing.getFront(side)), side^1)

    def dropIfCantStay() =
    {
        if (!canStay)
        {
            drop()
            true
        }
        else false
    }

    def drop()
    {
        TileMultipart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
        tile.remPart(this)
    }

    def getItem = getGateDef.makeStack

    override def getDrops = Seq(getItem)

    override def pickItem(hit:CuboidRayTraceResult) = getItem

    def getGateDef = GateDefinition(subID)

    override def getBounds = getLogicPrimitive.getBounds(this)

    override def getSubParts = Seq(new IndexedCuboid6(-1, getBounds))++getLogicPrimitive.getSubParts(this)

    override def getOcclusionBoxes = getLogicPrimitive.getOcclusions(this)

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult) = 2/30f

    override def getSlotMask = 1<<side

    override def solid(side:Int) = false

    override def getLightValue = getLogicPrimitive.lightLevel

    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, held:ItemStack, hand:EnumHand):Boolean =
    {
        if (getLogicPrimitive.activate(this, player, held, hit)) return true

        if (!held.isEmpty && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held)) {
            if (!world.isRemote) {
                if (player.isSneaking) configure()
                else rotate()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            return true
        }
        false
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
        getWriteStreamOf(2).writeByte(gateShape)
    }

    def sendOrientUpdate()
    {
        getWriteStreamOf(1).writeByte(orientation)
    }

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
    {
        if (layer == BlockRenderLayer.CUTOUT && Configurator.staticGates) {
            ccrs.setBrightness(world, this.pos)
            RenderGate.renderStatic(this, pos, ccrs)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, pass:Int, frame:Float)
    {
        if (pass == 0) {
            val ccrs = CCRenderState.instance()
            TextureUtils.bindBlockTexture()
            if (!Configurator.staticGates) {
                GL11.glDisable(GL11.GL_LIGHTING)
                ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
                RenderGate.renderStatic(this, pos, ccrs)
                ccrs.draw()
                GL11.glEnable(GL11.GL_LIGHTING)
            }
            RenderGate.renderDynamic(this, pos, frame, ccrs)
        }
    }

    @SideOnly(Side.CLIENT)
    override def getBreakingIcon(hit:CuboidRayTraceResult) = getBrokenIcon(hit.sideHit.ordinal)

    @SideOnly(Side.CLIENT)
    override def getBrokenIcon(side:Int) = ComponentStore.baseIcon
}

object GatePart
{
    var oBoxes = Array.ofDim[Cuboid6](6, 2)

    oBoxes(0)(0) = new Cuboid6(1 / 8D, 0, 0, 7 / 8D, 1 / 8D, 1)
    oBoxes(0)(1) = new Cuboid6(0, 0, 1 / 8D, 1, 1 / 8D, 7 / 8D)
    for (s <- 1 until 6)
    {
        val t = Rotation.sideRotations(s).at(Vector3.center)
        oBoxes(s)(0) = oBoxes(0)(0).copy.apply(t)
        oBoxes(s)(1) = oBoxes(0)(1).copy.apply(t)
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

    def activate(gate:T, player:EntityPlayer, held:ItemStack, hit:CuboidRayTraceResult) = false

    def getBounds(gate:T) = FaceMicroFactory.aBounds(0x10|gate.side)
    def getSubParts(gate:T) = Seq[IndexedCuboid6]()
    def getOcclusions(gate:T):Seq[Cuboid6] = GatePart.oBoxes(gate.side)

    def lightLevel = 7
}

trait TComplexGatePart extends GatePart
{
    def getLogicComplex = getLogic[TComplexGateLogic[TComplexGatePart]]

    def assertLogic()

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        getLogicComplex.save(tag)
    }

    abstract override def load(tag:NBTTagCompound)
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

    abstract override def preparePlacement(player:EntityPlayer, pos:BlockPos, side:Int, meta:Int)
    {
        super.preparePlacement(player, pos, side, meta)
        assertLogic()
    }
}

trait TComplexGateLogic[T <: TComplexGatePart] extends GateLogic[T]
{
    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    def readDesc(packet:MCDataInput){}
    def writeDesc(packet:MCDataOutput){}

    def read(packet:MCDataInput, key:Int){}
}
