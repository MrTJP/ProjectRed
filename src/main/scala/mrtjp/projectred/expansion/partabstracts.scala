package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Rotation, Vector3}
import codechicken.multipart.api.part.{TIconHitEffectsPart, TMultiPart, TNormalOcclusionPart}
import codechicken.multipart.block.TileMultiPart
import codechicken.multipart.util.PartRayTraceResult
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.{IPowerConnectable, PRLib, TFaceConnectable, TFacePowerPart, TSwitchPacket}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{ActionResultType, Direction, Hand}

import java.util
import scala.jdk.CollectionConverters._

trait TFaceElectricalDevice extends TMultiPart with TNormalOcclusionPart with TFaceConnectable with TSwitchPacket with TIconHitEffectsPart with TFacePowerPart
{
    def preparePlacement(player:PlayerEntity, pos:BlockPos, side:Int)
    {
        setSide(side^1)
        setRotation((Rotation.getSidedRotation(player, side)+2)%4)
    }

    override def save(tag:CompoundNBT):Unit = {
        tag.putByte("orient", orientation)
        tag.putInt("connMap", connMap)
    }

    override def load(tag:CompoundNBT):Unit = {
        orientation = tag.getByte("orient")
        connMap = tag.getInt("connMap")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        packet.writeByte(orientation)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        orientation = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 1 =>
            orientation = packet.readByte()
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendOrientUpdate():Unit = {
        sendUpdate(1, _.writeByte(orientation))
    }

    override def setRenderFlag(part:IConnectable):Boolean = false

    override def discoverOpen(dir:Int):Boolean = true

    override def discoverStraightOverride(absDir:Int):Boolean = world.getTileEntity(posOfStraight(absoluteRot(absDir))) match {
        case p:IPowerConnectable => p.connectStraight(this, absDir^1, Rotation.rotationTo(absDir, side))
        case _ => false
    }

    override def canConnectCorner(r:Int):Boolean = false

    override def onPartChanged(part:TMultiPart):Unit = {
        if (!world.isRemote)
            if (updateOutward())
                onMaskChanged()
    }

    override def onNeighborBlockChanged(from:BlockPos):Unit = {
        if (!world.isRemote) {
            if (!dropIfCantStay())
                if (updateExternalConns())
                    onMaskChanged()
        }
    }

    override def onAdded():Unit = {
        super.onAdded()
        if (!world.isRemote)
            if (updateInward())
                onMaskChanged()
    }

    override def onRemoved():Unit = {
        super.onRemoved()
        if (!world.isRemote) notifyAllExternals()
    }

    def canStay:Boolean = {
        val pos = tile.getPos.offset(Direction.byIndex(side))
        PRLib.canPlaceGateOnSide(world, pos, Direction.byIndex(side^1))
    }

    def dropIfCantStay():Boolean = {
        if (!canStay)  {
            drop()
            true
        } else false
    }

    def drop():Unit = {
        TileMultiPart.dropItem(getItem, world, Vector3.fromTileCenter(tile))
        tile.remPart(this)
    }

    def getItem:ItemStack

    override def getDrops:util.List[ItemStack] = Seq(getItem).asJava

    override def pickItem(hit:PartRayTraceResult):ItemStack = getItem

    override def getSlotMask:Int = 1<<side

    override def solid(side:Int):Boolean = false

    override def activate(player:PlayerEntity, hit:PartRayTraceResult, held:ItemStack, hand:Hand):ActionResultType = {
        if (!held.isEmpty && doesRotate && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held)) {
            if (!world.isRemote) {
                rotate()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            ActionResultType.SUCCESS
        } else
            ActionResultType.PASS
    }

    def rotate()
    {
        setRotation((rotation+1)%4)
        if (updateInward())
            onMaskChanged()
        tile.markDirty()
        tile.notifyPartChange(this)
        sendOrientUpdate()
        notifyExternals(0xF)
    }

    def doesRotate = true
}
