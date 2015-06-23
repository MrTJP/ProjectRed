/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{BlockCoord, Rotation, Vector3}
import codechicken.multipart._
import mrtjp.core.world.PlacementLib
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.{TFacePowerPart, TFaceConnectable, TSwitchPacket}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

import scala.collection.JavaConversions._

trait TFaceElectricalDevice extends TMultiPart with TCuboidPart with TNormalOcclusion with TFaceConnectable with TSwitchPacket with TIconHitEffects with TFacePowerPart
{
    def preparePlacement(player:EntityPlayer, pos:BlockCoord, side:Int, meta:Int)
    {
        setSide(side^1)
        setRotation((Rotation.getSidedRotation(player, side)+2)%4)
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("orient", orientation)
        tag.setInteger("connMap", connMap)
    }

    override def load(tag:NBTTagCompound)
    {
        orientation = tag.getByte("orient")
        connMap = if (tag.getBoolean("nolegacy")) tag.getInteger("connMap") else tag.getShort("connMap")|0xF000
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(orientation)
    }

    override def readDesc(packet:MCDataInput)
    {
        orientation = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            orientation = packet.readByte()
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendOrientUpdate()
    {
        getWriteStreamOf(1).writeByte(orientation)
    }

    override def setRenderFlag(part:IConnectable) = false

    override def discoverOpen(dir:Int) = true

    override def canConnectCorner(r:Int) = false

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote)
        {
            updateOutward()
        }
    }

    override def onNeighborChanged()
    {
        if (!world.isRemote)
        {
            if (dropIfCantStay()) return
            updateExternalConns()
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote)
        {
            updateInward()
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) notifyAllExternals()
    }

    def canStay =
    {
        val pos = new BlockCoord(tile).offset(side)
        PlacementLib.canPlaceGateOnSide(world, pos.x, pos.y, pos.z, side^1)
    }

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
        TileMultipart.dropItem(getItem, world, Vector3.fromTileEntityCenter(tile))
        tile.remPart(this)
    }

    def getItem:ItemStack

    override def getDrops = Seq(getItem)

    override def pickItem(hit:MovingObjectPosition) = getItem

    override def getSlotMask = 1<<side

    override def solid(side:Int) = false

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack):Boolean =
    {
        if (held != null && doesRotate && held.getItem.isInstanceOf[IScrewdriver] && held.getItem.asInstanceOf[IScrewdriver].canUse(player, held))
        {
            if (!world.isRemote)
            {
                rotate()
                held.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, held)
            }
            return true
        }
        false
    }

    def rotate()
    {
        setRotation((rotation+1)%4)
        updateInward()
        tile.markDirty()
        tile.notifyPartChange(this)
        sendOrientUpdate()
        notifyExternals(0xF)
    }

    def doesRotate = true
}