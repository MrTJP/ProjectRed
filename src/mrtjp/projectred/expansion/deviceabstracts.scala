/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.multipart.BlockMultipart
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.PRLib
import mrtjp.projectred.transportation._
import net.minecraft.block.Block
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}
import net.minecraft.util.EnumFacing

import scala.collection.mutable.ListBuffer

class ItemStorage
{
    private val storage = ListBuffer[PressurePayload]()
    var backlogged = false

    def isEmpty = storage.isEmpty

    def add(item:PressurePayload){ storage.prepend(item) }

    def add(item:ItemStack)
    {
        val p = new PressurePayload(AbstractPipePayload.claimID())
        p.setItemStack(item)
        add(p)
    }

    def addBacklog(item:PressurePayload){ storage.append(item); backlogged = true }

    def poll() =
    {
        val item = storage.remove(storage.size-1)
        if (storage.size == 0) backlogged = false
        item
    }

    def peek = storage(storage.size-1)

    def save(tag:NBTTagCompound)
    {
        val nbttaglist = new NBTTagList
        for (r <- storage)
        {
            val payloadData = new NBTTagCompound
            nbttaglist.appendTag(payloadData)
            r.save(payloadData)
        }
        tag.setTag("itemFlow", nbttaglist)
    }

    def load(tag:NBTTagCompound)
    {
        val nbttaglist = tag.getTagList("itemFlow", 0)
        for (j <- 0 until nbttaglist.tagCount)
        {
            try
            {
                val payloadData = nbttaglist.getCompoundTagAt(j)
                val r = new PressurePayload(AbstractPipePayload.claimID())
                r.load(payloadData)
                if (!r.isCorrupted) add(r)
            }
            catch {case t:Throwable =>}
        }
    }
}

trait TActiveDevice extends TileMachine
{
    val storage = new ItemStorage
    var powered = false
    var active = false

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("pow", powered)
        tag.setBoolean("act", active)
        storage.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        powered = tag.getBoolean("pow")
        active = tag.getBoolean("act")
        storage.load(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(powered).writeBoolean(active)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        powered = in.readBoolean()
        active = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 4 =>
            powered = in.readBoolean()
            active = in.readBoolean()
            markRender()
        case _ => super.read(in, key)
    }

    def sendStateUpdate()
    {
        writeStream(4).writeBoolean(powered).writeBoolean(active).sendToChunk(this)
    }

    def shouldAcceptBacklog = true
    def shouldAcceptInput = !powered && storage.isEmpty

    override def onScheduledTick()
    {
        if (!world.isRemote)
        {
            if (!storage.isEmpty)
            {
                exportBuffer()
                scheduleTick(if (storage.isEmpty) 4 else 16)
            }
            else if (!powered)
            {
                active = false
                onDeactivate()
                sendStateUpdate()
            }
        }
    }

    override def onNeighborBlockChange()
    {
        if (world.isBlockPowered(getPos))
        {
            if (powered) return
            powered = true
            markDirty()
            if (active) return
            active = true
            onActivate()
            sendStateUpdate()
        }
        else
        {
            if (active && !isTickScheduled) scheduleTick(4)
            powered = false
            markDirty()
        }
    }

    def onActivate()
    def onDeactivate(){}

    def exportBuffer()
    {
        while (!storage.isEmpty)
        {
            val r = storage.peek
            if (exportPipe(r) || exportInv(r) || exportEject(r)) storage.poll()
            else storage.backlogged = true

            if (storage.backlogged) return
        }
    }

    def exportPipe(r:PressurePayload) =
    {
        BlockMultipart.getPart(world, getPos.offset(EnumFacing.VALUES(side)), 6) match
        {
            case pipe:TPressureTube if pipe.hasDestination(r, side^1) =>
                pipe.injectPayload(r, side)
                true
            case _ => false
        }
    }

    def exportInv(r:PressurePayload) =
    {
        val inv = InvWrapper.getInventory(world, getPos.offset(EnumFacing.VALUES(side)))
        if (inv != null)
        {
            val w = InvWrapper.wrap(inv).setSlotsFromSide(side^1)
            r.payload.stackSize -= w.injectItem(r.payload.key, r.payload.stackSize)
            r.payload.stackSize <= 0
        }
        else false
    }

    def exportEject(r:PressurePayload):Boolean =
    {
        val pos = getPos.offset(EnumFacing.VALUES(side))
        if (world.isBlockLoaded(pos) &&
                !world.isAirBlock(pos)) return false

        WorldLib.centerEject(world, getPos, r.payload.makeStack, side, 0.25D)
        true
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        while(!storage.isEmpty)
            WorldLib.dropItem(world, getPos, storage.poll().payload.makeStack)
    }
}

trait TPressureActiveDevice extends TActiveDevice with TPressureDevice
{
    override def acceptItem(item:PressurePayload, side:Int):Boolean =
    {
        if (!canConnectSide(side)) return false

        if (canAcceptInput(item.payload.key, side) && shouldAcceptInput)
        {
            storage.add(item)
            active = true
            sendStateUpdate()
            scheduleTick(4)
            exportBuffer()
            true
        }
        else if (canAcceptBacklog(item.payload.key, side) && shouldAcceptBacklog)
        {
            storage.addBacklog(item)
            active = true
            sendStateUpdate()
            scheduleTick(4)
            true
        }
        else false
    }
}
