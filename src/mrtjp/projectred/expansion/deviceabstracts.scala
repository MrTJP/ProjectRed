/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.transportation.{PipePayload, TPressureDevice, TPressureTube}
import net.minecraft.block.Block
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}

import scala.collection.mutable.ListBuffer

class ItemStorage
{
    private val storage = ListBuffer[PipePayload]()
    var backlogged = false

    def isEmpty = storage.isEmpty

    def add(item:PipePayload){ storage.prepend(item) }

    def add(item:ItemStack){ add(PipePayload(item)) }

    def addBacklog(item:PipePayload){ storage.append(item); backlogged = true }

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
                val r = PipePayload.create()
                r.load(payloadData)
                if (!r.isCorrupted) add(r)
            }
            catch {case t:Throwable =>}
        }
    }
}

trait TActiveDevice extends TileMachine with TPressureDevice
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
        writeStream(4).writeBoolean(powered).writeBoolean(active).sendToChunk()
    }

    override def acceptItem(item:PipePayload, side:Int):Boolean =
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
                sendStateUpdate()
            }
        }
    }

    override def onNeighborChange(b:Block)
    {
        if (world.isBlockIndirectlyGettingPowered(x, y, z))
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

    def exportPipe(r:PipePayload) =
    {
        PRLib.getMultiPart(world, position.offset(side), 6) match
        {
            case pipe:TPressureTube if pipe.hasDestination(r, side^1) =>
                pipe.injectPayload(r, side)
                true
            case _ => false
        }
    }

    def exportInv(r:PipePayload) = false

    def exportEject(r:PipePayload):Boolean =
    {
        val pos = position.offset(side)
        if (world.blockExists(pos.x, pos.y, pos.z) &&
                !world.isAirBlock(pos.x, pos.y, pos.z)) return false

        WorldLib.centerEject(world, position, r.payload.makeStack, side, 0.25D)
        true
    }
}