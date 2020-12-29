/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.io.{ByteArrayOutputStream, DataOutputStream}
import java.util.{LinkedList => JLinkedList}

import codechicken.lib.data.MCDataOutputWrapper
import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.ProjectRedRelocation
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.{EntityPlayer, EntityPlayerMP}
import net.minecraft.network.play.server.SPacketDisconnect
import net.minecraft.network.play.{INetHandlerPlayClient, INetHandlerPlayServer}
import net.minecraft.util.math.ChunkPos
import net.minecraft.util.text.TextComponentString
import net.minecraft.world.World
import net.minecraftforge.fml.common.FMLCommonHandler

import scala.collection.JavaConversions._
import scala.collection.mutable.{HashMap => MHashMap, Map => MMap, MultiMap => MMultiMap, Set => MSet}

/**
  * Tweaked version of Chickenbones' compressed end-of-tick tile data stream.
  */
class RelocationPH
{
    val channel = "PR|Reloc"
}

object RelocationCPH extends RelocationPH with IClientPacketHandler
{
    def handlePacket(packet:PacketCustom, mc:Minecraft, netHandler:INetHandlerPlayClient)
    {
        try {
            packet.getType match {
                case 1 => handleChunkDesc(packet, mc.world)
                case 2 => handleChunkData(packet, mc.world)
            }
        }
        catch {
            case e:RuntimeException if e.getMessage.startsWith("DC: ") =>
                netHandler.handleDisconnect(new SPacketDisconnect(new TextComponentString(e.getMessage.substring(4))))
        }
    }

    def handleChunkData(packet:PacketCustom, world:World)
    {
        var i = packet.readUByte()
        while (i != 255) {
            MovementManager.read(world, packet, i)
            i = packet.readUByte()
        }
    }

    def handleChunkDesc(packet:PacketCustom, world:World)
    {
        MovementManager.readDesc(world, packet)
    }
}

object RelocationSPH extends RelocationPH with IServerPacketHandler
{

    @Deprecated
    class MCByteStream(bout:ByteArrayOutputStream) extends codechicken.lib.data.MCByteStream(bout)
    {
        override def getBytes = bout.toByteArray
    }

    override def handlePacket(packetCustom:PacketCustom, entityPlayerMP:EntityPlayerMP, iNetHandlerPlayServer:INetHandlerPlayServer){}

    private val updateMap = MMap[World, MMap[Set[ChunkPos], MCByteStream]]()
    private val chunkWatchers = new MHashMap[Int, MSet[ChunkPos]] with MMultiMap[Int, ChunkPos]
    private val newWatchers = MMap[Int, JLinkedList[ChunkPos]]()

    def onTickEnd()
    {
        val players = getServerPlayers
        sendData(players)
        sendDesc(players)
    }

    def onWorldUnload(world:World)
    {
        if (!world.isRemote) {
            updateMap.remove(world)
            if (chunkWatchers.nonEmpty) {
                val players = getServerPlayers
                for (p <- players) if (p.world.provider.getDimension == world.provider.getDimension)
                    chunkWatchers.remove(p.getEntityId)
            }
        }
    }

    def onChunkWatch(p:EntityPlayer, c:ChunkPos)
    {
        newWatchers.getOrElseUpdate(p.getEntityId, new JLinkedList).add(c)
    }

    def onChunkUnWatch(p:EntityPlayer, c:ChunkPos)
    {
        newWatchers.get(p.getEntityId) match {
            case Some(chunks) => chunks.remove(c)
            case _ =>
        }
        chunkWatchers.removeBinding(p.getEntityId, c)
    }

    private def getServerPlayers:Seq[EntityPlayerMP] =
        FMLCommonHandler.instance().getMinecraftServerInstance.getPlayerList.getPlayers

    private def sendData(players:Seq[EntityPlayerMP])
    {
        for (p <- players if chunkWatchers.containsKey(p.getEntityId)) {
            updateMap.get(p.world) match {
                case Some(m) if m.nonEmpty =>
                    val chunks = chunkWatchers(p.getEntityId)
                    val packet = new PacketCustom(channel, 2).compress()
                    var send = false
                    for ((uchunks, stream) <- m if uchunks.exists(chunks.contains)) {
                        send = true
                        packet.writeBytes(stream.getBytes)
                        packet.writeByte(255) //terminator
                    }
                    if (send) packet.sendToPlayer(p)
                case _ =>
            }
        }
        updateMap.foreach(_._2.clear())
    }

    private def sendDesc(players:Seq[EntityPlayerMP])
    {
        for (p <- players if newWatchers.containsKey(p.getEntityId)) {
            val watched = newWatchers(p.getEntityId)
            val pkt = getDescPacket(p.world, watched.toSet)
            if (pkt != null) pkt.sendToPlayer(p)
            for (c <- watched)
                chunkWatchers.addBinding(p.getEntityId, c)
        }
        newWatchers.clear()
    }

    private def getDescPacket(world:World, chunks:Set[ChunkPos]):PacketCustom =
    {
        val packet = new PacketCustom(channel, 1)
        if (MovementManager.writeDesc(world, chunks, packet)) packet else null
    }

    def forceSendData()
    {
        sendData(getServerPlayers)
    }

    def forceSendDesc()
    {
        sendDesc(getServerPlayers)
    }

    def getStream(world:World, chunks:Set[ChunkPos], key:Int) =
    {
        updateMap.getOrElseUpdate(world, {
                if (world.isRemote)
                    throw new IllegalArgumentException("Cannot use RelocationSPH on a client world")
                MMap()
            }).getOrElseUpdate(chunks, {
                val s = new MCByteStream(new ByteArrayOutputStream)
                s.writeByte(key)
                s
            })
    }
}
