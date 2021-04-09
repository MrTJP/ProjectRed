/*
/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import java.util.Random

import mrtjp.core.handler.MrTJPConfig
import mrtjp.core.handler.MrTJPCoreMod.log
import net.minecraft.nbt.{NBTTagCompound, NBTTagList, NBTTagString}
import net.minecraft.world.World
import net.minecraft.world.chunk.IChunkProvider
import net.minecraft.world.gen.IChunkGenerator
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.common.util.Constants
import net.minecraftforge.event.world.ChunkDataEvent
import net.minecraftforge.fml.common.IWorldGenerator
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.TickEvent.{Phase, WorldTickEvent}
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.Side

import scala.collection.mutable.{Queue => MQueue}

trait ISimpleStructureGen
{
    def genID:String

    def generate(w:World, chunkX:Int, chunkZ:Int, rand:Random, isRetro:Boolean):Boolean
}

object SimpleGenHandler extends IWorldGenerator
{
    private var structures = Seq[ISimpleStructureGen]()
    private var structHash = 0L

    private var genQueue = Map[Int, MQueue[ChunkCoord]]().withDefaultValue(MQueue[ChunkCoord]())

    private val retroGen = MrTJPConfig.retro_gen
    private val tagDB = "RetrogenData_"+MrTJPConfig.retro_gen_id

    def init()
    {
        GameRegistry.registerWorldGenerator(this, 0)
        MinecraftForge.EVENT_BUS.register(this)
        MinecraftForge.ORE_GEN_BUS.register(this)

        if (retroGen) MinecraftForge.EVENT_BUS.register(this)
    }

    def registerStructure(struct:ISimpleStructureGen)
    {
        if (structures.exists(_.genID == struct.genID))
            log.error("MrTJP Structure gen duplicate structure '%s'", struct.genID)
        else
        {
            structures :+= struct
            structHash += struct.genID.hashCode
        }
    }

    @SubscribeEvent
    def chunkSaveEvent(event:ChunkDataEvent.Save)
    {
        val genNBT = event.getData.getCompoundTag(tagDB)

        val structList = new NBTTagList
        for (g <- structures) structList.appendTag(new NBTTagString(g.genID))

        genNBT.setTag("StructList", structList)
        genNBT.setLong("StructHash", structHash)
        event.getData.setTag(tagDB, genNBT)
    }

    @SubscribeEvent
    def chunkLoadEvent(event:ChunkDataEvent.Load)
    {
        if (retroGen)
        {
            val dim = event.getWorld.provider.getDimension
            val tag = event.getData.getTag(tagDB).asInstanceOf[NBTTagCompound]
            val list = if (tag == null) new NBTTagList else tag.getTagList("StructList", Constants.NBT.TAG_STRING)

            if (tag == null || tag.getLong("StructHash") != structHash || list.tagCount != structures.size)
            {
                val chunk = ChunkCoord(event.getChunk.x, event.getChunk.z, list)
                val chunks = genQueue.getOrElse(dim, MQueue[ChunkCoord]())
                chunks.enqueue(chunk)
                genQueue += dim -> chunks
            }
        }
    }

    override def generate(rand:Random, chunkX:Int, chunkZ:Int, world:World, g:IChunkGenerator, p:IChunkProvider)
    {
        subGenerate(world, chunkX, chunkZ, rand, false)
    }

    private def subGenerate(w:World, chunkX:Int, chunkZ:Int, rand:Random, isRetro:Boolean, existingStructs:Set[String] = Set.empty)
    {
        var gen = false
        for (s <- structures) if (!existingStructs.contains(s.genID))
            gen |= s.generate(w, chunkX, chunkZ, rand, isRetro)

        if (isRetro && gen) w.getChunkFromChunkCoords(chunkX, chunkZ).setModified(true)
    }

    @SubscribeEvent
    def tickEnd(event:WorldTickEvent)
    {
        if (event.side != Side.SERVER || event.phase != Phase.END) return

        val world = event.world
        val dim = world.provider.getDimension

        if (world.getWorldTime%10 != 0) return //Don't gen too quickly
        if (!genQueue.contains(dim)) return

        val chunks = genQueue(dim)
        if (chunks.nonEmpty)
        {
            val chunk = chunks.dequeue()

            val worldSeed = world.getSeed
            val rand = new Random(worldSeed)
            val xSeed = rand.nextLong>>2+1L
            val zSeed = rand.nextLong>>2+1L
            rand.setSeed(xSeed*chunk.chunkX+zSeed*chunk.chunkZ^worldSeed)

            log.warn("Starting Retrogeneration on chunk @[DIM "+dim+"]"+chunk.toString)
            subGenerate(world, chunk.chunkX, chunk.chunkZ, rand, true, chunk.structList)
        }

        if (chunks.isEmpty) genQueue -= dim
    }

    private case class ChunkCoord(chunkX:Int, chunkZ:Int, tag:NBTTagList)
    {
        val structList =
        {
            val builder = Set.newBuilder[String]
            if (tag != null) for (i <- 0 until tag.tagCount())
                builder += tag.getStringTagAt(i)
            builder.result()
        }

        override def toString = "["+chunkX+", "+chunkZ+"]"
    }
}

*/
