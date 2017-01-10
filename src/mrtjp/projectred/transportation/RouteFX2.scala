/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.Vector3
import codechicken.multipart.BlockMultipart
import mrtjp.core.fx.ParticleAction._
import mrtjp.core.fx.particles.{BeamMulti, SpriteParticle}
import mrtjp.projectred.core.Configurator
import net.minecraft.client.Minecraft
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.BlockPos.MutableBlockPos
import net.minecraft.world.World
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{Set => MSet}

object RouteFX2
{
    val color_receive = EnumColour.ORANGE.ordinal
    val color_send = EnumColour.PURPLE.ordinal
    val color_relay = EnumColour.CYAN.ordinal
    val color_routeLost = EnumColour.MAGENTA.ordinal
    val color_route = EnumColour.RED.ordinal
    val color_sync = EnumColour.LIGHT_BLUE.ordinal
    val color_request = EnumColour.PINK.ordinal
    val color_checkInv = EnumColour.WHITE.ordinal
    val color_linked = EnumColour.LIME.ordinal
    val color_unlinked = EnumColour.RED.ordinal
    val color_blink = EnumColour.LIGHT_GRAY.ordinal

    def isFXDisabled = !Configurator.pipeRoutingFX

    //type 1 - expanding bubble
    def spawnType1(colour:Int, pipe:TNetworkPipe)
    {
        if (!pipe.world.isRemote) sendPacket(pipe.world, pipe.posOfInternal, 1, colour, -1)
        else spawnType1_do(colour, pipe)
    }

    @SideOnly(Side.CLIENT)
    private def spawnType1_do(colour:Int, pipe:TNetworkPipe)
    {
        if (isFXDisabled) return

        val c1 = EnumColour.BLACK
        val c2 = EnumColour.values()(colour)

        val particle = new SpriteParticle(pipe.world)
        Minecraft.getMinecraft.effectRenderer.addEffect(particle)
        particle.setPos(Vector3.fromBlockPosCenter(pipe.posOfInternal))
        particle.setMaxAge(15)
        particle.texture = "projectred:textures/particles/large_colourless_bubble.png"
        particle.alpha = 0
        particle.setRGB(c1.rF, c1.gF, c1.bF)
        particle.scale = Vector3.zero.copy

        val act = sequence(
            group(
                scaleFor(0.06, 0.06, 0.06, 5),
                fadeIn(5),
                changeColourTo(c2.rF, c2.gF, c2.bF, 5)
            ),
            delay(5),
            group(
                scaleFor(0.05, 0.05, 0.05, 5),
                fadeOut(5),
                changeColourTo(c1.rF, c1.gF, c1.bF, 5)
            ),
            kill()
        )
        particle.runAction(act)
    }

    //type 2 - 1 meter exit beam
    def spawnType2(colour:Int, dir:Int, pipe:TNetworkPipe)
    {
        if (!pipe.world.isRemote) sendPacket(pipe.world, pipe.posOfInternal, 2, colour, dir)
        else spawnType2_do(colour, dir, pipe)
    }

    @SideOnly(Side.CLIENT)
    private def spawnType2_do(colour:Int, dir:Int, pipe:TNetworkPipe)
    {
        if (isFXDisabled) return

        val c1 = EnumColour.BLACK
        val c2 = EnumColour.values()(colour)
        val beam = new BeamMulti(pipe.world)
        Minecraft.getMinecraft.effectRenderer.addEffect(beam)
        beam.setMaxAge(15)
        beam.texture = "projectred:textures/particles/beam4.png"
        beam.points = Seq(
            Vector3.fromBlockPosCenter(pipe.posOfInternal),
            Vector3.fromBlockPosCenter(pipe.posOfStraight(dir))
        )
        beam.alpha = 0
        beam.setRGB(c1.rF, c1.gF, c1.bF)

        val act = sequence(
            group(
                changeColourTo(c2.rF, c2.gF, c2.bF, 5),
                fadeIn(5)
            ),
            delay(5),
            group(
                changeColourTo(c1.rF, c1.gF, c1.bF, 5),
                fadeOut(5)
            ),
            kill()
        )
        beam.runAction(act)
    }

    //type 3 - router to router path finding beam
    def spawnType3(colour:Int, dir:Int, pipe:TNetworkPipe)
    {
        if (!pipe.world.isRemote) sendPacket(pipe.world, pipe.posOfInternal, 3, colour, dir)
        else spawnType3_do(colour, dir, pipe)
    }

    @SideOnly(Side.CLIENT)
    private def spawnType3_do(colour:Int, dir:Int, pipe:TNetworkPipe)
    {
        if (isFXDisabled) return

        val paths = BeamPathFinder.findPaths(pipe, dir)
        val c1 = EnumColour.BLACK
        val c2 = EnumColour.values()(colour)

        import mrtjp.core.fx.ParticleAction._
        val act = sequence(
            group(
                changeColourTo(c2.rF, c2.gF, c2.bF, 5),
                fadeIn(2.5)
            ),
            delay(5),
            group(
                changeColourTo(c1.rF, c1.gF, c1.bF, 5),
                fadeOut(2.5)
            ),
            kill()
        )

        for (path <- paths) if (path.size > 1) {
            val beam = new BeamMulti(pipe.world)
            Minecraft.getMinecraft.effectRenderer.addEffect(beam)
            beam.setMaxAge(20)
            beam.texture = "projectred:textures/particles/beam4.png"
            beam.points = path
            beam.alpha = 0
            beam.setRGB(c1.rF, c1.gF, c1.bF)
            beam.runAction(act)
        }
    }

    def sendPacket(w:World, pos:BlockPos, id:Int, colour:Int, dir:Int)
    {
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.particle_Spawn)
        packet.writeByte(id)
        packet.writeByte(colour).writePos(pos)
        if (dir != -1) packet.writeByte(dir)
        packet.sendPacketToAllAround(pos, 64, w.provider.getDimension)
    }

    def handleClientPacket(in:MCDataInput, w:World)
    {
        val id = in.readUByte()
        val colour = in.readUByte()
        BlockMultipart.getPart(w, in.readPos(), 6) match {
            case pipe:TNetworkPipe => id match {
                case 1 => spawnType1_do(colour, pipe.asInstanceOf[TNetworkPipe])
                case 2 => spawnType2_do(colour, in.readUByte(), pipe.asInstanceOf[TNetworkPipe])
                case 3 => spawnType3_do(colour, in.readUByte(), pipe.asInstanceOf[TNetworkPipe])
                case _ =>
            }
            case _ =>
        }
    }
}

object BeamPathFinder
{
    private var pipe:TNetworkPipe = null
    private val paths = MSet[Seq[Int]]()

    def findPaths(p:TNetworkPipe, dir:Int):Set[Seq[Vector3]] =
    {
        pipe = p

        val pos = pipe.pos
        val q = Queue.newBuilder[Node]
        q += Node(pos, dir)
        iterate(Queue(Node(pos, dir)), Set(Node(pos)))

        val result = paths.toSet
        pipe = null
        paths.clear()
        result.map(traceAndVectorize(pos, _))
    }

    @tailrec
    private def iterate(open:Seq[Node], closed:Set[Node] = Set.empty):Unit = open match
    {
        case Seq() =>
        case Seq(next, rest@_*) => getMultiPart(next.pos) match
        {
            case iwr:IWorldRouter with TNetworkPipe if !iwr.needsWork =>
                if (!closed.exists(_.pos == next.pos)) paths += next.path
                iterate(rest, closed+next)
            case p:TNetworkSubsystem =>
                val upNext = Seq.newBuilder[Node]
                for (s <- 0 until 6) if (s != (next.dir^1) && p.maskConnects(s))
                {
                    val route = next --> (s, p.getPathWeight)
                    if (!closed(route) && !open.contains(route)) upNext += route
                }
                iterate(rest++upNext.result(), closed+next)
            case _ => iterate(rest, closed+next)
        }
    }

    private def getMultiPart(pos:BlockPos) = BlockMultipart.getPart(pipe.world, pos, 6)

    private def traceAndVectorize(start:BlockPos, path:Seq[Int]):Seq[Vector3] =
    {
        val newList = Seq.newBuilder[BlockPos]
        val iterator = path.iterator

        var prev = -1
        var pos = new MutableBlockPos(start)

        while(iterator.hasNext)
        {
            val dir = iterator.next()
            if (dir == prev) pos.move(EnumFacing.values()(dir))
            else {
                newList += pos.toImmutable
                pos.move(EnumFacing.values()(dir))
                prev = dir
            }
        }

        newList += pos
        newList.result().map(Vector3.fromBlockPosCenter)
    }
}

private object Node
{
    def apply(pos:BlockPos):Node = new Node(pos, 0, 6)
    def apply(pos:BlockPos, dir:Int):Node = new Node(pos.offset(EnumFacing.values()(dir)), 1, dir, Seq(dir))
}
private class Node(val pos:BlockPos, val dist:Int, val dir:Int, val path:Seq[Int] = Seq.empty) extends Ordered[Node]
{
    def -->(toDir:Int, distAway:Int):Node =
    {
        val bc2 = pos.offset(EnumFacing.values()(toDir))
        new Node(bc2, dist+distAway, toDir, path :+ toDir)
    }
    def -->(toDir:Int):Node = this -->(toDir, 1)

    override def compare(that:Node) = dist-that.dist

    override def equals(other:Any) = other match
    {
        case that:Node =>
            pos == that.pos && dir == that.dir
        case _ => false
    }

    override def hashCode = pos.hashCode

    override def toString = "@"+pos.toString+": delta("+dir+")"
}