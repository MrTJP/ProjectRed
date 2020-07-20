/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.lib.vec.{BlockCoord, Vector3}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.fx.FXEngine
import mrtjp.core.fx.ParticleAction._
import mrtjp.core.fx.particles.{BeamMulti, SpriteParticle}
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.libmc.PRLib
import net.minecraft.world.World

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.collection.mutable.{Set => MSet}

object RouteFX2
{
    val color_receive = Colors.ORANGE.ordinal
    val color_send = Colors.PURPLE.ordinal
    val color_relay = Colors.CYAN.ordinal
    val color_routeLost = Colors.MAGENTA.ordinal
    val color_route = Colors.RED.ordinal
    val color_sync = Colors.LIGHT_BLUE.ordinal
    val color_request = Colors.PINK.ordinal
    val color_checkInv = Colors.WHITE.ordinal
    val color_linked = Colors.LIME.ordinal
    val color_unlinked = Colors.RED.ordinal
    val color_blink = Colors.LIGHT_GREY.ordinal

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

        val c1 = Colors.BLACK
        val c2 = Colors(colour)

        val particle = new SpriteParticle(pipe.world)
        FXEngine.addEffect(particle)
        particle.setPos(pipe.posOfInternal.toVector3Centered)
        particle.setMaxAge(15)
        particle.texture = "projectred:textures/particles/large_colourless_bubble.png"
        particle.alpha = 0
        particle.setRGB(c1.rF, c1.gF, c1.bF)
        particle.scale = Vector3.zero.copy

        val act = sequence(
            group(
                scaleFor(0.085, 0.085, 0.085, 5),
                fadeIn(2.5),
                changeColourTo(c2.rF, c2.gF, c2.bF, 5)
            ),
            delay(5),
            group(
                scaleFor(0.085, 0.085, 0.085, 5),
                fadeOut(2.5),
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

        val c1 = Colors.BLACK
        val c2 = Colors(colour)
        val beam = new BeamMulti(pipe.world)
        FXEngine.addEffect(beam)
        beam.setMaxAge(15)
        beam.texture = "projectred:textures/particles/beam4.png"
        beam.points = Seq(
            pipe.posOfInternal.toVector3Centered,
            pipe.posOfInternal.offset(dir).toVector3Centered
        )
        beam.alpha = 0
        beam.setRGB(c1.rF, c1.gF, c1.bF)

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
        val c1 = Colors.BLACK
        val c2 = Colors(colour)

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

        for (path <- paths) if (path.size > 1)
        {
            val beam = new BeamMulti(pipe.world)
            FXEngine.addEffect(beam)
            beam.setMaxAge(20)
            beam.texture = "projectred:textures/particles/beam4.png"
            beam.points = path
            beam.alpha = 0
            beam.setRGB(c1.rF, c1.gF, c1.bF)
            beam.runAction(act)
        }
    }

    def sendPacket(w:World, bc:BlockCoord, id:Int, colour:Int, dir:Int)
    {
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.particle_Spawn)
        packet.writeByte(id)
        packet.writeByte(colour).writeCoord(bc)
        if (dir != -1) packet.writeByte(dir)
        packet.sendPacketToAllAround(bc.x, bc.y, bc.z, 64, w.provider.dimensionId)
    }

    def handleClientPacket(in:MCDataInput, w:World)
    {
        val id = in.readUByte()
        val colour = in.readUByte()
        PRLib.getMultiPart(w, in.readCoord(), 6) match
        {
            case pipe:TNetworkPipe => id match
            {
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

        val bc = new BlockCoord(pipe.tile)

        val q = Queue.newBuilder[Node]
        q += Node(bc, dir)
        iterate(Queue(Node(bc, dir)), Set(Node(bc)))

        val result = paths.toSet
        pipe = null
        paths.clear()
        result.map(traceAndVectorize(bc, _))
    }

    @tailrec
    private def iterate(open:Seq[Node], closed:Set[Node] = Set.empty):Unit = open match
    {
        case Seq() =>
        case Seq(next, rest@_*) => getMultiPart(next.bc) match
        {
            case iwr:IRouterContainer with TNetworkPipe =>
                if (!closed.exists(_.bc == next.bc)) paths += next.path
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

    private def getMultiPart(bc:BlockCoord) = PRLib.getMultiPart(pipe.world, bc, 6)

    private def traceAndVectorize(start:BlockCoord, path:Seq[Int]):Seq[Vector3] =
    {
        val newList = Seq.newBuilder[BlockCoord]
        val iterator = path.iterator

        var prev = -1
        var bc = start.copy

        while(iterator.hasNext)
        {
            val dir = iterator.next()
            if (dir == prev) bc.offset(dir)
            else
            {
                newList += bc
                bc = bc.copy.offset(dir)
                prev = dir
            }
        }

        newList += bc
        newList.result().map(_.toVector3Centered)
    }
}

private object Node
{
    def apply(bc:BlockCoord):Node = new Node(bc.copy, 0, 6)
    def apply(bc:BlockCoord, dir:Int):Node = new Node(bc.copy.offset(dir), 1, dir, Seq(dir))
}
private class Node(val bc:BlockCoord, val dist:Int, val dir:Int, val path:Seq[Int] = Seq.empty) extends Ordered[Node]
{
    def -->(toDir:Int, distAway:Int):Node =
    {
        val bc2 = bc.copy.offset(toDir)
        new Node(bc2, dist+distAway, toDir, path :+ toDir)
    }
    def -->(toDir:Int):Node = this -->(toDir, 1)

    override def compare(that:Node) = dist-that.dist

    override def equals(other:Any) = other match
    {
        case that:Node =>
            bc == that.bc && dir == that.dir
        case _ => false
    }

    override def hashCode = bc.hashCode

    override def toString = "@"+bc.toString+": delta("+dir+")"
}