package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.core.libmc.inventory.InvWrapper
import mrtjp.projectred.core.libmc.{ItemKey, PRLib}
import net.minecraft.inventory.IInventory

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object PressurePriority
{
    val inventory = 0x1
    val backlog = 0x2
    val setDestination = 0x4
}

class PressurePathfinder(item:ItemKey, pipe:TPressureSubsystem, flags:Int, dim:Int, color:Int = -1)
{
    private var shortestDist = Integer.MAX_VALUE
    private var isBacklog = true
    private var directions = 0

    private def setPath(n:Node, bl:Boolean = false)
    {
        if (n.dist < shortestDist || (isBacklog && !bl))
        {
            shortestDist = n.dist
            directions = 0
            isBacklog = false
        }
        if (n.dist == shortestDist) directions |= 1<<n.hop
    }

    def result() =
    {
        val bc = new BlockCoord(pipe.tile)
        val q = Queue.newBuilder[Node]
        for (s <- 0 until 6 if (dim&1<<s) != 0 && pipe.maskConnects(s)) q += Node(bc, s)
        iterate(q.result())
        directions
    }

    @tailrec
    private def iterate(open:Seq[Node], closed:Set[Node] = Set.empty):Unit = open match
    {
        case Seq() =>
        case Seq(next, rest@_*) => getTile(next.bc) match
        {
            case inv:IInventory if (flags&PressurePriority.inventory) != 0 =>
                if (InvWrapper.wrap(inv).hasSpaceForItem(item)) setPath(next)
                iterate(rest, closed+next)
            case _ => getPipe(next.bc) match
            {
                case d:TPressureDevice => iterate(rest, closed+next)

                case p:TPressureTube =>
                    val upNext = Vector.newBuilder[Node]
                    for (s <- 0 until 6) if (s != (next.dir^1) && p.maskConnects(s))
                    {
                        val route = next --> (s, p.pathWeight, p.pathFilter(next.dir^1, s))
                        if (route.flagRouteTo && route.allowColor(color) && route.allowItem(item))
                            if (!closed(route)) upNext += route
                    }
                    iterate(rest++upNext.result(), closed+next)

                case _ => iterate(rest, closed+next)
            }
        }
        case _ =>
    }

    private def getPipe(bc:BlockCoord) = PRLib.getMultiPart(pipe.world, bc, 6) match
    {
        case p:TPressureSubsystem => p
        case _ => null
    }

    private def getTile(bc:BlockCoord) = pipe.world.getTileEntity(bc.x, bc.y, bc.z)

    private object Node
    {
        def apply(bc:BlockCoord, dir:Int):Node = new Node(bc.copy().offset(dir), 1, dir, dir)
    }
    private class Node(val bc:BlockCoord, val dist:Int, val dir:Int, val hop:Int, filters:Set[PathFilter] = Set.empty) extends Path(filters) with Ordered[Node]
    {
        def -->(toDir:Int, distAway:Int, filter:PathFilter):Node = new Node(bc.copy.offset(toDir), dist+distAway, toDir, hop, filters+filter)
        def -->(toDir:Int, distAway:Int):Node = new Node(bc.copy().offset(toDir), dist+distAway, toDir, hop, filters)
        def -->(toDir:Int):Node = this -->(toDir, 1)

        override def compare(that:Node) = dist-that.dist

        override def equals(other:Any) = other match
        {
            case that:Node => bc == that.bc && hop == that.hop
            case _ => false
        }
    }
}