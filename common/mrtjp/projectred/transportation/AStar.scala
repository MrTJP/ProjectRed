package mrtjp.projectred.transportation

import codechicken.lib.vec.BlockCoord
import java.util.PriorityQueue
import net.minecraft.world.World

abstract class AStar(world:World)
{
    var closed = Set[PathNode]()
    val open = new PriorityQueue[PathNode]()

    def openInitials()

    def recurse()
    {
        while (!open.isEmpty)
        {
            val dequeue = open.poll()
            if (!isClosed(dequeue))
            {
                evaluate(dequeue)
                close(dequeue)
            }
        }
    }

    def isClosed(n:PathNode) = closed.contains(n)

    def close(n:PathNode)
    {
        closed += n
    }

    def open(n:PathNode)
    {
        open.add(n)
    }

    def evaluate(n:PathNode)

    def start()
    {
        openInitials()
        recurse()
    }

}

class PathNode(val path:Vector[PathNode], val bc:BlockCoord, val dist:Int, val dir:Int, val hop:Int) extends Ordered[PathNode]
{
    def this(bc:BlockCoord, dir:Int) = this(Vector[PathNode](), bc.copy().offset(dir), 1, dir, dir)

    def -->(to:PathNode, toDir:Int):PathNode = new PathNode(path :+ this, to.bc, dist+to.dist, toDir, hop)

    def -->(toDir:Int, distAway:Int):PathNode = new PathNode(path :+ this, bc.copy().offset(toDir), dist+distAway, toDir, hop)
    def -->(toDir:Int):PathNode = this --> (toDir, 1)

    def -->(to:BlockCoord, distAway:Int):PathNode = new PathNode(path :+ this, to, dist+distAway, dir, hop)
    def -->(to:BlockCoord):PathNode = this --> (to, 1)

    override def compare(that:PathNode) = dist-that.dist

    override def equals(other:Any) = other match
    {
        case that:PathNode => bc == that.bc && hop == that.hop
        case _ => false
    }
}