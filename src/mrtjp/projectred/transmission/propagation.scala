package mrtjp.projectred.transmission

import codechicken.lib.vec.BlockCoord
import codechicken.multipart.handler.MultipartProxy
import codechicken.multipart.{TileMultipart, TMultiPart}
import com.google.common.collect.HashMultimap
import java.util
import mrtjp.projectred.core.CommandDebug
import net.minecraft.block.{Block, BlockRedstoneWire}
import net.minecraft.world.World
import scala.collection.immutable.HashSet
import net.minecraft.init.Blocks

object WirePropagator
{
    private val wiresProvidePower =
    {
        try
        {
            val c = classOf[BlockRedstoneWire].getDeclaredFields.apply(0)
            c.setAccessible(true)
            c
        }
        catch {case e:Exception => throw new RuntimeException(e)}
    }
    def setDustProvidePower(b:Boolean)
    {
        try {wiresProvidePower.setBoolean(Blocks.redstone_wire, b)}
        catch {case t:Throwable =>}
    }

    private val rwConnectable = new ThreadLocal[Boolean]
    rwConnectable.set(true)
    def redwiresConnectable = rwConnectable.get
    def setRedwiresConnectable(b:Boolean) {rwConnectable.set(b)}

    var redwiresProvidePower = true

    val reusableRuns = new util.Stack[PropagationRun]
    var currentRun:PropagationRun = null
    var finishing:PropagationRun = null

    val notApart = new TMultiPart
    {
        def getType = null
    }

    def addNeighborChange(pos:BlockCoord)
    {
        currentRun.neighborChanges += pos
    }

    def addPartChange(part:TMultiPart)
    {
        currentRun.partChanges.put(part.tile, part)
    }

    def logCalculation()
    {
        if (finishing != null) finishing.recalcs += 1
    }

    def propagateTo(part:IWirePart, from:TMultiPart, mode:Int)
    {
        var p = currentRun
        if (p == null) p = if (reusableRuns.isEmpty) new PropagationRun else reusableRuns.pop
        p.add(part, from, mode)
        if (currentRun != p)
        {
            if (currentRun != null) throw new RuntimeException("Report this to ProjectRed developers")
            p.start(finishing, part.world)
        }
    }

    def propagateTo(part:IWirePart, mode:Int)
    {
        propagateTo(part, notApart, mode)
    }

    def propagateAnalogDrop(part:IWirePart)
    {
        currentRun.addAnalogDrop(part)
    }
}

class PropagationRun
{
    var world:World = null
    var parent:PropagationRun = null
    var lastCaller:TMultiPart = null
    var count = 0
    var recalcs = 0
    var partChanges = HashMultimap.create[TileMultipart, TMultiPart]
    var neighborChanges = HashSet[BlockCoord]()
    var propagationList = List[Propagation]()
    var analogDrops = List[Propagation]()

    def clear()
    {
        partChanges.clear()
        neighborChanges = HashSet[BlockCoord]()
        count = 0
        recalcs = 0
        lastCaller = null
        WirePropagator.reusableRuns.add(this)
    }

    def finish()
    {
        WirePropagator.currentRun = null
        if (partChanges.isEmpty && neighborChanges.isEmpty)
        {
            WirePropagator.finishing = parent
            clear()
            return
        }

        WirePropagator.finishing = this
        if (CommandDebug.WIRE_READING)
            println(count+" propogations, "+partChanges.size+" part changes, "+neighborChanges.size+" block updates")

        import scala.collection.JavaConversions._
        for (entry <- partChanges.asMap.entrySet)
        {
            val parts = entry.getValue

            import scala.collection.JavaConversions._
            for (part <- parts) part.asInstanceOf[IWirePart].onSignalUpdate()
            entry.getKey.multiPartChange(parts)
        }

        neighborChanges.foreach(b => world.notifyBlockOfNeighborChange(b.x, b.y, b.z, MultipartProxy.block))

        WirePropagator.finishing = parent

        if (CommandDebug.WIRE_READING) println(recalcs+" recalculations")

        clear()
    }

    def start(parent:PropagationRun, world:World)
    {
        this.world = world
        this.parent = parent
        WirePropagator.currentRun = this
        runLoop()
    }

    private def runLoop()
    {
        do
        {
            val tmp = propagationList
            propagationList = List[Propagation]()
            tmp.foreach(p => p.go())

            if (propagationList.isEmpty && !analogDrops.isEmpty)
            {
                propagationList = analogDrops
                analogDrops = List[Propagation]()
            }
        }
        while (!propagationList.isEmpty)
        finish()
    }

    def add(part:IWirePart, from:TMultiPart, mode:Int)
    {
        if (from != lastCaller)
        {
            lastCaller = from
            count += 1
        }
        propagationList :+= new Propagation(part, from, mode)
    }

    def addAnalogDrop(part:IWirePart)
    {
        analogDrops :+= new Propagation(part, WirePropagator.notApart, IWirePart.RISING)
    }
}

class Propagation(part:IWirePart, from:TMultiPart, mode:Int)
{
    def go()
    {
        part.updateAndPropagate(from, mode)
    }
}

object IWirePart
{
    /**
     * Standard operation procedure, no special propogation rules. The
     * propogator signal may not have increased.
     */
    final val RISING = 0
    /**
     * Used when the propogator signal dropped (to 0). Propagation should
     * continue until a rising or constant change is encountered at which point
     * a RISING should be propogated back to this wire.
     */
    final val DROPPING = 1
    /**
     * Used when a wire's connection state has changed. Even if the signal
     * remains the same, new connections still need to be recalculated
     */
    final val FORCE = 2
    /**
     * Used when the propogator did not change signal, but a new connection may
     * have been established and signal needs recalculating
     */
    final val FORCED = 3
}

/**
 * Trait that marks a propagation subject
 */
trait IWirePart
{
    /**
     * Recalculates the signal of this wire and calls the appropriate
     * propogation methods in WirePropagator. DO NOT CALL THIS YOURSELF. Use
     * WirePropagator.propagateTo
     *
     * @param prev The part which called this propogation (should be connected)
     *             may be null.
     * @param mode One of RISING, DROPPING, FORCE and FORCED specified above
     */
    def updateAndPropagate(prev:TMultiPart, mode:Int)

    /**
     * Called at the end of a propogation run for partChanged events. Marks the
     * end of a state change for this part.
     */
    def onSignalUpdate()

    /**
     * @param side The side of this part to test for wire connection. For face
     *             parts, a rotation, or -1 for center. For center parts, a
     *             forgedirection. The special value Integer.MAX_VALUE should
     *             always return true and is used for return signals
     * @return true if the specified side of this block is connected to a 'wire'
     *         where signal should decrease by one.
     */
    def isWireSide(side:Int):Boolean

    /**
     * The world in which this part resides
     *
     * @return
     */
    def world:World
}