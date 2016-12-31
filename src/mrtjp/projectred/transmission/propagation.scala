package mrtjp.projectred.transmission

import java.util.{Stack => JStack}

import codechicken.multipart.handler.MultipartProxy
import codechicken.multipart.{TMultiPart, TileMultipart}
import com.google.common.collect.HashMultimap
import net.minecraft.block.BlockRedstoneWire
import net.minecraft.init.Blocks
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

import scala.collection.immutable.HashSet

object WirePropagator
{
    private val wiresProvidePower =
    {
        try {
            val c = classOf[BlockRedstoneWire].getDeclaredFields.apply(0)
            c.setAccessible(true)
            c
        }
        catch {case e:Exception => throw new RuntimeException(e)}
    }
    def setDustProvidePower(b:Boolean)
    {
        try {wiresProvidePower.setBoolean(Blocks.REDSTONE_WIRE, b)}
        catch {case t:Throwable =>}
    }

    private val rwConnectable = {val b = new ThreadLocal[Boolean]; b.set(true); b}
    def redwiresConnectable = rwConnectable.get
    def setRedwiresConnectable(b:Boolean) {rwConnectable.set(b)}

    var redwiresProvidePower = true

    def reset()
    {
        setDustProvidePower(true)
        setRedwiresConnectable(true)
        redwiresProvidePower = true
    }

    val reusableRuns = new JStack[PropagationRun]()
    var currentRun:PropagationRun = null
    var finishing:PropagationRun = null

    val notApart = new TMultiPart
    {
        def getType = null
    }

    def addNeighborChange(pos:BlockPos)
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
        if (currentRun != p) {
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
    var neighborChanges = HashSet.newBuilder[BlockPos]
    var propagationList = Seq.newBuilder[Propagation]
    var analogDrops = Seq.newBuilder[Propagation]

    def clear()
    {
        partChanges.clear()
        neighborChanges.clear()
        count = 0
        recalcs = 0
        lastCaller = null
        WirePropagator.reusableRuns.add(this)
    }

    def finish()
    {
        WirePropagator.currentRun = null
        val res_NeighborChanges = neighborChanges.result()

        if (partChanges.isEmpty && res_NeighborChanges.isEmpty) {
            WirePropagator.finishing = parent
            clear()
            return
        }

        WirePropagator.finishing = this
//        if (CommandDebug.WIRE_READING)
//            println(count+" propogations, "+partChanges.size+" part changes, "+res_NeighborChanges.size+" block updates")

        import scala.collection.JavaConversions._
        for (entry <- partChanges.asMap.entrySet) {
            val parts = entry.getValue

            for (part <- parts) part.asInstanceOf[IWirePart].onSignalUpdate()
            entry.getKey.multiPartChange(parts)
        }

        res_NeighborChanges.foreach(b => world.notifyBlockOfStateChange(b.toImmutable, MultipartProxy.block))

        WirePropagator.finishing = parent

//        if (CommandDebug.WIRE_READING) println(recalcs+" recalculations")

        clear()
    }

    def start(parent:PropagationRun, world:World)
    {
        this.world = world
        this.parent = parent
        WirePropagator.currentRun = this
        runLoop()
    }

    private var pChange = false
    private var aChange = false
    private def runLoop()
    {
        var ptmp:Seq[Propagation] = null
        var atmp:Seq[Propagation] = null

        def fetch()
        {
            if (pChange || ptmp == null) ptmp = propagationList.result()
            if (aChange || atmp == null) atmp = analogDrops.result()
            pChange = false
            aChange = false
        }
        fetch()

        do {
            propagationList.clear(); pChange = true //we emptied it, probably changed it, but if we didnt, the loop will break anyway.
            ptmp.foreach(_.go())

            fetch() //Update results

            if (ptmp.isEmpty && atmp.nonEmpty) {
                propagationList = analogDrops; ptmp = atmp; pChange = false //atmp is already up to date, so now ptmp is too.
                analogDrops = Vector.newBuilder; aChange = true //atmp was nonempty, now it is
            }
        }
        while (ptmp.nonEmpty)
        finish()
    }

    def add(part:IWirePart, from:TMultiPart, mode:Int)
    {
        if (from != lastCaller) {
            lastCaller = from
            count += 1
        }
        propagationList += new Propagation(part, from, mode)
        pChange = true
    }

    def addAnalogDrop(part:IWirePart)
    {
        analogDrops += new Propagation(part, WirePropagator.notApart, IWirePart.RISING)
        aChange = true
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
     *             parts, a rotation, or -1 for center (up). For center parts, a
     *             forgedirection.
     * @return true if the specified side of this block is connected to, for
     *         example, a 'wire' where signal should decrease by one.
     */
    def diminishOnSide(side:Int):Boolean

    /**
     * The world in which this part resides
     *
     * @return
     */
    def world:World
}
