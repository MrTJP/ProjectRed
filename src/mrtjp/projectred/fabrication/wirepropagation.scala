/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import java.util.{Stack => JStack}

import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.core.vec.Point

import scala.collection.immutable.HashSet

object ICPropagator
{
    var redwiresProvidePower = true
    var redwiresConnectable = true

    val reusableRuns = new JStack[ICPropagationRun]()
    var currentRun:ICPropagationRun = null
    var finishing:ICPropagationRun = null

    val notApart = new CircuitPart {
        override def getPartType = null
        @SideOnly(Side.CLIENT)
        override def getPartName = ""
    }

    def addNeighborChange(pos:Point)
    {
        currentRun.neighborChanges += pos
    }

    def addPartChange(part:CircuitPart)
    {
        currentRun.partChanges += part
    }

    def logCalculation()
    {
        if (finishing != null) finishing.recalcs += 1
    }

    def propagateTo(part:IWireICPart, from:CircuitPart, mode:Int)
    {
        var p = currentRun
        if (p == null) p = if (reusableRuns.isEmpty) new ICPropagationRun else reusableRuns.pop
        p.add(part, from, mode)
        if (currentRun != p)
        {
            if (currentRun != null) throw new RuntimeException("Report this to ProjectRed developers")
            p.start(finishing, part.world)
        }
    }

    def propagateTo(part:IWireICPart, mode:Int)
    {
        propagateTo(part, notApart, mode)
    }

    def propagateAnalogDrop(part:IWireICPart)
    {
        currentRun.addAnalogDrop(part)
    }
}

class ICPropagationRun
{
    var world:IntegratedCircuit = null
    var parent:ICPropagationRun = null
    var lastCaller:CircuitPart = null
    var count = 0
    var recalcs = 0

    var partChanges = HashSet.newBuilder[CircuitPart]
    var neighborChanges = HashSet.newBuilder[Point]
    var propagationList = Seq.newBuilder[ICPropagation]
    var analogDrops = Seq.newBuilder[ICPropagation]

    def clear()
    {
        partChanges.clear()
        neighborChanges.clear()
        count = 0
        recalcs = 0
        lastCaller = null
        ICPropagator.reusableRuns.add(this)
    }

    def finish()
    {
        ICPropagator.currentRun = null
        val res_PartChanges = partChanges.result()
        val res_NeighborChanges = neighborChanges.result()

        if (res_PartChanges.isEmpty && res_NeighborChanges.isEmpty)
        {
            ICPropagator.finishing = parent
            clear()
            return
        }

        ICPropagator.finishing = this
        res_PartChanges.foreach(_.asInstanceOf[IWireICPart].onSignalUpdate())
        res_NeighborChanges.foreach(p => world.notifyNeighbor(p.x, p.y))
        ICPropagator.finishing = parent

        clear()
    }

    def start(parent:ICPropagationRun, world:IntegratedCircuit)
    {
        this.world = world
        this.parent = parent
        ICPropagator.currentRun = this
        runLoop()
    }

    private var pChange = false
    private var aChange = false
    private def runLoop()
    {
        var ptmp:Seq[ICPropagation] = null
        var atmp:Seq[ICPropagation] = null

        def fetch()
        {
            if (pChange || ptmp == null) ptmp = propagationList.result()
            if (aChange || atmp == null) atmp = analogDrops.result()
            pChange = false
        }
        fetch()

        do
        {
            propagationList.clear(); pChange = true
            ptmp.foreach(_.go())
            fetch()
            if (ptmp.isEmpty && atmp.nonEmpty)
            {
                propagationList = analogDrops; ptmp = atmp; pChange = false
                analogDrops = Seq.newBuilder; aChange = true
            }
        }
        while(ptmp.nonEmpty)
        finish()
    }

    def add(part:IWireICPart, from:CircuitPart, mode:Int)
    {
        if (from != lastCaller)
        {
            lastCaller = from
            count += 1
        }
        propagationList += new ICPropagation(part, from, mode)
        pChange = true
    }

    def addAnalogDrop(part:IWireICPart)
    {
        analogDrops += new ICPropagation(part, ICPropagator.notApart, IWireICPart.RISING)
        aChange = true
    }
}

class ICPropagation(part:IWireICPart, from:CircuitPart, mode:Int)
{
    def go(){part.updateAndPropagate(from, mode)}
}

object IWireICPart
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

trait IWireICPart
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
    def updateAndPropagate(prev:CircuitPart, mode:Int)

    /**
     * Called at the end of a propogation run for partChanged events. Marks the
     * end of a state change for this part.
     */
    def onSignalUpdate()

    /**
     * @param r The rotation of this part to test for wire connection.
     * @return true if the specified side of this block is connected to, for
     *         example, a 'wire' where signal should decrease by one.
     */
    def diminishOnSide(r:Int):Boolean

    /**
     * The world in which this part resides
     *
     * @return
     */
    def world:IntegratedCircuit
}

