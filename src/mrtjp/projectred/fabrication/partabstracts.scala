/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.vec.{Vector3, Rotation}
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.IWireICPart._

trait TICOrient extends CircuitPart
{
    var orientation:Byte = 0

    def rotation = orientation&0x3

    def setRotation(r:Int)
    {
        orientation = (orientation&0xFC|r).toByte
    }

    def rotationT = Rotation.quarterRotations(rotation).at(new Vector3(0.5, 0, 0.5))

    // internal r from absRot
    def toInternal(absRot:Int) = (absRot+4-rotation)%4
    // absRot from internal r
    def toAbsolute(r:Int) = (r+rotation)%4

    def toInternalMask(mask:Int) = TICOrient.shiftMask(mask, toInternal(0))
    def toAbsoluteMask(mask:Int) = TICOrient.shiftMask(mask, toAbsolute(0))
}

object TICOrient
{
    def shiftMask(mask:Int, r:Int) = (mask& ~0xF)|(mask<<r|mask>>4-r)&0xF
    def flipMaskZ(mask:Int) = mask&5|mask<<2&8|mask>>2&2
}

trait TICAcquisitions extends CircuitPart
{
    def getStraight(r:Int) = world.getPart(posOfStraight(r))
    def posOfStraight(r:Int) = Point(x, y).offset(r)
    def rotFromStraight(r:Int) = (r+2)%4

    def notifyToDir(r:Int){world.notifyNeighbor(posOfStraight(r))}
    def notify(mask:Int){world.notifyNeighbors(x, y, mask)}
}

trait TICRSAcquisitions extends TICAcquisitions with IPoweredCircuitPart
{
    def calcSignal(r:Int):Int = resolveSignal(getStraight(r), rotFromStraight(r))

    def resolveSignal(part:Any, r:Int):Int
}

trait TICBundledAcquisitions extends TICAcquisitions
{
    def calcArray(r:Int):Array[Byte] = resolveArray(getStraight(r), rotFromStraight(r))

    def resolveArray(part:Any, r:Int):Array[Byte]
}

trait TConnectableICPart extends CircuitPart with TICAcquisitions
{
    var connMap:Byte = 0

    def maskConnects(r:Int) = (connMap&1<<r) != 0

    def discover(r:Int) = getStraight(r) match
    {
        case c:TConnectableICPart => canConnectPart(c, r) && c.connect(this, rotFromStraight(r))
        case c => discoverOverride(r, c)
    }

    def discoverOverride(r:Int, part:CircuitPart) = false

    def connect(part:CircuitPart, r:Int) =
    {
        if (canConnectPart(part, r))
        {
            val oldConn = connMap
            connMap = (connMap|1<<r).toByte
            if (oldConn != connMap) onMaskChanged()
            true
        }
        else false
    }

    def updateConns() =
    {
        var newConn = 0
        for (r <- 0 until 4) if (discover(r)) newConn |= 1<<r
        if (newConn != connMap)
        {
            connMap = newConn.toByte
            onMaskChanged()
            true
        }
        else false
    }

    def canConnectPart(part:CircuitPart, r:Int):Boolean

    def onMaskChanged(){}
}

trait TPropagatingICPart extends CircuitPart with TConnectableICPart with IWireICPart
{
    var propagationMask = 0xF

    def propagate(prev:CircuitPart, mode:Int)
    {
        if (mode != FORCED) ICPropagator.addPartChange(this)
        for (r <- 0 until 4) if ((propagationMask&1<<r) != 0)
            if (maskConnects(r)) propagateExternal(getStraight(r), posOfStraight(r), prev, mode)

        propagateOther(mode)
    }

    def propagateOther(mode:Int){}

    def propagateExternal(to:CircuitPart, at:Point, from:CircuitPart, mode:Int)
    {
        if (to != null)
        {
            if (to == from) return
            if (propagateTo(to, mode)) return
        }
        ICPropagator.addNeighborChange(at)
    }

    def propagateTo(part:CircuitPart, mode:Int) = part match
    {
        case w:IWireICPart =>
            ICPropagator.propagateTo(w, this, mode)
            true
        case _ => false
    }
}

trait TRSPropagatingICPart extends TPropagatingICPart
{
    def calculateSignal:Int

    def getSignal:Int
    def setSignal(signal:Int)

    override def updateAndPropagate(prev:CircuitPart, mode:Int)
    {
        if (mode == DROPPING && getSignal == 0) return
        val newSignal = calculateSignal
        if (newSignal < getSignal)
        {
            if (newSignal > 0) ICPropagator.propagateAnalogDrop(this)
            setSignal(0)
            propagate(prev, DROPPING)
        }
        else if (newSignal > getSignal)
        {
            setSignal(newSignal)
            if (mode == DROPPING) propagate(null, RISING)
            else propagate(prev, RISING)
        }
        else if (mode == DROPPING) propagateTo(prev, RISING)
        else if (mode == FORCE) propagate(prev, FORCED)
    }
}