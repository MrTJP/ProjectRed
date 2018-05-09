/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.SEIntegratedCircuit._
import net.minecraft.nbt.NBTTagCompound

import scala.collection.mutable.{Map => MMap, Set => MSet}

trait IWireICTile
{
    /**
      * Returns true if this wire is attached to a signal sink on side r
      */
    def isNetOutput(r:Int):Boolean

    /**
      * Returns true if this wire is attached to a signal source on side r
      */
    def isNetInput(r:Int):Boolean

    /**
      * Returns the type of connection on side r:
      * SingleWire = 0, PortWire = 1, BusWire = 2
      */
    def getConnType(r:Int):Int

    /**
      * Returns the colour mask of this wire.
      */
    def getInputColourMask(r:Int):Int

    /**
      * Returns a mask of colours this wire can propagate out to.
      */
    def getOutputColourMask(r:Int):Int

    /**
      * Returns the propagation mask corresponding to side r
      */
    def getPropMask(r:Int):Int

    /**
      * Returns true if the wire is connected on a side
      */
    def isConnected(r:Int):Boolean
}

object IWireICTile
{
    val SingleWire = 0
    val PortWire = 1
    val BusWire = 2
}

abstract class WireICTile extends ICTile with TConnectableICTile with ISEWireTile with IWireICTile
{
    override def save(tag:NBTTagCompound)
    {
        tag.setByte("connMap", connMap)
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getByte("connMap")
    }

    override def writeDesc(out:MCDataOutput)
    {
        out.writeByte(connMap)
    }

    override def readDesc(in:MCDataInput)
    {
        connMap = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 => connMap = in.readByte()
        case _ => super.read(in, key)
    }

    def sendConnUpdate()
    {
        writeStreamOf(1).writeByte(connMap)
    }

    override def onMaskChanged()
    {
        sendConnUpdate()
        editor.markSchematicChanged()
    }

    override def onNeighborChanged()
    {
        if (!editor.network.isRemote)
            updateConns()
    }

    override def onAdded()
    {
        super.onAdded()
        if (!editor.network.isRemote)
            updateConns()
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!editor.network.isRemote) notify(connMap)
    }

    override def getPropMask(r:Int) = 0xF

    override def isConnected(r:Int) = maskConnects(r)

    override def buildWireNet(r:Int) =
    {
        val wireNet = new WireNet(tileMap, pos, 0xF)
        wireNet.calculateNetwork()
        wireNet
    }
}

private class WireNetChannel
{
    val points = MSet[(Point, Int)]()

    val inputs = MSet[(Point, Int)]()
    val outputs = MSet[(Point, Int)]()

    private val inputsToRegIDMap = MMap[(Point, Int), Int]()
    private var outputRegID = -1

    def allocateRegisters(linker:ISELinker)
    {
        outputRegID = linker.allocateRegisterID(points.map(_._1).toSet)
        linker.addRegister(outputRegID, new StandardRegister[Byte](0))

        if (inputs.size > 1) { //multiple drivers to this channel, will have to be OR'd together
            for (s <- inputs) {
                val id = linker.allocateRegisterID(points.map(_._1).toSet)
                linker.addRegister(id, new StandardRegister[Byte](0))
                inputsToRegIDMap += s -> id
            }
        } else { //only one driver to this channel. input can write to output register directly
            for (s <- inputs) //should only be 1 in here
                inputsToRegIDMap += s -> outputRegID
        }
    }

    def declareOperations(linker:ISELinker)
    {
        if (inputs.size > 1) {
            val gateID = linker.allocateGateID(points.map(_._1).toSet)
            val outRegID = outputRegID
            val inRegIDs = inputsToRegIDMap.values.toSeq
            val op = new ISEGate {
                override def compute(ic:SEIntegratedCircuit) {
                    ic.queueRegVal[Byte](outRegID,
                        if (inRegIDs.exists(ic.getRegVal(_) != 0)) 1 else 0)
                }
            }
            linker.addGate(gateID, op, inRegIDs, Seq(outputRegID))
        }
    }

    def getInputRegID(p:Point, r:Int) = inputsToRegIDMap((p, r))

    def getOutputRegID = outputRegID

    def getAllRegisters = inputsToRegIDMap.values.toSet + outputRegID
}

class ImplicitWireNet(ic:ICTileMapContainer, p:Point, r:Int) extends IWireNet
{
    override val points = MSet[(Point, Int)]()

    private var regID = -1

    private var hasSignalIn = false
    private var hasSignalOut = false

    def calculateNetwork()
    {
        points += ((p, r))
        val p2 = p.offset(r)
        points += ((p2, (r+2)%4))

        val t1 = ic.getTile(p)
        val t2 = ic.getTile(p2)

        (t1, t2) match {
            case (g1:IRedwireICGate with TConnectableICTile, g2:IRedwireICGate with TConnectableICTile) =>
                hasSignalOut = g1.canOutputTo(r) && g2.canInputFrom(g1.rotFromStraight(r))
                hasSignalIn = g1.canInputFrom(r) && g2.canOutputTo(g1.rotFromStraight(r))
            case _ =>
        }
    }

    def isRedundant = !hasSignalIn && !hasSignalOut

    override def allocateRegisters(linker:ISELinker)
    {
        regID = linker.allocateRegisterID(points.map(_._1).toSet)
        linker.addRegister(regID, new StandardRegister[Byte](0))
    }

    override def declareOperations(linker:ISELinker){}

    override def getInputRegister(p:Point, r:Int) = if (points.contains((p, r))) regID else REG_ZERO

    override def getOutputRegister(p:Point, r:Int) = if (points.contains((p, r))) regID else REG_ZERO
}

class WireNet(ic:ICTileMapContainer, p:Point, mask:Int) extends IWireNet
{
    override val points = MSet[(Point, Int)]()

    private val channels = MSet[WireNetChannel]()

    private val inputs = MSet[(Point, Int)]()
    private val outputs = MSet[(Point, Int)]()

    private val busWires = MSet[(Point, Int)]()
    private val portWires = MSet[(Point, Int)]()
    private val singleWires = MSet[(Point, Int)]()

    private val pointToChannelMap = MMap[(Point, Int), WireNetChannel]() //non bus points to channel map
    private val busPointToChannelsMap = MMap[(Point, Int), MSet[WireNetChannel]]() //bus points to passing channels map

    private def searchForWireNet(open:Seq[NetSearchNode], closed:Set[NetSearchNode] = Set()):Unit = open match {
        case Seq() =>
        case Seq(next, rest@_*) => ic.getTile(next.pos) match {
            case w:IWireICTile =>

                val upNext = Seq.newBuilder[NetSearchNode]

                for (r <- 0 until 4) if ((next.mask&1<<r) != 0) {
                    w.getConnType(r) match {
                        case IWireICTile.BusWire => busWires += ((next.pos, r))
                        case IWireICTile.PortWire => portWires += ((next.pos, r))
                        case IWireICTile.SingleWire => singleWires += ((next.pos, r))
                    }

                    if (w.isNetOutput(r)) outputs += ((next.pos, r))
                    if (w.isNetInput(r)) inputs += ((next.pos, r))

                    if (w.isConnected(r)) ic.getTile(next.pos.offset(r)) match {
                        case w2:IWireICTile =>
                            val p = next --> (r, w2.getPropMask((r+2)%4))
                            if (!closed(p) && !open.contains(p)) upNext += p
                        case _ =>
                    }

                    points += ((next.pos, r))
                }

                searchForWireNet(rest ++ upNext.result(), closed + next)

            case _ =>
                searchForWireNet(rest)
        }
    }

    def mapChannelForPoint(p:Point, r:Int):Seq[(Point, Int)] =
    {
        val (cmask, pmask) = ic.getTile(p) match {
            case w:IWireICTile =>
                (w.getInputColourMask(r)|w.getOutputColourMask(r), w.getPropMask(r))
            case _ => (0, 0)
        }

        if (cmask == 0) return null
        if (pmask == 0) return null

        def iterate(open:Seq[CSearchNode2], closed:Set[CSearchNode2] = Set(), points:Seq[(Point, Int)] = Seq()):Seq[(Point, Int)] = open match {
            case Seq() => points
            case Seq(next, rest@_*) => ic.getTile(next.pos) match {
                case w:IWireICTile =>
                    val upNext = Seq.newBuilder[CSearchNode2]
                    for (r <- 0 until 4) if ((next.pmask&1<<r) != 0 && w.isConnected(r)) {
                        ic.getTile(next.pos.offset(r)) match {
                            case w2:IWireICTile =>
                                val newCMask = (1<<next.colour & w2.getInputColourMask((r+2)%4)) | w2.getOutputColourMask((r+2)%4)
                                val newPMask = w2.getPropMask((r+2)%4)
                                val routes = next --> (r, newCMask, newPMask)
                                for (r <- routes) {
                                    if (!open.contains(r) && !closed.contains(r))
                                        upNext += r
                                }
                            case _ =>
                        }

                    }
                    iterate(rest ++ upNext.result(), closed + next, (points :+ (next.pos, next.r)).distinct)
            }
        }

        iterate(CSearchNode2.startNodes(p, cmask, pmask))
    }

    def calculateNetwork()
    {
        searchForWireNet(Seq(NetSearchNode(p, mask)))

        def getOrCreateChannel(p:Point, r:Int):WireNetChannel = {
            pointToChannelMap.get((p, r)) match {
                case Some(c) => c
                case _ =>
                    val points = mapChannelForPoint(p, r)
                    val ch = new WireNetChannel
                    ch.points ++= points
                    channels += ch

                    for (p <- points)
                        if (busWires.contains(p)) {
                            busPointToChannelsMap.getOrElseUpdate(p, MSet()) += ch
                        } else {
                            pointToChannelMap += p -> ch
                        }
                    ch
            }
        }

        for ((p, r) <- inputs) {
            val channel = getOrCreateChannel(p, r)
            channel.inputs += ((p, r))
        }
        for ((p, r) <- outputs) {
            val channel = getOrCreateChannel(p, r)
            channel.outputs += ((p, r))
        }
    }

    override def allocateRegisters(linker:ISELinker)
    {
        val list = Seq.newBuilder[Point]
        for (pos <- points)
            if (!pointToChannelMap.contains(pos) && !busPointToChannelsMap.contains(pos))
                list += pos._1
        val res = list.result()
        if (res.nonEmpty)
            linker.getLogger.logWarning(res, "wire has no associated channel")


        for (ch <- channels) {
            if (ch.inputs.isEmpty || ch.outputs.isEmpty) {
                val points = ch.points.filter(!busWires.contains(_))

                if (ch.outputs.isEmpty)
                    linker.getLogger.logWarning(points.map(_._1).toSeq, "wirenet channel has no outputs")

                if (ch.inputs.isEmpty)
                    linker.getLogger.logWarning(points.map(_._1).toSeq, "wirenet channel has no inputs")
            }
            ch.allocateRegisters(linker)
        }
    }

    override def declareOperations(linker:ISELinker)
    {
        for (ch <- channels)
            ch.declareOperations(linker)
    }

    override def getInputRegister(p:Point, r:Int) = pointToChannelMap.get((p, r)) match {
        case Some(ch) => ch.getInputRegID(p, r)
        case _ => REG_ZERO
    }

    override def getOutputRegister(p:Point, r:Int) = pointToChannelMap.get((p, r)) match {
        case Some(ch) => ch.getOutputRegID
        case _ => REG_ZERO
    }
}

private case class NetSearchNode(pos:Point, mask:Int)
{
    def -->(r:Int, m:Int):NetSearchNode = NetSearchNode(pos.offset(r), m)

    override def equals(that:Any) = that match {
        case n:NetSearchNode => n.pos == pos && n.mask == mask
        case _ => false
    }
}

private object CSearchNode2
{
    def startNodes(pos:Point, colourMask:Int, propMask:Int):Seq[CSearchNode2] =
    {
        val b = Seq.newBuilder[CSearchNode2]
        for (r <- 0 until 4) if ((propMask&1<<r) != 0)
            for (i <- 0 until 16) if ((colourMask&1<<i) != 0)
                b += CSearchNode2(pos, i, r, propMask)
        b.result()
    }
}

private case class CSearchNode2(pos:Point, colour:Int, r:Int, pmask:Int)
{
    def -->(towardsR:Int, colourMask:Int, propMask:Int):Seq[CSearchNode2] =
    {
        val b = Seq.newBuilder[CSearchNode2]
        val p = pos.offset(towardsR)
        for (r <- 0 until 4) if ((propMask&1<<r) != 0)
            for (i <- 0 until 16) if ((colourMask&1<<i) != 0)
                b += CSearchNode2(p, i, r, propMask)
        b.result()
    }

    override def equals(that:Any) = that match {
        case n:CSearchNode2 => n.pos == pos && n.colour == colour && n.pmask == pmask
        case _ => false
    }
}