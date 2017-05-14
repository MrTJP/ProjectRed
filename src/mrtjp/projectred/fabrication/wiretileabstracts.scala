/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.core.vec.Point
import net.minecraft.nbt.NBTTagCompound

import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}
import SEIntegratedCircuit._

abstract class WireICTile extends ICTile with TConnectableICTile with ISEWireTile
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

    def isNetOutput:Boolean

    def isNetInput:Boolean

    override def buildWireNet =
    {
        val wireNet = new WireNet(tileMap, Point(x, y))
        wireNet.calculateNetwork()
        wireNet
    }

    def getTravelMask:Int
    def getMixerMask:Int
}

class WireNetChannel
{
    val points = MSet[Point]()
    val inputs = MSet[Point]()
    val outputs = MSet[Point]()

    private val inputsToRegIDMap = MMap[Point, Int]()
    private var outputRegID = -1

    def allocateRegisters(linker:ISELinker)
    {
        outputRegID = linker.allocateRegisterID()
        linker.addRegister(outputRegID, new StandardRegister[Byte](0))

        if (inputs.size > 1) { //multiple drivers to this channel, will have to be OR'd together
            for (s <- inputs) {
                val id = linker.allocateRegisterID()
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
            val gateID = linker.allocateGateID()
            val outRegID = outputRegID
            val inRegIDs = inputsToRegIDMap.values.toSeq
            val op = new ISEGate {
                override def compute(ic:SEIntegratedCircuit) {
                    ic.queueRegVal[Byte](outRegID,
                        if (inRegIDs.exists(ic.getRegVal(_) != 0)) 1 else 0)
                }
            }
            linker.addGate(gateID, op, inputsToRegIDMap.values.toSeq, Seq(outputRegID))
        }
    }

    def getInputRegID(p:Point) = inputsToRegIDMap(p)

    def getOutputRegID = outputRegID

    def getAllRegisters = inputsToRegIDMap.values.toSet + outputRegID
}

class WireNet(ic:ICTileMapContainer, p:Point) extends IWireNet
{
    val points = MSet[Point]()

    private val channels = MSet[WireNetChannel]()
    private val pointToChannelMap = MMap[Point, WireNetChannel]() //non bus points to channel map
    private val busPointToChannelsMap = MMap[Point, MSet[WireNetChannel]]() //bus points to passing channels map

    private val busWires = MSet[Point]()
    private val portWires = MSet[Point]()
    private val singleWires = MSet[Point]()

    private val inputs = MSet[Point]()
    private val outputs = MSet[Point]()

    private def searchForWireNet(open:Seq[Point]):Unit = open match {
        case Seq() =>
        case Seq(next, rest@_*) => ic.getTile(next) match {
            case w:WireICTile =>
                w match {
                    case _:IBundledCableICPart => busWires += next
                    case _:IInsulatedRedwireICPart => portWires += next
                    case _:IRedwireICPart => singleWires += next
                }

                if (w.isNetOutput) outputs += next
                if (w.isNetInput) inputs += next

                val upNext = Seq.newBuilder[Point]
                for (r <- 0 until 4) if (w.maskConnects(r)) {
                    val p = next.offset(r)
                    if (!points(p) && !open.contains(p)) upNext += p
                }

                points += next
                searchForWireNet(rest ++ upNext.result())
            case _ =>
                searchForWireNet(rest)
        }
    }

    def mapChannelForPoint(p:Point):Seq[Point] =
    {
        val mask = ic.getTile(p) match {
            case w:WireICTile => w.getTravelMask|w.getMixerMask
            case _ => 0
        }
        if (mask == 0) return null

        def iterate(open:Seq[Node], closed:Set[Node] = Set(), points:Seq[Point] = Seq()):Seq[Point] = open match {
            case Seq() => points
            case Seq(next, rest@_*) => ic.getTile(next.pos) match {
                case w:WireICTile =>
                    val upNext = Seq.newBuilder[Node]
                    for (r <- 0 until 4) if (w.maskConnects(r)) {
                        ic.getTile(next.pos.offset(r)) match {
                            case w2:WireICTile =>
                                val newMask = (1<<next.colour & w2.getTravelMask) | w2.getMixerMask
                                val routes = next --> (r, newMask)
                                for (r <- routes) {
                                    if (!open.contains(r) && !closed.contains(r))
                                        upNext += r
                                }
                            case _ =>
                        }
                    }
                    iterate(rest ++ upNext.result(), closed + next, (points :+ next.pos).distinct)
            }
        }

        iterate(Node.startNodes(p, mask))
    }


    def calculateNetwork()
    {
        searchForWireNet(Seq(p))

        //create channels for all inputs and outputs
        def getOrCreateChannel(p:Point):WireNetChannel = {
            pointToChannelMap.get(p) match {
                case Some(c) => c
                case _ =>
                    val points = mapChannelForPoint(p)
                    val ch = new WireNetChannel
                    ch.points ++= points
                    channels += ch

                    /* TODO
                       Potentially problematic, as points of bus wires can be shared between channels.
                       Solution is to keep a pointToChannels map instead [point -> Seq(channel)]
                       Not dangerous as buses are not currently ever used as io for the wire net
                     */
                    for (p <- points)
                        if (busWires.contains(p)) {
                            busPointToChannelsMap.getOrElseUpdate(p, MSet()) += ch
                        } else {
                            pointToChannelMap += p -> ch
                        }
                    ch
            }
        }

        for (i <- inputs) {
            val channel = getOrCreateChannel(i)
            channel.inputs += i
        }
        for (o <- outputs) {
            val channel = getOrCreateChannel(o)
            channel.outputs += o
        }
    }

    override def allocateRegisters(linker:ISELinker)
    {
        val list = Seq.newBuilder[Point]
        for (p <- points)
            if (!pointToChannelMap.contains(p) && !busPointToChannelsMap.contains(p))
                list += p
        val res = list.result()
        if (res.nonEmpty) {
            linker.getLogger.logError(res, "wire has no associated channel")
            linker.getLogger.logWarning(res, "test warning")
        }


        for (ch <- channels) {
            if (ch.inputs.isEmpty || ch.outputs.isEmpty) {
                val points = ch.points.filter(!busWires.contains(_))

                if (ch.outputs.isEmpty)
                    linker.getLogger.logWarning(points.toSeq, "wirenet channel has no outputs")

                if (ch.inputs.isEmpty)
                    linker.getLogger.logWarning(points.toSeq, "wirenet channel has no inputs")
            }
            ch.allocateRegisters(linker)
        }

    }

    override def declareOperations(linker:ISELinker)
    {
        for (ch <- channels)
            ch.declareOperations(linker)
    }

    override def getInputRegister(p:Point) = pointToChannelMap.get(p) match {
        case Some(ch) => ch.getInputRegID(p)
        case _ => REG_ZERO
    }

    override def getOutputRegister(p:Point) = pointToChannelMap.get(p) match {
        case Some(ch) => ch.getOutputRegID
        case _ => REG_ZERO
    }

    override def getChannelStateRegisters(p:Point) = //get all registers for the channel at this point
        pointToChannelMap.get(p) match {
            case Some(ch) => ch.getAllRegisters
            case _ => Set(REG_ZERO)
        }
}

private object Node
{
    def startNodes(pos:Point, colourMask:Int):Seq[Node] =
    {
        val b = Seq.newBuilder[Node]
        for (i <- 0 until 16) if ((colourMask&1<<i) != 0)
            b += Node(pos, i)
        b.result()
    }
}

private case class Node(pos:Point, colour:Int)
{
    def -->(dir:Int, colourMask:Int):Seq[Node] =
    {
        val b = Seq.newBuilder[Node]
        val p = pos.offset(dir)
        for (i <- 0 until 16) if ((colourMask&1<<i) != 0)
            b += Node(p, i)
        b.result()
    }

    override def equals(that:Any) = that match {
        case n:Node => n.pos == pos && n.colour == colour
        case _ => false
    }
}