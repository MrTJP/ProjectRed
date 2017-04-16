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
import scala.collection.mutable.{Map => MMap, Set => MSet}
import SEIntegratedCircuit._

abstract class WireICPart extends ICTile with TConnectableICPart with ISEWireTile with IErrorICTile
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
        val wireNet = new WireNet(editor, Point(x, y))
        wireNet.calculateNetwork()
        wireNet
    }

    override def postErrors =
    {
        Integer.bitCount(connMap&0xF) match
        {
            case 0 => ("Unreachable wiring", EnumColour.RED.ordinal)
            case 1 => ("Useless wiring", EnumColour.YELLOW.ordinal)
            case _ => null
        }
    }
}

class WireNetChannel(mask:Int)
{
    val inputs = MSet[Point]()
    val outputs = MSet[Point]()

    private val inputsToRegIDMap = MMap[Point, Int]()
    private var outputRegID = -1

    def allocateRegisters(linker:ISELinker)
    {
        if (false && (outputs.isEmpty || inputs.isEmpty)) { //invalid channel not being driven or used TODO for rendering
            for (s <- inputs)
                inputsToRegIDMap += s -> REG_ZERO
            outputRegID = REG_ZERO
        } else {
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

class WireNet(ic:ICTileMapEditor, p:Point) extends IWireNet
{
    val points = MSet[Point]()

    private val channels = MSet[WireNetChannel]()
    private val pointToColourMap = MMap[Point, Int]() //any non-bus point in net to colour
    private val colourToChannelMap = MMap[Int, WireNetChannel]() //any colour 0-15 to its channel
    private val ioPointToChannelMap = MMap[Point, WireNetChannel]() //io point to channel that it drives/is driven by

    private val busWires = MSet[Point]()
    private val portWires = MSet[Point]()
    private val singleWires = MSet[Point]()

    private val inputs = MSet[Point]()
    private val outputs = MSet[Point]()

    private def searchForWireNet(open:Seq[Point]):Unit = open match {
        case Seq() =>
        case Seq(next, rest@_*) => ic.getPart(next) match {
            case w:WireICPart =>
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
                //points += next //dont add non-wires
                searchForWireNet(rest)
        }
    }

    private def searchForClosestPortColour(open:Seq[Point], closed:Set[Point] = Set()):Int = open match {
        case Seq() => 0 //default to white port if this is a red alloy only net
        case Seq(next, rest@_*) => ic.getPart(next) match {
            //TODO needs case for bundled
            case w:WireICPart with IInsulatedRedwireICPart =>
                w.getInsulatedColour
            case w:WireICPart with IRedwireICPart =>
                val upNext = Seq.newBuilder[Point]
                for (r <- 0 until 4) if (w.maskConnects(r)) {
                    val p = next.offset(r)
                    if (!closed(p) && !open.contains(p)) upNext += p
                }
                searchForClosestPortColour(rest ++ upNext.result(), closed + next)
            case _ =>
                searchForClosestPortColour(rest, closed + next)
        }
    }

    private def searchForPortGroup(colour:Int):Set[Int] =
    {
        val wires = portWires.filter { ic.getPart(_) match {
            case w:WireICPart with IInsulatedRedwireICPart => w.getInsulatedColour == colour
            case _ => false
        }
        }.toSeq

        def iterate(open:Seq[Point], closed:Set[Point] = Set(), ports:Set[Int] = Set(colour)):Set[Int] = open match {
            case Seq() => ports
            case Seq(next, rest@_*) => ic.getPart(next) match {
                case w:WireICPart with IRedwireICPart =>
                    val upNext = Seq.newBuilder[Point]
                    for (r <- 0 until 4) if (w.maskConnects(r)) {
                        val p = next.offset(r)
                        if (!closed(p) && !open.contains(p)) upNext += p
                    }
                    iterate(rest ++ upNext.result(), closed + next, if (w.isInstanceOf[IInsulatedRedwireICPart])
                        ports + w.asInstanceOf[IInsulatedRedwireICPart].getInsulatedColour else ports)
                case _ =>
                    iterate(rest, closed + next, ports)
            }
        }

        iterate(wires)
    }

    private def getMaskForPorts(ports:Set[Int]):Int = ports.foldLeft(0) { (p, mask) => mask|1<<p }

    def calculateNetwork()
    {
        searchForWireNet(Seq(p))

        //map every non-bus wire to a colour
        for (p <- singleWires ++ portWires) {
            val colour = searchForClosestPortColour(Seq(p))
            pointToColourMap += p -> colour
        }

        //create channels for all inputs and outputs
        def getOrCreateChannel(p:Point):WireNetChannel = {
//            val colour = searchForClosestPortColour(Seq(i))
            val colour = pointToColourMap(p)
            colourToChannelMap.get(colour) match {
                case Some(ch) => ch
                case _ =>
                    val colours = searchForPortGroup(colour)
                    val ch = new WireNetChannel(getMaskForPorts(colours))
                    channels += ch
                    for (c <- colours)
                        colourToChannelMap += c -> ch
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

        for (ch <- channels)
            for (p <- ch.outputs ++ ch.inputs)
                ioPointToChannelMap += p -> ch
    }

    override def allocateRegisters(linker:ISELinker)
    {
        for (ch <- channels)
            ch.allocateRegisters(linker)
    }

    override def declareOperations(linker:ISELinker)
    {
        for (ch <- channels)
            ch.declareOperations(linker)
    }

    override def getInputRegister(p:Point) = ioPointToChannelMap(p).getInputRegID(p)

    override def getOutputRegister(p:Point) = ioPointToChannelMap(p).getOutputRegID

    override def getChannelStateRegisters(p:Point) = //get all registers for the channel at this point
    {
        val colour = pointToColourMap(p)
        if (colourToChannelMap.contains(colour)) {
            val channel = colourToChannelMap(colour)
            channel.getAllRegisters
        } else
            Set(REG_ZERO)
    }
}