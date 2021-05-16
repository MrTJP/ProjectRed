/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.{IndexedVoxelShape, MultiIndexedVoxelShape, VoxelShapeCache}
import codechicken.lib.vec.{Cuboid6, Vector3}
import codechicken.microblock.FaceMicroFactory
import codechicken.multipart.util.PartRayTraceResult
import com.google.common.collect.ImmutableSet
import mrtjp.core.vec.VecLib
import mrtjp.projectred.api.{IBundledEmitter, IBundledTile, IConnectable, IScrewdriver}
import mrtjp.projectred.core.BundledCommons._
import mrtjp.projectred.core.TFaceOrient._
import mrtjp.projectred.core.{BundledSignalsLib, Configurator, TFaceBundledAquisitions}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.Direction
import net.minecraft.util.math.shapes.VoxelShape

import java.util.Random
import scala.jdk.CollectionConverters._

trait TBundledGatePart extends GatePart with TFaceBundledAquisitions with IBundledEmitter
{
//    def getLogicBundled = getLogic[TBundledGateLogic[TBundledGatePart]]

    abstract override def discoverStraightOverride(absDir:Int) =
    {
        val pos = posOfStraight(absDir)
        world.getBlockEntity(pos) match {
            case t:IBundledTile => t.canConnectBundled(absDir^1)
            case _ if BundledSignalsLib.canConnectBundledViaInteraction(world, pos, Direction.values()(absDir^1)) => true
            case _ => super.discoverStraightOverride(absDir)
        }
    }

    override def getBundledSignal(r:Int):Array[Byte] = {
        val ir = toInternal(r)
        if ((bundledOutputMask(shape)&1<<ir) != 0) getBundledOutput(ir)
        else null
    }

    def getBundledInput(r:Int):Array[Byte] = {
        val ar = toAbsolute(r)
        if (maskConnectsCorner(ar)) calcCornerArray(ar)
        else if (maskConnectsStraight(ar)) calcStraightArray(ar)
        else if (maskConnectsInside(ar)) calcInternalArray(ar)
        else null
    }

    override def resolveArray(part:Any, r:Int):Array[Byte] = part match {
        case be:IBundledEmitter => be.getBundledSignal(r)
        case _ => null
    }

    abstract override def gateLogicCanConnectTo(part:IConnectable, r:Int):Boolean = part match {
        case be:IBundledEmitter => canConnectBundled(r)
        case _ => super.gateLogicCanConnectTo(part, r)
    }

    def canConnectBundled(r:Int):Boolean = ((bundledInputMask(shape)|bundledOutputMask(shape))&1<<r) != 0

    def bundledInputMask(shape:Int) = 0
    def bundledOutputMask(shape:Int) = 0

    def getBundledOutput(r:Int):Array[Byte] = null
}

abstract class BundledGatePart(gateType:GateType) extends RedstoneGatePart(gateType) with TBundledGatePart
//
//object BundledGateLogic
//{
//    import mrtjp.projectred.integration.GateType._
//    def create(gate:BundledGatePart, gateType:GateType) = gateType match
//    {
//        case BUS_TRANSCEIVER => new BusTransceiver(gate)
//        case BUS_RANDOMIZER => new BusRandomizer(gate)
//        case BUS_CONVERTER => new BusConverter(gate)
//        case BUS_INPUT_PANEL => new BusInputPanel(gate)
//        case SEGMENT_DISPLAY => new SegmentDisplay(gate)
//        case _ => throw new IllegalArgumentException("Invalid gateType: "+gateType)
//    }
//}

//abstract class BundledGateLogic(val gate:BundledGatePart) extends RedstoneGateLogic[BundledGatePart] with TBundledGateLogic[BundledGatePart] with TComplexGateLogic[BundledGatePart]

class BusTransceiver extends BundledGatePart(GateType.BUS_TRANSCEIVER)
{
    var input0, output0, input2, output2:Array[Byte] = null

    override def bundledOutputMask(shape:Int) = 5
    override def bundledInputMask(shape:Int) = 5
    override def outputMask(shape:Int) = 0
    override def inputMask(shape:Int) = 10

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        saveSignal(tag, "in0", input0)
        saveSignal(tag, "out0", output0)
        saveSignal(tag, "in2", input2)
        saveSignal(tag, "out2", output2)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        input0 = loadSignal(tag, "in0")
        input2 = loadSignal(tag, "in2")
        output0 = loadSignal(tag, "out0")
        output2 = loadSignal(tag, "out2")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeInt(packClientData)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        unpackClientData(packet.readInt)
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => unpackClientData(packet.readInt())
        case _ => super.read(packet, key)
    }

    def sendClientUpdate():Unit = {
        sendUpdate(11, _.writeInt(packClientData))
    }

    def packClientData:Int = packDigital(output0)|packDigital(output2)<<16

    def unpackClientData(packed:Int):Unit = {
        output0 = unpackDigital(output0, packed&0xFFFF)
        output2 = unpackDigital(output2, packed>>>16)
    }

    override def getBundledOutput(r:Int):Array[Byte] = if (r == 0) output0 else output2

    def calcBundledInput(r:Int):Array[Byte] = raiseSignal(copySignal(getBundledInput(r)), getBundledOutput(r)) //OR'd w/ output

    override def gateLogicOnChange():Unit = {
        var inputChanged = false

        val oldInput = state&0xF
        val newInput = getInput(10)
        if (oldInput != newInput)
        {
            setState(state&0xF0|newInput)
            inputChanged = true
        }

        val newInput0 = calcBundledInput(0)
        if (!signalsEqual(input0, newInput0))
        {
            input0 = newInput0
            inputChanged = true
        }

        val newInput2 = calcBundledInput(2)
        if (!signalsEqual(input2, newInput2))
        {
            input2 = newInput2
            inputChanged = true
        }

        if (inputChanged) onInputChange()
        if (!signalsEqual(output0, calcBundledOutput(0)) || !signalsEqual(output2, calcBundledOutput(2))) scheduleTick(2)
    }

    def calcBundledOutput(r:Int):Array[Byte] = {
        var input = state&0xF
        if (shape == 1) input = flipMaskZ(input)

        if (r == 0) {
            if ((input&2) != 0) input2 else null
        } else if (r == 2) {
            if ((input&8) != 0) input0 else null
        } else
            null
    }

    override def gateLogicOnScheduledTick():Unit = {
        output0 = calcBundledOutput(0)
        output2 = calcBundledOutput(2)
        gateLogicOnChange()
        onOutputChange(5)
        sendClientUpdate()
    }

    override def gateLogicCycleShape():Boolean = {
        setShape(shape^1)
        true
    }

    override def getLightValue:Int = 0
}

class BusRandomizer extends BundledGatePart(GateType.BUS_RANDOMIZER)
{
    val rand = new Random

    var unpackedOut = new Array[Byte](16)
    var output = 0
    var mask = 0xFFFF

    override def bundledOutputMask(shape:Int) = 5
    override def inputMask(shape:Int) = 10
    override def outputMask(shape:Int) = 0

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putShort("in", (mask&0xFFFF).asInstanceOf[Short])
        tag.putShort("out", (output&0xFFFF).asInstanceOf[Short])
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        mask = tag.getShort("in")
        output = tag.getShort("out")
        unpackedOut = unpackDigital(unpackedOut, output)
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeShort(output)
        packet.writeShort(mask)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        output = packet.readUShort()
        mask = packet.readUShort()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => output = packet.readUShort()
        case 12 => mask = packet.readUShort()
        case _ => super.read(packet, key)
    }

    def sendOutUpdate():Unit = {
        sendUpdate(11, _.writeShort(output))
    }

    def sendMaskUpdate():Unit = {
        sendUpdate(12, _.writeShort(mask))
    }

    override def gateLogicOnChange():Unit = {
        var inputChanged = false
        val oldInput = state&0xF
        val newInput = getInput(10)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            inputChanged = true
        }

        var newMask = packDigital(getBundledInput(2))
        if (newMask == 0) newMask = 0xFFFF
        if (mask != newMask) {
            mask = newMask
            inputChanged = true
            sendMaskUpdate()
        }

        if (inputChanged) onInputChange()
        if (newInput != 0) scheduleTick(2)
    }

    override def gateLogicOnScheduledTick():Unit = {
        val oldOut = output
        output = if ((state&0xF) != 0) if (shape == 0) calc1BitOut else calcNBitOut else oldOut
        if (oldOut != output) {
            unpackedOut = unpackDigital(unpackedOut, output)
            onOutputChange(1)
            sendOutUpdate()
        }
        gateLogicOnChange()
    }

    def calc1BitOut:Int = {
        val high = Integer.bitCount(mask)
        val n = rand.nextInt(high)
        var v = 0
        for (i <- 0 until 16) if ((mask&1<<i) != 0 && {v+=1; v-1} == n) return 1<<i
        0
    }

    def calcNBitOut:Int = {
        var out = 0
        for (i <- 0 until 16) if ((mask&1<<i) != 0 && rand.nextBoolean) out |= 1<<i
        out
    }

    override def getBundledOutput(r:Int):Array[Byte] = if (r == 0) unpackedOut else null

    override def gateLogicCycleShape():Boolean = {
        setShape(shape^1)
        true
    }

    override def getLightValue:Int = 0
}

class BusConverter extends BundledGatePart(GateType.BUS_CONVERTER)
{
    var bIn, bOut = 0
    var rsIn, rsOut = 0
    var bOutUnpacked:Array[Byte] = null

    override def bundledOutputMask(shape:Int):Int = if (shape == 0) 1 else 0
    override def bundledInputMask(shape:Int):Int = if (shape == 0) 0 else 1
    override def outputMask(shape:Int):Int = if (shape == 0) 10 else 14
    override def inputMask(shape:Int):Int = if (shape == 0) 4 else 0

    def setBOut(newBOut:Int):Unit = {
        if (bOut != newBOut) {
            bOut = newBOut
            bOutUnpacked = unpackDigital(bOutUnpacked, bOut)
        }
    }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putByte("in", rsIn.toByte)
        tag.putByte("out", rsOut.toByte)
        tag.putByte("in0", bIn.toByte)
        tag.putByte("out0", bOut.toByte)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        rsIn = tag.getByte("in")
        rsOut = tag.getByte("out")
        bIn = tag.getByte("in0")
        setBOut(tag.getByte("out0"))
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeShort(packClientData)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        unpackClientData(packet.readUShort)
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => unpackClientData(packet.readUShort)
        case _ => super.read(packet, key)
    }

    def sendClientUpdate():Unit = {
        sendUpdate(11, _.writeShort(packClientData))
    }

    def packClientData:Int = rsIn|rsOut<<4|mostSignificantBit(bIn)<<8|mostSignificantBit(bOut)<<12

    def unpackClientData(data:Int):Unit = {
        rsIn = data&0xF
        rsOut = data>>4&0xF
        bIn = 1<<(data>>8&0xF)
        setBOut(1<<(data>>12&0xF))
    }

    override def getOutput(r:Int):Int =
        if (shape != 0 && r == 2) rsOut else if ((state&0x10 << r) != 0) 15 else 0

    override def getBundledOutput(r:Int):Array[Byte] = if (shape == 0 && r == 0) bOutUnpacked else null

    override def gateLogicOnChange():Unit = {
        var changed = false

        val oldRSIn = rsIn
        rsIn = if (shape == 0) getRedstoneInput(2)/17 else 0
        if (oldRSIn != rsIn) changed = true

        val oldBIn = bIn
        bIn = if (shape == 0) 0 else packDigital(getBundledInput(0))
        if (oldBIn != bIn) changed = true

        if (changed) {
            onInputChange()
            scheduleTick(2)
            sendClientUpdate()
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        var changeMask = 0

        val oldBOut = bOut
        setBOut(if (shape == 0) 1<<rsIn else 0)
        if (oldBOut != bOut) changeMask |= 1

        val oldRSOut = rsOut
        rsOut = if (shape == 0) 0 else mostSignificantBit(bIn)
        if (rsOut != oldRSOut) changeMask |= 4

        val oldOut2 = state>>4
        val newOut2 = if ((if (shape == 0) rsIn else bIn) != 0) 10 else 0
        if (oldOut2 != newOut2) {
            setState(state&0xF|newOut2<<4)
            changeMask |= 10
        }

        if (changeMask != 0) {
            onOutputChange(changeMask)
            sendClientUpdate()
        }

        gateLogicOnChange()
    }

    /**
     * Shape 0: Analog to Digital (RS to Bundled)
     * Shape 1: Digital to Analog (Bundled to RS)
     */
    override def gateLogicCycleShape():Boolean = {
        setShape(shape^1)
        true
    }

    override def getLightValue:Int = 0
}

object BusInputPanel
{
    val unpressed = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 3, 13), new Vector3(-0.25, 0, -0.25))
    val pressed = VecLib.buildCubeArray(4, 4, new Cuboid6(3, 1, 3, 13, 2.5, 13), new Vector3(-0.25, 0, -0.25))

    val sBoxes = Array.ofDim[Cuboid6](6*4, 65536)
    val sShapes = Array.ofDim[MultiIndexedVoxelShape](6*4, 65536)

    def getOrCreateOutline(orient:Int, pressMask:Int):MultiIndexedVoxelShape = {
        var shape = sShapes(orient)(pressMask)
        if (shape == null) {
            val t = VecLib.orientT(orient)
            val shapeBuilder = Set.newBuilder[IndexedVoxelShape]

            //Base platform box
            val baseBounds = FaceMicroFactory.aBounds(0x10|0).copy.apply(t)
            val baseShape = VoxelShapeCache.getShape(baseBounds)
            shapeBuilder += new IndexedVoxelShape(baseShape, -1)
            for (i <- 0 until 16) {
                val bounds = (if ((pressMask&1<<i) != 0) pressed else unpressed)(i).copy.apply(t)
                shapeBuilder += new IndexedVoxelShape(VoxelShapeCache.getShape(bounds), i)
            }

            shape = new MultiIndexedVoxelShape(ImmutableSet.copyOf(shapeBuilder.result().asJava))
            sShapes(orient)(pressMask) = shape
        }
        shape
    }
}

class BusInputPanel extends BundledGatePart(GateType.BUS_INPUT_PANEL)
{
    var pressMask = 0

    var bOut = 0
    var bOutUnpack:Array[Byte] = null

    override def bundledOutputMask(shape:Int) = 4
    override def bundledInputMask(shape:Int) = 0
    override def outputMask(shape:Int) = 0
    override def inputMask(shape:Int) = 1

    def setBOut(newBOut:Int):Unit = {
        if (bOut != newBOut) {
            bOut = newBOut
            bOutUnpack = unpackDigital(bOutUnpack, bOut)
        }
    }

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putShort("press", pressMask.toShort)
        tag.putShort("mask", bOut.toShort)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        pressMask = tag.getShort("press")
        setBOut(tag.getShort("mask")&0xFFFF)
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeShort(pressMask)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        pressMask = packet.readUShort
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => pressMask = packet.readShort()
        case _ => super.read(packet, key)
    }

    def sendClientUpdate():Unit = {
        sendUpdate(11, _.writeShort(pressMask))
    }

    override def getOutput(r:Int):Int = if ((state&0x10<<r) != 0) 15 else 0

    override def getBundledOutput(r:Int):Array[Byte] = bOutUnpack

    override def gateLogicOnChange():Unit = {
        var inputChanged = false

        val oldInput = state&0xF
        val newInput = getInput(1)
        if (oldInput != newInput) {
            setState(state&0xF0|newInput)
            inputChanged = true
        }

        if ((state&1) != 0) pressMask = 0

        val oldBInput = bOut
        val newBInput = pressMask
        if (oldBInput != newBInput) inputChanged = true

        if (inputChanged) {
            onInputChange()
            scheduleTick(2)
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        var outputChanged:Boolean = false

        val oldBOut = bOut
        val newBOut = pressMask
        if (oldBOut != newBOut) {
            setBOut(pressMask)
            outputChanged = true
            sendClientUpdate()
        }

        if (outputChanged) onOutputChange(bundledOutputMask(shape))
        gateLogicOnChange()
    }

    override def getOutlineShape:VoxelShape =
        BusInputPanel.getOrCreateOutline(orientation&0xFF, pressMask&0xFFFF)


    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (!held.isEmpty && held.getItem.isInstanceOf[IScrewdriver])
            false
        else if (hit.subHit > -1) {
            if (!world.isClientSide) {
                pressMask ^= (1<<hit.subHit)
                gateLogicOnChange()
            }
            true
        } else
            false
    }
}

class SegmentDisplay extends BundledGatePart(GateType.SEGMENT_DISPLAY)
{
    var bInH = 0
    var colour:Byte = EnumColour.RED.ordinal.toByte

    override def bundledInputMask(shape:Int) = 1

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putByte("in", bInH.toByte)
        tag.putByte("col", colour)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        bInH = tag.getByte("in")
        colour = tag.getByte("col")
    }

    override def writeDesc(packet:MCDataOutput):Unit = {
        super.writeDesc(packet)
        packet.writeByte(bInH)
        packet.writeByte(colour)
    }

    override def readDesc(packet:MCDataInput):Unit = {
        super.readDesc(packet)
        bInH = packet.readByte()
        colour = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int):Unit = key match {
        case 11 => bInH = packet.readByte()
        case 12 =>
            colour = packet.readByte()
            if (Configurator.staticGates) tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendClientUpdate():Unit = {
        sendUpdate(11, _.writeByte(bInH))
    }

    def sendColourUpdate():Unit ={
        sendUpdate(12, _.writeByte(colour))
    }

    override def gateLogicCycleShape():Boolean = {
        setShape(shape^1)
        true
    }

    override def gateLogicOnChange():Unit = {
        val newBIn = packDigital(getBundledInput(0))
        if ((bInH<<8|state) != newBIn) {
            setState(newBIn&0xFF)
            bInH = newBIn>>8
            onInputChange()
            sendClientUpdate()
            scheduleTick(2)
        }
    }

    override def gateLogicOnScheduledTick():Unit = {
        gateLogicOnChange()
    }

    override def gateLogicActivate(player:PlayerEntity, held:ItemStack, hit:PartRayTraceResult):Boolean = {
        if (!held.isEmpty) {
            val c = EnumColour.fromDyeStack(held)
            if (c != null && c.ordinal != (colour&0xFF) && c != EnumColour.BLACK) {
                if (!world.isClientSide) {
                    colour = c.ordinal.toByte
                    sendColourUpdate()
                }
                true
            } else
                false
        } else
            false
    }
}
