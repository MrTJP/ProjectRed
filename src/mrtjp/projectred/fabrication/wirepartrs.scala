/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Transformation
import mrtjp.core.vec.Point
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

trait IICRedwireEmitter

trait IRedwireICPart extends IICRedwireEmitter

trait IInsulatedRedwireICPart extends IRedwireICPart
{
    def getInsulatedColour:Int
}

abstract class RedwireICPart extends WireICPart with IRedwireICPart
{
    private var stateRegisters = Set.empty[Int]
    var signal:Byte = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("signal", signal)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        signal = tag.getByte("signal")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(signal)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        signal = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 10 => signal = in.readByte()
        case _ => super.read(in, key)
    }

    def sendSignalUpdate()
    {
        writeStreamOf(10).writeByte(signal)
    }

//    override def onSignalUpdate()
//    {
//        super.onSignalUpdate()
//        writeStreamOf(10).writeByte(signal)
//    }

//    override def discoverOverride(r:Int, part:CircuitPart) = part match
//    {
//        case pow:IPoweredCircuitPart => pow.canConnectRS(rotFromStraight(r))
//        case _ => super.discoverOverride(r, part)
//    }

//    override def canConnectRS(r:Int) = true

//    override def getRedwireSignal(r:Int) = getSignal
//
//    override def getSignal = signal&0xFF
//    override def setSignal(sig:Int){signal = sig.toByte}

//    override def rsOutputLevel(r:Int) =
//        if (ICPropagator.redwiresProvidePower && maskConnects(r)) (signal&0xFF)+16
//        else 0

    override def canConnectPart(part:ICTile, r:Int) = part match {
        case re:IICRedwireEmitter => true
        case pc:IRedwireICGate => true
        case _ => false
    }

    override def discoverOverride(r:Int, part:ICTile) = part match {
        case gate:IRedwireICGate => gate.canConnectRS(rotFromStraight(r))
        case _ => false
    }

//    override def resolveSignal(part:Any, r:Int) = part match
//    {
//        case t:IRedwireICPart if t.diminishOnSide(r) => t.getRedwireSignal(r)-1
//        case t:IICRedwireEmitter => t.getRedwireSignal(r)
//        case t:IPoweredCircuitPart => t.rsOutputLevel(r)
//        case _ => 0
//    }
//
//    override def calculateSignal =
//    {
//        var s = 0
//        ICPropagator.redwiresProvidePower = false
//        def raise(sig:Int){ if (sig > s) s = sig }
//        for (r <- 0 until 4) if (maskConnects(r)) raise(calcSignal(r))
//        ICPropagator.redwiresProvidePower = true
//        s
//    }

    override def isNetOutput:Boolean =
    {
        for (r <- 0 until 4) if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if(gate.canInputFrom(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def isNetInput:Boolean =
    {
        for (r <- 0 until 4) if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if (gate.canOutputTo(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def cacheStateRegisters(linker:ISELinker)
    {
        stateRegisters = linker.getAllWireNetRegisters(new Point(x, y))
    }

    override def onRegistersChanged(regIDs:Set[Int])
    {
        try {
            val oldSignal = signal
            signal = if (stateRegisters.exists(reg =>
                editor.simEngineContainer.simEngine.getRegVal[Byte](reg) != 0))
                255.toByte else 0

            if (oldSignal != signal)
                sendSignalUpdate()
        } catch {
            case e:ClassCastException =>
                print("ERROR")

        }
    }

    @SideOnly(Side.CLIENT)
    override def getRolloverData(detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]

        import com.mojang.realmsclient.gui.ChatFormatting._
        data += GRAY+s"registers: $stateRegisters"
//        if (detailLevel >= 3) data += GRAY+"signal: 0x"+Integer.toHexString(signal&0xFF)
//        else if (detailLevel >= 2) data += GRAY+"state: "+(if (signal != 0) "high" else "low")

        super.getRolloverData(detailLevel)++data.result()
    }
}

class AlloyWireICPart extends RedwireICPart
{
    override def getPartType = ICTileDefs.AlloyWire

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderICAlloyWire.prepairDynamic(this)
        RenderICAlloyWire.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = "Alloy wire"

    @SideOnly(Side.CLIENT)
    override def getPickOp = CircuitOpDefs.AlloyWire.getOp
}

class InsulatedWireICPart extends RedwireICPart with IInsulatedRedwireICPart
{
    var colour:Byte = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("colour", colour)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        colour = tag.getByte("colour")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(colour)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        colour = in.readByte()
    }

    override def getPartType = ICTileDefs.InsulatedWire

//    override def resolveSignal(part:Any, r:Int) = part match
//    {
//        case b:IBundledCableICPart => (b.getBundledSignal.apply(colour)&0xFF)-1
//        case _ => super.resolveSignal(part, r)
//    }
//
    override def canConnectPart(part:ICTile, r:Int) = part match
    {
        case b:IBundledCableICPart => true
        case iw:InsulatedWireICPart => iw.colour == colour
        case _ => super.canConnectPart(part, r)
    }

    override def getInsulatedColour = colour

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderICInsulatedWire.prepairDynamic(this)
        RenderICInsulatedWire.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = EnumColour.values()(colour&0xFF).name+" Insulated wire"

    @SideOnly(Side.CLIENT)
    override def getPickOp =
        CircuitOpDefs.values(CircuitOpDefs.WhiteInsulatedWire.ordinal+colour).getOp
}