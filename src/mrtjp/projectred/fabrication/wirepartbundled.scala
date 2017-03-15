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
import mrtjp.projectred.fabrication.IWireICPart._
import mrtjp.projectred.transmission.BundledCommons._
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

trait IBundledCableICPart extends IWireICPart with IICBundledEmitter
{
    def getBundledSignal:Array[Byte]
    def calculateSignal:Array[Byte]
    def setSignal(sig:Array[Byte])

    def getBundledColour:Int
}

trait IICBundledEmitter
{
    def getBundledSignal(r:Int):Array[Byte]
}

class BundledCableICPart extends WireICPart with TICBundledAcquisitions with IBundledCableICPart
{
    var signal = new Array[Byte](16)
    var colour:Byte = -1

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByteArray("signal", signal)
        tag.setByte("colour", colour)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        signal = tag.getByteArray("signal")
        colour = tag.getByte("colour")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(colour)
        out.writeShort(packDigital(signal))
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        colour = in.readByte()
        signal = unpackDigital(signal, in.readShort())
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 10 => signal = unpackDigital(signal, in.readUShort())
        case _ => super.read(in, key)
    }

    override def onSignalUpdate()
    {
        super.onSignalUpdate()
        writeStreamOf(10).writeShort(packDigital(signal))
    }

    override def getPartType = CircuitPartDefs.BundledCable

    override def canConnectPart(part:CircuitPart, r:Int) = part match
    {
        case b:IBundledCableICPart => b.getBundledColour == -1 || colour == -1 || b.getBundledColour == colour
        case ins:IInsulatedRedwireICPart => true
        case be:IICBundledEmitter => true
        case _ => false
    }

    protected var propagatingMask = 0xFFFF
    override def updateAndPropagate(prev:CircuitPart, mode:Int)
    {
        import mrtjp.projectred.transmission.BundledCommons._
        val mask = getUpdateMask(prev, mode)
        if (mode == DROPPING && isSignalZero(getBundledSignal, mask)) return

        val newSignal = calculateSignal
        applyChangeMask(getBundledSignal, newSignal, mask)

        propagatingMask = mask
        if (dropSignalsLessThan(getBundledSignal, newSignal))
        {
            if (!isSignalZero(newSignal, mask)) ICPropagator.propagateAnalogDrop(this)
            propagate(prev, DROPPING)
        }
        else if (!signalsEqual(getBundledSignal, newSignal))
        {
            setSignal(newSignal)
            if (mode == DROPPING) propagate(null, RISING)
            else propagate(prev, RISING)
        }
        else if (mode == DROPPING) propagateTo(prev, RISING)
        else if (mode == FORCE) propagate(prev, FORCED)
        propagatingMask = 0xFFFF
    }

    def getUpdateMask(from:CircuitPart, mode:Int) = from match
    {
        case ins:IInsulatedRedwireICPart => 1<<ins.getInsulatedColour
        case b:IBundledCableICPart if mode == DROPPING =>
            var m = 0
            val osignal = b.getBundledSignal
            for (i <- 0 until 16) if (osignal(i) == 0) m |= 1<<i
            m
        case b:IBundledCableICPart if mode == RISING =>
            var m = 0
            val osignal = b.getBundledSignal
            for (i <- 0 until 16) if ((osignal(i)&0xFF) > (getBundledSignal.apply(i)&0xFF)) m |= 1<<i
            m
        case _ => 0xFFFF
    }

    override def resolveArray(part:Any, r:Int) =
    {
        part match
        {
            case b:IBundledCableICPart =>
                val osig = b.getBundledSignal
                for (i <- 0 until 16) if ((osig(i)&0xFF)-1 > (tmpSignal(i)&0xFF))
                    tmpSignal(i) = (osig(i)-1).toByte
            case i:IInsulatedRedwireICPart =>
                val s = i.getRedwireSignal(r)-1
                if (s > (tmpSignal(i.getInsulatedColour)&0xFF))
                    tmpSignal(i.getInsulatedColour) = s.toByte
            case b:IICBundledEmitter => raiseSignal(tmpSignal, b.getBundledSignal(r))
            case _ =>
        }
        tmpSignal
    }

    private val tmpSignal = new Array[Byte](16)
    private def tmpSignalClear()
    {
        for (i <- 0 until 16) tmpSignal(i) = 0.toByte
    }

    override def propagateTo(part:CircuitPart, mode:Int) =
    {
        def shouldPropogate(part:CircuitPart, mode:Int) = part match
        {
            case ins:IInsulatedRedwireICPart => (propagatingMask&1<<ins.getInsulatedColour) != 0
            case _ => true
        }

        if (shouldPropogate(part, mode)) super.propagateTo(part, mode)
        else true
    }

    override def setSignal(sig:Array[Byte])
    {
        if (sig == null) signal.transform(_ => 0.toByte)
        else for (i <- 0 until 16) signal(i) = sig(i)
    }

    override def getBundledSignal = signal

    override def getBundledSignal(r:Int) = if (maskConnects(r)) getBundledSignal else null

    override def getBundledColour = colour

    override def calculateSignal =
    {
        tmpSignalClear()
        for (r <- 0 until 4) if (maskConnects(r)) calcArray(r)
        tmpSignal
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderICBundledCable.prepairDynamic(this)
        RenderICBundledCable.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = (if (colour != -1) EnumColour.values()(colour&0xFF).name+" " else "")+"Bundled cable"

    @SideOnly(Side.CLIENT)
    override def getPickOp = CircuitOpDefs.values(CircuitOpDefs.NeutralBundledCable.ordinal+colour+1).getOp

    @SideOnly(Side.CLIENT)
    override def getRolloverData(detailLevel:Int) =
    {
        val data = Seq.newBuilder[String]

        import com.mojang.realmsclient.gui.ChatFormatting._
        val sig = packDigital(signal)
        if (detailLevel >= 3) data += GRAY+"signal: 0x"+Integer.toHexString(sig)
        else if (detailLevel >= 2) data += GRAY+"state: "+(if (sig != 0) "active" else "inactive")

        super.getRolloverData(detailLevel)++data.result()
    }
}