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
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.ListBuffer

trait IICRedwireEmitter

trait IRedwireICPart extends IICRedwireEmitter

trait IInsulatedRedwireICPart extends IRedwireICPart
{
    def getInsulatedColour:Int
}

abstract class RedwireICTile extends WireICTile with IRedwireICPart
{
    private var stateRegister = 0
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

    override def canConnectTile(part:ICTile, r:Int) = part match {
        case re:IICRedwireEmitter => true
        case pc:IRedwireICGate => true
        case _ => false
    }

    override def discoverOverride(r:Int, part:ICTile) = part match {
        case gate:IRedwireICGate => gate.canConnectRS(rotFromStraight(r))
        case _ => false
    }

    override def isNetOutput(r:Int):Boolean =
    {
        if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if(gate.canInputFrom(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def isNetInput(r:Int):Boolean =
    {
        if (maskConnects(r)) getStraight(r) match {
            case gate:IRedwireICGate =>
                if (gate.canOutputTo(rotFromStraight(r))) return true
            case _ =>
        }
        false
    }

    override def cacheStateRegisters(linker:ISELinker)
    {
        stateRegister = linker.getWirenetOutputRegister(pos, 0)
    }

    override def onRegistersChanged(regIDs:Set[Int])
    {
        val oldSignal = signal
        signal = if (editor.simEngineContainer.simEngine.getRegVal[Byte](stateRegister) != 0)
                    255.toByte else 0

        if (oldSignal != signal)
            sendSignalUpdate()
    }

    @SideOnly(Side.CLIENT)
    override def buildRolloverData(buffer:ListBuffer[String])
    {
        super.buildRolloverData(buffer)

        import com.mojang.realmsclient.gui.ChatFormatting._
        buffer += GRAY+"state: "+(if (signal != 0) "high" else "low")
    }
}

class AlloyWireICTile extends RedwireICTile
{
    override def getPartType = ICTileDefs.AlloyWire

    override def getConnType(r:Int) = 0

    override def getInputColourMask(r:Int) = 0xFFFF
    override def getOutputColourMask(r:Int) = 0xFFFF

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderTileAlloyWire.prepairDynamic(this)
        RenderTileAlloyWire.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = "Alloy wire"

    @SideOnly(Side.CLIENT)
    override def getPickOp = TileEditorOpDefs.AlloyWire.getOp
}

class InsulatedWireICTile extends RedwireICTile with IInsulatedRedwireICPart
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

    override def canConnectTile(part:ICTile, r:Int) = part match
    {
        case b:IBundledCableICPart => true
        case iw:InsulatedWireICTile => iw.colour == colour
        case _ => super.canConnectTile(part, r)
    }

    override def getConnType(r:Int) = 1

    override def getInputColourMask(r:Int) = 1<<getInsulatedColour
    override def getOutputColourMask(r:Int) = 0

    override def getInsulatedColour = colour

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderTileInsulatedWire.prepairDynamic(this)
        RenderTileInsulatedWire.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = EnumColour.values()(colour&0xFF).name+" Insulated wire"

    @SideOnly(Side.CLIENT)
    override def getPickOp =
        TileEditorOpDefs.values(TileEditorOpDefs.WhiteInsulatedWire.ordinal+colour).getOp
}