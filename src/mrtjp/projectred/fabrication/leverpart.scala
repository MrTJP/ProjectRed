/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataOutput, MCDataInput}
import codechicken.lib.vec.Transformation
import cpw.mods.fml.relauncher.{Side, SideOnly}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.EnumChatFormatting

class LeverICPart extends CircuitPart with TICAcquisitions with IPoweredCircuitPart with TClientNetCircuitPart
{
    var on = false

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("on", on)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        on = tag.getBoolean("on")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(on)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        on = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 => on = in.readBoolean()
        case _ => super.read(in, key)
    }

    override def readClientPacket(in:MCDataInput)
    {
        on = !on
        notify(0xF)
        sendStateUpdate()
    }

    def sendStateUpdate()
    {
        writeStreamOf(1).writeBoolean(on)
    }

    override def getPartType = CircuitPartDefs.Lever

    override def onAdded()
    {
        if (!world.network.isRemote) notify(0xF)
    }

    override def onRemoved()
    {
        if (!world.network.isRemote) notify(0xF)
    }

    override def rsOutputLevel(r:Int) = if (on) 255 else 0
    override def canConnectRS(r:Int) = true

    @SideOnly(Side.CLIENT)
    override def onClicked()
    {
        sendClientPacket()//data not necessary, only 1 reason to send this.
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = "Lever"

    @SideOnly(Side.CLIENT)
    override def getPickOp = CircuitOpDefs.Lever.getOp


    @SideOnly(Side.CLIENT)
    override def getRolloverData(detailLevel:Int) =
    {
        val b = Seq.newBuilder[String]
        if (detailLevel > 1) b += EnumChatFormatting.GRAY+"state: "+(if (on) "on" else "off")
        super.getRolloverData(detailLevel)++b.result()
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(t:Transformation, ortho:Boolean, frame:Float) =
    {
        RenderICLever.prepairDynamic(this)
        RenderICLever.render(t, ortho)
    }
}

class CircuitOpLever extends SinglePlacementOp
{
    override def doPartRender(t:Transformation)
    {
        RenderICLever.prepairInv()
        RenderICLever.render(t, true)
    }

    override def createPart = CircuitPartDefs.Lever.createPart

    @SideOnly(Side.CLIENT)
    override def getOpName = "Lever"
}