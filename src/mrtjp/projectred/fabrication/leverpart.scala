/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Transformation
import com.mojang.realmsclient.gui.ChatFormatting
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.SEIntegratedCircuit.REG_ZERO
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class LeverICPart extends ICTile with TICTileAcquisitions with IRedwireICGate with ISEGateTile with TClientNetICTile
{
    val outputRegs = Array(REG_ZERO, REG_ZERO, REG_ZERO, REG_ZERO)
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
        pushToRegisters()
//        notify(0xF) //we dont need to do this no mo
        sendStateUpdate()
    }

    def sendStateUpdate()
    {
        writeStreamOf(1).writeBoolean(on)
    }

    override def getPartType = ICTileDefs.Lever

    override def onAdded()
    {
        if (!editor.network.isRemote) notify(0xF)
    }

    override def onRemoved()
    {
        if (!editor.network.isRemote) notify(0xF)
    }

//    override def rsOutputLevel(r:Int) = if (on) 255 else 0
//    override def canConnectRS(r:Int) = true

    def pushToRegisters()
    {
        for (r <- 0 until 4)
            editor.simEngineContainer.simEngine.queueRegVal[Byte](outputRegs(r), if (on) 1 else 0)
        editor.simEngineContainer.simEngine.repropagate()
    }

    override def onRegistersChanged(regIDs:Set[Int]){} //we dont care if other registers change

    override def canOutputTo(r:Int) = true

    override def canInputFrom(r:Int) = false //this is output only 'gate'

    override def buildImplicitWireNet(r:Int):IWireNet = null //TODO

    override def allocateOrFindRegisters(linker:ISELinker)
    {
        val pos = new Point(x, y)
        for (r <- 0 until 4)
            outputRegs(r) = linker.findOutputRegister(pos, r)
    }

    override def declareOperations(linker:ISELinker){} //no gate op, we simply write to output registers directly

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
        if (detailLevel > 1) b += ChatFormatting.GRAY+"state: "+(if (on) "on" else "off")
        super.getRolloverData(detailLevel)++b.result()
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float) =
    {
        RenderICLever.prepairDynamic(this)
        RenderICLever.render(ccrs, t, ortho)
    }
}

class CircuitOpLever extends SimplePlacementOp
{
    override def doPartRender(ccrs:CCRenderState, t:Transformation)
    {
        RenderICLever.prepairInv()
        RenderICLever.render(ccrs, t, true)
    }

    override def createPart = ICTileDefs.Lever.createPart

    @SideOnly(Side.CLIENT)
    override def getOpName = "Lever"
}