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

import scala.collection.mutable.ListBuffer

class ButtonICTile extends ICTile with TICTileAcquisitions with IRedwireICGate with TClientNetICTile with ISEGateTile
{
    val outputRegs = Array(REG_ZERO, REG_ZERO, REG_ZERO, REG_ZERO)
    var on = false
    var sched = -1L

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("on", on)
        tag.setLong("sched", sched)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        on = tag.getBoolean("on")
        sched = tag.getLong("sched")
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
        press()
    }

    def press()
    {
        if (!on) {
            on = true
            pushToRegisters()
            sendStateUpdate()
            sched = editor.network.getEditorWorld.getTotalWorldTime+20 //schedule depress
        }
    }

    def depress()
    {
        if (on) {
            on = false
            pushToRegisters()
            sendStateUpdate()
            sched = -1 //clear depress schedule
        }
    }

    override def update()
    {
        if (sched != -1 && editor.network.getEditorWorld.getTotalWorldTime >= sched)
            depress()
    }

    def sendStateUpdate()
    {
        writeStreamOf(1).writeBoolean(on)
    }

    override def getPartType = ICTileDefs.Button

    override def onAdded()
    {
        if (!editor.network.isRemote) notify(0xF)
    }

    override def onRemoved()
    {
        if (!editor.network.isRemote) notify(0xF)
    }

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
        for (r <- 0 until 4)
            outputRegs(r) = linker.findOutputRegister(pos, r)
    }

    override def declareOperations(linker:ISELinker)
    {
        on = false
        sched = -1
        if (editor != null)
            sendStateUpdate()
    }

    @SideOnly(Side.CLIENT)
    override def onClicked()
    {
        sendClientPacket()//data not necessary, only 1 reason to send this.
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = "Button"

    @SideOnly(Side.CLIENT)
    override def getPickOp = TileEditorOpDefs.Button.getOp


    @SideOnly(Side.CLIENT)
    override def buildRolloverData(buffer:ListBuffer[String])
    {
        super.buildRolloverData(buffer)
        buffer += ChatFormatting.GRAY+"state: "+(if (on) "on" else "off")
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float) =
    {
        RenderTileButton.prepairDynamic(this)
        RenderTileButton.render(ccrs, t, ortho)
    }
}

class OpButton extends SimplePlacementOp
{
    override def doPartRender(ccrs:CCRenderState, t:Transformation)
    {
        RenderTileButton.prepairInv()
        RenderTileButton.render(ccrs, t, true)
    }

    override def createPart = ICTileDefs.Button.createPart

    @SideOnly(Side.CLIENT)
    override def getOpName = "Button"
}