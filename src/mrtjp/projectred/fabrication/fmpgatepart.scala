/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec._
import mrtjp.core.math.MathLib
import mrtjp.projectred.fabrication.IIOCircuitPart._
import mrtjp.projectred.integration
import mrtjp.projectred.integration._
import mrtjp.projectred.transmission.BundledCommons._
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import org.lwjgl.opengl.GL11

class CircuitGatePart extends RedstoneGatePart with TBundledGatePart with TComplexGatePart
{
    private var logic:CircuitGateLogic = null

    override def getLogic[T] = logic.asInstanceOf[T]
    def getLogicIC = getLogic[CircuitGateLogic]

    private var itemTag:NBTTagCompound = null

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setTag("itemTag", itemTag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        itemTag = tag.getCompoundTag("itemTag")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeNBTTagCompound(itemTag)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        itemTag = packet.readNBTTagCompound()
    }

    override def preparePlacement(player:EntityPlayer, pos:BlockCoord, side:Int, meta:Int)
    {
        super.preparePlacement(player, pos, side, meta)
        itemTag = player.getHeldItem.getTagCompound
        CircuitGateLogic.constructICLogic(logic, player.getHeldItem)
    }

    override def getItem =
    {
        val stack = getGateDef.makeStack
        stack.setTagCompound(itemTag)
        stack
    }

    override def assertLogic()
    {
        if (logic == null)
            logic = new CircuitGateLogic(this)
    }

    override def getType = "pr_icgate"
}

class CircuitGateLogic(gate:CircuitGatePart) extends RedstoneGateLogic[CircuitGatePart] with TBundledGateLogic[CircuitGatePart] with TComplexGateLogic[CircuitGatePart] with SimulatedWorldCircuit
{
    val ic = new IntegratedCircuit
    ic.network = this
    ic.outputChangedDelegate = {() => if (!gate.world.isRemote) scheduledTick(gate)}

    var ri = 0
    var ro = 0
    var bi = 0
    var bo = 0

    var connmodes = Array(NoConn, NoConn, NoConn, NoConn)
    var name = "untitled"

    override def save(tag:NBTTagCompound)
    {
        ic.save(tag)
        tag.setShort("masks", CircuitGateLogic.packIO(ri, ro, bi, bo).toShort)
        tag.setShort("cmode", CircuitGateLogic.packConnModes(connmodes).toShort)
    }

    override def load(tag:NBTTagCompound)
    {
        ic.load(tag)
        val (ri0, ro0, bi0, bo0) = CircuitGateLogic.unpackIO(tag.getShort("masks"))
        ri = ri0; ro = ro0; bi = bi0; bo = bo0
        connmodes = CircuitGateLogic.unpackConnModes(tag.getShort("cmode"))
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeShort(CircuitGateLogic.packIO(ri, ro, bi, bo))
        packet.writeShort(CircuitGateLogic.packConnModes(connmodes))
        packet.writeString(ic.name)
    }

    override def readDesc(packet:MCDataInput)
    {
        val (ri0, ro0, bi0, bo0) = CircuitGateLogic.unpackIO(packet.readShort())
        ri = ri0; ro = ro0; bi = bi0; bo = bo0
        connmodes = CircuitGateLogic.unpackConnModes(packet.readShort())
        name = packet.readString()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case _ => super.read(packet, key)
    }

    override def getIC = ic
    override def getWorld = gate.world
    override def markDirty(){ if (!getWorld.isRemote) gate.tile.markDirty() }

    override def inputMask(shape:Int) = ri
    override def outputMask(shape:Int) = ro
    override def bundledInputMask(shape:Int) = bi
    override def bundledOutputMask(shape:Int) = bo

    override def onTick(gate:CircuitGatePart)
    {
        if (!gate.world.isRemote) ic.tick()
    }

    override def onChange(gate:CircuitGatePart)
    {
        var cmask = 0
        for (r <- 0 until 4)
            if ((inputMask(gate.shape)&1<<r) != 0 && checkRSInputChange(r)) cmask |= 1<<r
            else if ((bundledInputMask(gate.shape)&1<<r) != 0 && checkBundledInputChange(r)) cmask |= 1<<r

        if (cmask != 0)
        {
            gate.setState(gate.state&0xF0|getRSInputs)
            gate.onInputChange()
            ic.onInputChanged(cmask)
        }
    }

    override def scheduledTick(gate:CircuitGatePart)
    {
        val oldOutput = gate.state>>4
        val newOutput = getRSOutputs
        if (oldOutput != newOutput)
        {
            gate.setState(newOutput<<4|gate.state&0xF)
            gate.onOutputChange(oldOutput^newOutput)
        }
        onChange(gate)
    }

    def checkRSInputChange(r:Int):Boolean =
    {
        if ((ri&1<<r) == 0) return false
        val oldInput = ic.iostate(r)&0xFFFF
        val newInput = 1<<(gate.getRedstoneInput(r)/17)
        if (newInput != oldInput)
        {
            ic.setInput(r, newInput)
            true
        }
        else false
    }

    def checkBundledInputChange(r:Int):Boolean =
    {
        if ((bi&1<<r) == 0) return false
        val oldInput = ic.iostate(r)&0xFFFF
        val newInput = packDigital(gate.getBundledInput(r))
        if (newInput != oldInput)
        {
            ic.setInput(r, newInput)
            true
        }
        else false
    }

    def getRSInputs =
    {
        var m = 0
        for (r <- 0 until 4) if ((ic.iostate(r)&0xFFFE) != 0) m |= 1<<r
        m
    }

    def getRSOutputs =
    {
        var m = 0
        for (r <- 0 until 4) if (((ic.iostate(r)>>16)&0xFFFE) != 0) m |= 1<<r
        m
    }

    override def getOutput(gate:CircuitGatePart, r:Int) =
    {
        val msb = MathLib.mostSignificant(ic.iostate(r)>>>16)
        if ((outputMask(gate.shape)&1<<r) != 0) msb else 0
    }

    private val bout = new Array[Byte](16)
    override def getBundledOutput(gate:CircuitGatePart, r:Int) =
    {
        if ((bundledOutputMask(gate.shape)&1<<r) != 0) unpackDigital(bout, ic.iostate(r)>>16)
        else null
    }
}

object CircuitGateLogic
{
    def constructICLogic(logic:CircuitGateLogic, stack:ItemStack)
    {
        import ItemICBlueprint._
        if (hasICInside(stack))
        {
            loadIC(logic.ic, stack)
            val (ri0, ro0, bi0, bo0) = getGateMasks(stack)
            logic.ri = ri0; logic.ro = ro0; logic.bi = bi0; logic.bo = bo0
            logic.connmodes = getConnModes(stack)
        }
    }

    def packIO(ri:Int, ro:Int, bi:Int, bo:Int) = ri|ro<<4|bi<<8|bo<<12
    def unpackIO(io:Int) = (io&0xF, io>>4&0xF, io>>8&0xF, io>>12&0xF)

    def packConnModes(connmodes:Array[Int]) = connmodes.foldRight(0){(i, pack) => pack<<4|i&0xF}
    def unpackConnModes(cm:Int) =
    {
        val connmodes = new Array[Int](4)
        for (i <- 0 until 4) connmodes(i) = cm>>4*i&0xF
        connmodes
    }
}

import mrtjp.projectred.integration.ComponentStore._
class RenderCircuitGate extends GateRenderer[CircuitGatePart]
{
    var simp = new SidedWireModel(generateWireModels("IC1", 4))
    var analog = new SidedWireModel(generateWireModels("IC2", 4))
    var bundled = new SidedICBundledCableModel
    var housing = new ICChipHousingModel

    var name = "untitled"

    override def coreModels = Seq(new integration.BaseComponentModel, simp, analog, bundled, new ICChipModel, housing)

    override def prepareInv(stack:ItemStack)
    {
        import ItemICBlueprint._
        if (hasICInside(stack))
        {
            name = getICName(stack)
            val cm = getConnModes(stack)
            simp.sidemask = compMaskFor(Simple, cm)
            analog.sidemask = compMaskFor(Analog, cm)
            bundled.sidemask = compMaskFor(Bundled, cm)
        }
        else
        {
            name = "ERROR!"
            simp.sidemask = 0
            analog.sidemask = 0
            bundled.sidemask = 0
        }

        simp.wires.foreach(_.on = false)
        analog.wires.foreach(_.on = false)
    }

    override def prepare(gate:CircuitGatePart)
    {
        simp.sidemask = compMaskFor(Simple, gate.getLogicIC.connmodes)
        analog.sidemask = compMaskFor(Analog, gate.getLogicIC.connmodes)
        bundled.sidemask = compMaskFor(Bundled, gate.getLogicIC.connmodes)

        simp.wires(0).on = (gate.state&0x11) != 0
        simp.wires(1).on = (gate.state&0x22) != 0
        simp.wires(2).on = (gate.state&0x44) != 0
        simp.wires(3).on = (gate.state&0x88) != 0
        analog.wires.zipWithIndex.foreach(w => w._1.on = simp.wires(w._2).on)
    }

    def compMaskFor(c:Int, conns:Seq[Int]) =
    {
        var m = 0
        for (r <- 0 until 4)
            if (conns(r) == c) m |= 1<<r
        m
    }

    override def hasSpecials = true
    override def prepareDynamic(gate:CircuitGatePart, frame:Float)
    {
        name = gate.getLogicIC.name
    }

    override def renderDynamic(t:Transformation)
    {
        import GL11._

        glDisable(GL_LIGHTING)
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        glPushMatrix()

        val s = GuiDraw.getStringWidth(name) max 93
        val f = 9/16D*1.0/s
        (new Rotation(90.0.toRadians, 1, 0, 0) `with` new Translation(8/16D*1/f, 2.26/16D, 11.25/16D*1/f) `with`
                new Scale(f, 1, f) `with` t).glApply()
        GuiDraw.drawStringC(name, 0, 0, 0xFFFFFFFF, false)

        glPopMatrix()
        glEnable(GL_LIGHTING)
        glDisable(GL_BLEND)

        //glass
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        TextureUtils.bindAtlas(0)
        CCRenderState.startDrawing()
        CCRenderState.pullLightmap()
        CCRenderState.setDynamic()
        housing.renderDynamic(t)
        CCRenderState.draw()
        glDisable(GL_BLEND)
    }
}