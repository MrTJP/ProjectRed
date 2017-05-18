/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec._
import mrtjp.core.math.MathLib
import mrtjp.projectred.fabrication.IIOGateTile._
import mrtjp.projectred.integration
import mrtjp.projectred.integration._
import mrtjp.projectred.transmission.BundledCommons._
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.math.BlockPos
import org.lwjgl.opengl.GL11

class ICGatePart extends RedstoneGatePart with TBundledGatePart with TComplexGatePart
{
    private var logic:ICGateLogic = null

    override def getLogic[T] = logic.asInstanceOf[T]
    def getLogicIC = getLogic[ICGateLogic]

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

    override def preparePlacement(player:EntityPlayer, pos:BlockPos, side:Int, meta:Int)
    {
        super.preparePlacement(player, pos, side, meta)
        val stack = player.getHeldItemMainhand
        itemTag = stack.getTagCompound
        ICGateLogic.constructICLogic(logic, stack)
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
            logic = new ICGateLogic(this)
    }

    override def getType = GateDefinition.typeICGate
}

class ICGateLogic(gate:ICGatePart) extends RedstoneGateLogic[ICGatePart] with TBundledGateLogic[ICGatePart] with TComplexGateLogic[ICGatePart]// with SimulatedWorldCircuit
{
    val tmap = new ICTileMapContainer
    val sim = new ICSimEngineContainer
    sim.ioChangedDelegate = {() => if (!gate.world.isRemote) gate.scheduleTick(2)}

    var systime_last = -1L

    var (ri, ro, bi, bo) = (0, 0, 0, 0)

    var out = new Array[Int](4)
    var outUnpacked = Array.ofDim[Byte](4, 16)

    var connmodes = Array(NoConn, NoConn, NoConn, NoConn)
    var name = "untitled"

    override def save(tag:NBTTagCompound)
    {
        tmap.saveTiles(tag)
        sim.saveSimState(tag)
        tag.setShort("masks", ICGateLogic.packIO(ri, ro, bi, bo).toShort)
        tag.setShort("cmode", ICGateLogic.packConnModes(connmodes).toShort)
        tag.setIntArray("bout", out)
    }

    override def load(tag:NBTTagCompound)
    {
        tmap.loadTiles(tag)
        sim.recompileSimulation(tmap)
        sim.loadSimState(tag)

        val (ri0, ro0, bi0, bo0) = ICGateLogic.unpackIO(tag.getShort("masks"))
        ri = ri0; ro = ro0; bi = bi0; bo = bo0

        connmodes = ICGateLogic.unpackConnModes(tag.getShort("cmode"))

        val b = tag.getIntArray("bout")
        for (r <- 0 until b.length) setOut(r, b(r))
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeShort(ICGateLogic.packIO(ri, ro, bi, bo))
        packet.writeShort(ICGateLogic.packConnModes(connmodes))
        packet.writeString(tmap.name)
    }

    override def readDesc(packet:MCDataInput)
    {
        val (ri0, ro0, bi0, bo0) = ICGateLogic.unpackIO(packet.readShort())
        ri = ri0; ro = ro0; bi = bi0; bo = bo0
        connmodes = ICGateLogic.unpackConnModes(packet.readShort())
        name = packet.readString()
    }

    def setOut(r:Int, output:Int)
    {
        out(r) = output
        if (((bi|bo)&1<<r) != 0) //only unpack if needed
            outUnpacked(r) = unpackDigital(outUnpacked(r), out(r))
    }

    override def inputMask(shape:Int) = ri
    override def outputMask(shape:Int) = ro
    override def bundledInputMask(shape:Int) = bi
    override def bundledOutputMask(shape:Int) = bo

    override def onTick(gate:ICGatePart)
    {
        if (!gate.world.isRemote) {
            val t = gate.world.getTotalWorldTime
            val dt = if (systime_last < 0) 1 else t-systime_last
            sim.advanceTime(dt)
            sim.repropagate()
            systime_last = t
        }
    }

    override def onChange(gate:ICGatePart)
    {
        var cmask = 0
        for (r <- 0 until 4)
            if (checkAndSetInputChange(r))
                cmask |= 1<<r

        if (cmask != 0) {
            gate.setState(gate.state&0xF0|getRSRenderInputs)
            gate.onInputChange()
            sim.onInputChanged(cmask)
            sim.repropagate()
        }
    }

    override def scheduledTick(gate:ICGatePart)
    {
        var cmask = 0
        for (r <- 0 until 4)
            if (checkAndSetOutputChange(r))
                cmask |= 1<<r

        if (cmask != 0) {
            gate.setState(gate.state&0xF|getRSRenderOutputs<<4)
            gate.onOutputChange(cmask)
        }
        onChange(gate)
    }

    def checkAndSetInputChange(r:Int):Boolean = {
        val (oldInput, newInput) = connmodes(r) match {
            case NoConn => (0, 0)
            case Simple => (sim.iostate(r)&1, if (gate.getRedstoneInput(r) != 0) 1 else 0)
            case Analog => (sim.iostate(r)&0xFFFF, 1<<(gate.getRedstoneInput(r)/17))
            case Bundled => (sim.iostate(r)&0xFFFF, packDigital(gate.getBundledInput(r)))
        }

        if (oldInput != newInput) {
            sim.setInput(r, newInput)
            true
        } else false
    }

    def checkAndSetOutputChange(r:Int):Boolean = {
        val (oldOutput, newOutput) = connmodes(r) match {
            case NoConn => (0, 0)
            case Simple => (out(r)&1, (sim.iostate(r)>>>16)&1)
            case Analog => (out(r), sim.iostate(r)>>>16)
            case Bundled =>(out(r), sim.iostate(r)>>>16)
        }
        if (oldOutput != newOutput) {
            setOut(r, newOutput)
            true
        } else
            false
    }

    def getRSRenderInputs =
    {
        var m = 0
        for (r <- 0 until 4) connmodes(r) match {
            case Simple => if ((sim.iostate(r)&1) != 0) m |= 1<<r
            case Analog => if ((sim.iostate(r)&0xFFFE) != 0) m |= 1<<r
            case _ =>
        }
        m
    }

    def getRSRenderOutputs =
    {
        var m = 0
        for (r <- 0 until 4) if (getOutput(gate, r) != 0) m |= 1<<r
        m
    }

    override def getOutput(gate:ICGatePart, r:Int):Int =
    {
        if ((outputMask(gate.shape)&1<<r) == 0)
            return 0

        connmodes(r) match {
            case Simple => if ((out(r)&1) != 0) 15 else 0
            case Analog => MathLib.mostSignificant(out(r))
            case _ => 0
        }
    }

    override def getBundledOutput(gate:ICGatePart, r:Int):Array[Byte] =
    {
        if ((bundledOutputMask(gate.shape)&1<<r) == 0)
            return null

        connmodes(r) match {
            case Bundled => outUnpacked(r)
            case _ => null
        }
    }

    override def lightLevel = 2
}

object ICGateLogic
{
    def constructICLogic(logic:ICGateLogic, stack:ItemStack)
    {
        import ItemICBlueprint._
        if (hasICInside(stack)) {
            loadTileMap(logic.tmap, stack)
            logic.sim.recompileSimulation(logic.tmap)
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

class RenderICGate extends GateRenderer[ICGatePart]
{
    import mrtjp.projectred.integration.ComponentStore._

    var simp = new SidedWireModel(generateWireModels("IC1", 4))
    var analog = new SidedWireModel(generateWireModels("IC2", 4))
    var bundled = new SidedICBundledCableModel
    var housing = new ICChipHousingModel

    var name = "untitled"

    override val coreModels = Seq(new integration.BaseComponentModel, simp, analog, bundled, new ICChipModel, housing)

    override def prepareInv(stack:ItemStack)
    {
        import ItemICBlueprint._
        if (hasICInside(stack)) {
            name = getICName(stack)
            val cm = getConnModes(stack)
            simp.sidemask = connTypeMask(Simple, cm)
            analog.sidemask = connTypeMask(Analog, cm)
            bundled.sidemask = connTypeMask(Bundled, cm)
        } else {
            name = "ERROR!"
            simp.sidemask = 0
            analog.sidemask = 0
            bundled.sidemask = 0
        }

        simp.wires.foreach(_.on = false)
        analog.wires.foreach(_.on = false)
    }

    override def prepare(gate:ICGatePart)
    {
        simp.sidemask = connTypeMask(Simple, gate.getLogicIC.connmodes)
        analog.sidemask = connTypeMask(Analog, gate.getLogicIC.connmodes)
        bundled.sidemask = connTypeMask(Bundled, gate.getLogicIC.connmodes)

        simp.wires(0).on = (gate.state&0x11) != 0
        simp.wires(1).on = (gate.state&0x22) != 0
        simp.wires(2).on = (gate.state&0x44) != 0
        simp.wires(3).on = (gate.state&0x88) != 0
        analog.wires.zipWithIndex.foreach(w => w._1.on = simp.wires(w._2).on)
    }

    def connTypeMask(c:Int, conns:Array[Int]) =
    {
        var m = 0
        for (r <- 0 until 4)
            if (conns(r) == c) m |= 1<<r
        m
    }

    override def hasSpecials = true
    override def prepareDynamic(gate:ICGatePart, frame:Float)
    {
        name = gate.getLogicIC.name
    }


    override def renderDynamic(t:Transformation, ccrs:CCRenderState)
    {
        import GL11._
        import net.minecraft.client.renderer.GlStateManager._

        disableLighting()
        enableBlend()
        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        pushMatrix()

        val s = GuiDraw.getStringWidth(name) max 93
        val f = 9/16D*1.0/s
        (new Rotation(90.0.toRadians, 1, 0, 0) `with` new Translation(8/16D*1/f, 2.26/16D, 11.25/16D*1/f) `with`
                new Scale(f, 1, f) `with` t).glApply()
        GuiDraw.drawStringC(name, 0, 0, 0xFFFFFFFF, false)

        popMatrix()
        enableLighting()
        disableBlend()

        //glass
        enableBlend()
        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
        TextureUtils.bindBlockTexture()
        ccrs.startDrawing(0x7, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
        housing.renderDynamic(t, ccrs)
        ccrs.draw()
        disableBlend()
    }
}