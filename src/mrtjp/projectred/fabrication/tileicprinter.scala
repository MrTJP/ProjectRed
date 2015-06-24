/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{IconTransformation, MultiIconTransformation, UVTransformation}
import codechicken.lib.render.{CCModel, CCRenderState, TextureUtils}
import codechicken.lib.vec._
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TInstancedBlockRender
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.item.{ItemKey, ItemKeyStack}
import mrtjp.core.vec.{Point, Size, Vec2}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.integration.ComponentStore
import mrtjp.projectred.transmission.WireDef
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.{IIcon, ResourceLocation}
import net.minecraft.world.IBlockAccess
import org.lwjgl.opengl.GL11._

import scala.collection.JavaConversions._
import scala.collection.mutable.{Map => MMap}

class TileICPrinter extends TileICMachine with TInventory
{
    var progress = 0.0
    var speed = 0.0
    var isWorking = false
    var inputICState = 0 // 0 - none, 1 - blank, 2 - written

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setFloat("prog", progress.toFloat)
        tag.setFloat("sp", speed.toFloat)
        tag.setBoolean("w", isWorking)
        tag.setByte("in", inputICState.toByte)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        progress = tag.getFloat("prog")
        speed = tag.getFloat("sp")
        isWorking = tag.getBoolean("w")
        inputICState = tag.getByte("in")
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeFloat(progress.toFloat)
        out.writeFloat(speed.toFloat)
        out.writeBoolean(isWorking)
        out.writeByte(inputICState)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        progress = in.readFloat()
        speed = in.readFloat()
        isWorking = in.readBoolean()
        inputICState = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 4 => isWorking = false
        case 5 =>
            isWorking = true
            progress = in.readFloat()
            speed = in.readFloat()
        case 6 => inputICState = in.readByte()
        case _ => super.read(in, key)
    }

    def sendStartWorking()
    {
        val out = writeStream(5)
        out.writeFloat(progress.toFloat)
        out.writeFloat(speed.toFloat)
        out.sendToChunk()
    }

    def sendStopWorking()
    {
        writeStream(4).sendToChunk()
    }

    def sendInputICStateUpdate()
    {
        writeStream(6).writeByte(inputICState).sendToChunk()
    }

    //0 - 17 = ingredients
    //18 = Blueprint input
    //19 = IC Input
    //20 = Output
    override def isItemValidForSlot(slot:Int, item:ItemStack) =
    {
        if (slot == 18) item.getItem.isInstanceOf[ItemICBlueprint] && ItemICBlueprint.hasICInside(item)
        else if (slot == 19) item.getItem.isInstanceOf[ItemICChip]
        else if (slot == 20) false
        else true
    }

    override def size = 21
    override def stackLimit = 64
    override def name = "icprinter"

    override def isSolid(side:Int) = false

    override def update()
    {
        if (isWorking)
        {
            progress += speed
            if (progress >= 1.0)
            {
                doStop()
                onFinished()
            }
        }
        else if (canStart)
        {
            doStart()
        }
    }

    def canStart =
    {
        checkOutputClear && checkInputIC && checkBlueprint && checkIngredients
    }

    private def checkBlueprint =
    {
        val stack = getStackInSlot(18)
        stack != null && stack.getItem.isInstanceOf[ItemICBlueprint] &&
                ItemICBlueprint.hasICInside(stack)
    }

    private def checkInputIC =
    {
        val stack = getStackInSlot(19)
        stack != null && stack.getItem.isInstanceOf[ItemICChip]
    }

    private def checkOutputClear = getStackInSlot(20) == null

    private def checkIngredients =
    {
        getRequiredResources.forall(containsEnoughOf)
    }

    def getRequiredResources =
    {
        val stack = getStackInSlot(18)
        if (stack != null && ItemICBlueprint.hasICInside(stack))
        {
            val ic = ItemICBlueprint.loadIC(stack)
            TileICPrinter.resolveResources(ic)
        }
        else Seq()
    }

    def containsEnoughOf(stack:ItemKeyStack):Boolean =
    {
        var a = 0
        for (i <- 0 until 18)
        {
            val s = getStackInSlot(i)
            if (s != null && ItemKey.get(s) == stack.key)
            {
                a += s.stackSize
                if (a >= stack.stackSize) return true
            }
        }
        false
    }

    def eatResource(stack:ItemKeyStack)
    {
        var left = stack.stackSize
        for (i <- 0 until 18)
        {
            val s = getStackInSlot(i)
            if (s != null)
            {
                val toEat = math.min(left, s.stackSize)
                left -= toEat
                s.stackSize -= toEat
                if (s.stackSize <= 0) setInventorySlotContents(i, null)
                if (left <= 0) return
            }
        }
    }

    def doStart()
    {
        isWorking = true
        progress = 0.0
        speed = 0.0005
        sendStartWorking()
    }

    def doStop()
    {
        isWorking = false
        progress = 0.0
        speed = 0.0
        sendStopWorking()
    }

    def onFinished()
    {
        getRequiredResources.foreach(eatResource)
        val bp = getStackInSlot(18)
        val chip = getStackInSlot(19)

        ItemICBlueprint.copyIC(bp, chip)
        setInventorySlotContents(19, null)
        setInventorySlotContents(20, chip)
    }

    override def markDirty()
    {
        super.markDirty()
        if (!world.isRemote)
        {
            if (!canStart && isWorking) doStop()
            val oldICState = inputICState

            if (getStackInSlot(20) != null) inputICState = 2
            else
            {
                val s = getStackInSlot(19)
                if (s != null)
                {
                    if (ItemICBlueprint.hasICInside(s)) inputICState = 2
                    else inputICState = 1
                }
                else inputICState = 0
            }

            if (inputICState != oldICState) sendInputICStateUpdate()
        }
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        if (super.onBlockActivated(player, side)) return true
        if (!world.isRemote)
            GuiICPrinter.open(player, createContainer(player), _.writeCoord(x, y, z))
        true
    }

    def createContainer(player:EntityPlayer) =
        new ContainerPrinter(player, this)

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, x, y, z)
    }

    //Client-side render things
    import TileICPrinter._
    var lProgress = 1.0
    var lSpeed = 0.0
    var lState = LERPTOREST
    private var lerpCount = 0

    override def updateClient()
    {
        if (isWorking)
        {
            progress += speed
            progress = math.min(progress, 1.0)
        }

        lState match
        {
            case `REST` =>
                lProgress = 0
                lSpeed = 0
                if (isWorking) lState = `REALTIME`
            case LERPTOREST =>
                lSpeed = -0.025
                lProgress += lSpeed
                if (lProgress <= 0) lState = REST
                else if (isWorking) lState = LERPTOREALTIME
            case LERPTOREALTIME =>
                if (lProgress > progress)
                {
                    lSpeed = -0.16
                    lProgress += lSpeed
                    if (lProgress <= progress) lState = REALTIME
                }
                else
                {
                    lSpeed = 0.16
                    lProgress += lSpeed
                    if (lProgress >= progress) lState = REALTIME
                }
            case `REALTIME` =>
                if (progress >= 1.0 || !isWorking)
                {
                    lerpCount = 0
                    lState = `FIN`
                }
                else
                {
                    lProgress = progress
                    lSpeed = speed
                }
            case `FIN` =>
                lSpeed = 0
                lerpCount += 1
                if (lerpCount >= 25) lState = LERPTOREST
        }
    }
}

object TileICPrinter
{
    //render states
    val REST = 0
    val LERPTOREST = 1
    val LERPTOREALTIME = 2
    val REALTIME = 3
    val FIN = 4

    def resolveResources(ic:IntegratedCircuit) =
    {
        val map = MMap[ItemKey, Double]()
        def add(key:ItemKey, amount:Double)
        {
            val c = map.getOrElse(key, 0.0)
            map(key) = c+amount
        }

        for (part <- ic.parts.values) part match
        {
            case p:AlloyWireICPart => add(ItemKey.get(WireDef.RED_ALLOY.makeStack), 0.25)
            case p:InsulatedWireICPart => add(ItemKey.get(WireDef.INSULATED_WIRES(p.colour&0xFF).makeStack), 0.25)
            case p:BundledCableICPart => add(ItemKey.get(WireDef.BUNDLED_WIRES((p.colour+1)&0xFF).makeStack), 0.25)
            case _ =>
        }

        map.map(e => ItemKeyStack.get(e._1, e._2.ceil.toInt)).toSeq
    }
}

class ContainerPrinter(player:EntityPlayer, tile:TileICPrinter) extends NodeContainer
{
    {
        var i = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 75, 9, 2, 0, 0))
        {
            addSlotToContainer(new Slot3(tile, i, x, y))
            i += 1
        }
        addSlotToContainer(new Slot3(tile, 18, 63, 20))
        addSlotToContainer(new Slot3(tile, 19, 63, 46))
        addSlotToContainer(new Slot3(tile, 20, 134, 33))

        addPlayerInv(player, 8, 119)
    }

    //0 - 17 = ingredients
    //18 = Blueprint input
    //19 = IC Input
    //20 = Output
    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (from == 20)
        {
            if (tryMergeItemStack(stack, 21, 56, true)) return true
        }
        else if (0 to 20 contains from)
        {
            if (tryMergeItemStack(stack, 21, 56, false)) return true
        }
        else
        {
            stack.getItem match
            {
                case i:ItemICBlueprint => if (tryMergeItemStack(stack, 18, 19, false)) return true
                case i:ItemICChip => if (tryMergeItemStack(stack, 19, 20, false)) return true
                case _ =>
            }

            if (tryMergeItemStack(stack, 0, 18, false)) return true
        }
        false
    }

    var slotChangeDelegate = {() =>}

    override def slotClick(id:Int, mouse:Int, shift:Int, player:EntityPlayer) =
    {
        val i = super.slotClick(id, mouse, shift, player)
        slotChangeDelegate()
        i
    }
}

class GuiICPrinter(c:ContainerPrinter, tile:TileICPrinter) extends NodeGui(c, 176, 201)
{
    var list:ItemListNode = null

    {
        val clip = new ClipNode
        clip.position = Point(8, 17)
        clip.size = Size(48, 48)
        addChild(clip)

        val pan = new NodePan
        pan.size = Size(48, 48)
        pan.scrollModifier = Vec2(0, 1)
        pan.scrollBarHorizontal = false
        clip.addChild(pan)

        list = new ItemListNode
        list.zPosition = -0.01
        list.itemSize = Size(14, 14)
        list.displayNodeFactory = {stack =>
            val d = new ItemDisplayNode
            d.backgroundColour = if (tile.containsEnoughOf(stack))
                Colors.LIME.rgb|0x44000000 else Colors.RED.rgb|0x44000000
            d
        }
        pan.addChild(list)
        resetList()

        c.slotChangeDelegate = {() => resetList()}
    }

    def resetList()
    {
        list.items = tile.getRequiredResources
        list.reset()
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        PRResources.guiICPrinter.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 201)
        GuiDraw.drawString("IC Printer", 8, 6, Colors.GREY.argb, false)
        if (tile.isWorking)
        {
            val dx = 37*tile.progress
            GuiDraw.drawTexturedModalRect(86, 32, 176, 0, dx.toInt, 18)
        }
    }
}

object GuiICPrinter extends TGuiBuilder
{
    override def getID = FabricationProxy.icPrinterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val t = WorldLib.getTileEntity(player.worldObj, data.readCoord(), classOf[TileICPrinter])
        if (t != null) new GuiICPrinter(t.createContainer(player), t)
        else null
    }
}

object RenderICPrinter extends TInstancedBlockRender
{
    val lowerBoxes =
    {
        val array = new Array[CCModel](4)
        val box = CCModel.quadModel(24).generateBlock(0, new Cuboid6(0, 0, 0, 1, 10/16D, 1))
        for (r <- 0 until 4)
        {
            val m = box.copy.apply(Rotation.quarterRotations(r).at(Vector3.center))
            m.computeNormals()
            array(r) = m
        }
        array
    }

    var headIcon:IIcon = null
    var bottom:IIcon = null
    var side1:IIcon = null
    var side2:IIcon = null
    var top:IIcon = null
    var iconT:UVTransformation = null

    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        val tile = WorldLib.getTileEntity(w, x, y, z, classOf[TileICPrinter])
        CCRenderState.reset()
        CCRenderState.lightMatrix.locate(w, x, y, z)
        lowerBoxes(tile.rotation).render(new Translation(x, y, z), iconT, CCRenderState.lightMatrix)
    }

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case 2 => side1
        case 3 => side1
        case _ => side2
    }

    override def renderInvBlock(r:RenderBlocks, meta:Int)
    {
        val invT = new Translation(-0.5, -0.5, -0.5)

        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.pullLightmap()
        CCRenderState.startDrawing()
        lowerBoxes(0).render(invT, iconT)
        CCRenderState.draw()

        RenderICPrinterDynamic.progress = 0
        RenderICPrinterDynamic.speed = 0
        RenderICPrinterDynamic.frame = 0
        RenderICPrinterDynamic.icState = 2
        RenderICPrinterDynamic.renderPrinterTop(invT)
    }

    override def registerIcons(reg:IIconRegister)
    {
        def register(t:String) = reg.registerIcon("projectred:circuits/printer/"+t)

        headIcon = register("printerhead")
        bottom = register("bottom")
        side1 = register("side1")
        side2 = register("side2")
        top = register("top")

        iconT = new MultiIconTransformation(bottom, top, side2, side1, side2, side2)
    }
}

object RenderICPrinterDynamic extends TileEntitySpecialRenderer
{
    var models = getMods

    def getMods =
    {
        val map = CCModel.parseObjModels(new ResourceLocation("projectred:textures/obj/circuits/printer.obj"), 7, null).toMap
        map.values.foreach
        { m =>
            m.verts = m.backfacedCopy.verts
            m.apply(new Translation(8/16D, 10/16D, 8/16D))
            m.computeNormals()
            m.shrinkUVs(0.0005)
        }
        map
    }

    var progress = 0.0
    var speed = 0.0
    var frame = 0.0
    var icState = 0
    var rasterMode = false

    override def renderTileEntityAt(tile:TileEntity, x:Double, y:Double, z:Double, f:Float)
    {
        val ptile = tile.asInstanceOf[TileICPrinter]

        progress = ptile.lProgress
        speed = ptile.lSpeed
        frame = f
        icState = ptile.inputICState
        rasterMode = ptile.lState == TileICPrinter.REALTIME

        renderPrinterTop(ptile.rotationT `with` new Translation(x, y, z))
    }

    def renderPrinterTop(t:Transformation)
    {
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.pullLightmap()
        CCRenderState.setDynamic()
        CCRenderState.startDrawing()

        if (icState != 0) renderICChip(t)

        val iconT = new IconTransformation(RenderICPrinter.headIcon)
        renderFrame(t, iconT)
        renderShaft(t, iconT)
        CCRenderState.draw()

        renderGlass(t, iconT)
    }

    def renderICChip(t:Transformation)
    {
        import ComponentStore._
        icChip.render(Rotation.quarterRotations(2) `with` new Translation(0.5, 9.5/16D, 0.5) `with` t,
            new IconTransformation(if (icState == 1) icChipIconOff else icChipIcon))
    }

    def renderFrame(t:Transformation, iconT:UVTransformation)
    {
        models("frame").render(t, iconT)
    }

    def renderShaft(t:Transformation, iconT:UVTransformation)
    {
        val min = -4.5/16D
        val max = 4.5/16D
        val p = progress+speed*frame
        val subT = new Translation(0, 0, min+(max-min)*p) `with` t

        models("shaft").render(subT, iconT)
        renderHead(subT, iconT)
    }

    def renderHead(t:Transformation, iconT:UVTransformation)
    {
        val amp = 3.5/16D
        val freq = 900
        val p = progress+speed*frame
        val trans = if (rasterMode) math.cos(p*freq)*amp else amp*2*p-amp
        val subT = new Translation(trans, 0, 0) `with` t
        models("head").render(subT, iconT)
    }

    def renderGlass(t:Transformation, iconT:UVTransformation)
    {
        glEnable(GL_BLEND)
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)

        CCRenderState.startDrawing()
        models("glass").render(t, iconT)
        CCRenderState.draw()

        glDisable(GL_BLEND)
    }
}