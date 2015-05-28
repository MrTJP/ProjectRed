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
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.vec.Point
import mrtjp.core.world.{Messenger, WorldLib}
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.integration.ComponentStore
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

class TileICPrinter extends TileICMachine with TInventory
{
    var progress = 0.0
    var speed = 0.0
    var isWorking = false
    var hasInputIC = false

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeFloat(progress.toFloat)
        out.writeFloat(speed.toFloat)
        out.writeBoolean(isWorking)
        out.writeBoolean(hasInputIC)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        progress = in.readFloat()
        speed = in.readFloat()
        isWorking = in.readBoolean()
        hasInputIC = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 4 => isWorking = false
        case 5 =>
            isWorking = true
            progress = in.readFloat()
            speed = in.readFloat()
        case 6 => hasInputIC = in.readBoolean()
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

    def sendHasICUpdate()
    {
        writeStream(6).writeBoolean(hasInputIC).sendToChunk()
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

    private def checkIngredients = true

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
            val oldHasIC = hasInputIC
            hasInputIC = checkInputIC
            if (hasInputIC != oldHasIC) sendHasICUpdate()
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
    {
        val c = new NodeContainer
        c.addPlayerInv(player, 8, 119)

        var i = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 75, 9, 2, 0, 0))
        {
            c.addSlotToContainer(new Slot3(this, i, x, y))
            i += 1
        }

        c.addSlotToContainer(new Slot3(this, 18, 63, 20))
        c.addSlotToContainer(new Slot3(this, 19, 63, 46))
        c.addSlotToContainer(new Slot3(this, 20, 134, 33))
        c
    }

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
}


class GuiICPrinter(c:NodeContainer, tile:TileICPrinter) extends NodeGui(c, 176, 201)
{
    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        PRResources.guiICPrinter.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 201)
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
    val lowerBox =
    {
        val m = CCModel.quadModel(24)
        m.generateBlock(0, new Cuboid6(0, 0, 0, 1, 10/16D, 1))
        m.computeNormals()
        m.shrinkUVs(0.0005)
        m
    }

    var headIcon:IIcon = null

    var bottom:IIcon = null
    var side1:IIcon = null
    var side2:IIcon = null
    var top:IIcon = null

    override def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)
    {
        val tile = WorldLib.getTileEntity(w, x, y, z, classOf[TileICPrinter])
        CCRenderState.reset()
        CCRenderState.lightMatrix.locate(w, x, y, z)
        lowerBox.render(tile.rotationT `with` new Translation(x, y, z),
            new MultiIconTransformation(bottom, top, side2, side1, side2, side2), CCRenderState.lightMatrix)

        RenderICPrinterDynamic.models = RenderICPrinterDynamic.getMods //TODO debug code. REMOVE
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
        lowerBox.render(invT, new MultiIconTransformation(bottom, top, side2, side1, side2, side2))
        CCRenderState.draw()

        RenderICPrinterDynamic.progress = 0
        RenderICPrinterDynamic.speed = 0
        RenderICPrinterDynamic.frame = 0
        RenderICPrinterDynamic.hasIC = true
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
    var hasIC = false
    var rasterMode = false

    override def renderTileEntityAt(tile:TileEntity, x:Double, y:Double, z:Double, f:Float)
    {
        val ptile = tile.asInstanceOf[TileICPrinter]

        progress = ptile.lProgress
        speed = ptile.lSpeed
        frame = f
        hasIC = ptile.hasInputIC
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

        if (hasIC) renderICChip(t)

        val iconT = new IconTransformation(RenderICPrinter.headIcon)
        renderFrame(t, iconT)
        renderShaft(t, iconT)
        CCRenderState.draw()

        renderGlass(t, iconT)
    }

    def renderICChip(t:Transformation)
    {
        ComponentStore.icChip.render(Rotation.quarterRotations(2) `with` new Translation(0.5, 9.5/16D, 0.5) `with` t,
            new IconTransformation(ComponentStore.icChipIcon))
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