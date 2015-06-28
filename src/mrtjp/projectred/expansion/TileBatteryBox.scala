/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.MultiIconTransformation
import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec.{Scale, Translation, Vector3}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{ICrafting, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.ItemRenderType._
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}

import scala.collection.mutable.ListBuffer

trait TPowerStorage extends TileMachine with TPoweredMachine
{
    var storage = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("storage", storage)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        storage = tag.getInteger("storage")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeInt(storage)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        storage = in.readInt()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 5 =>
            storage = in.readInt()
            markRender()
        case _ => super.read(in, key)
    }

    def sendStorage()
    {
        writeStream(5).writeInt(storage).sendToChunk()
    }

    def getStorageScaled(i:Int) = math.min(i, i*storage/getMaxStorage)

    def getMaxStorage:Int
    def getDrawSpeed:Int
    def getDrawCeil:Int
    def getDrawFloor:Int

    abstract override def update()
    {
        super.update()

        if (cond.charge > getDrawCeil && storage < getMaxStorage)
        {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-storage)
            cond.drawPower(n*1000)
            storage += n
        }
        else if (cond.charge < getDrawFloor && storage > 0)
        {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, storage)
            cond.applyPower(n*1000)
            storage -= n
        }
    }
}

class TileBatteryBox extends TileMachine with TPowerStorage with TGuiMachine with TInventory with ISidedInventory
{
    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
        s = getStorageScaled(8)
    }

    override def size = 2
    override def stackLimit = 1
    override def name = "battery_box"
    def getAccessibleSlotsFromSide(s:Int) = s match
    {
        case 1 => Array(0) // input
        case _ => Array(1) // output
    }
    def canInsertItem(slot:Int, itemstack:ItemStack, side:Int) = true
    def canExtractItem(slot:Int, itemstack:ItemStack, side:Int) = true

    override def isItemValidForSlot(slot:Int, item:ItemStack) =
        item != null && item.getItem.isInstanceOf[TItemBattery]

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    override def onBlockPlaced(side:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        super.onBlockPlaced(side, meta, player, stack, hit)
        if (stack.hasTagCompound)
        {
            val tag = stack.getTagCompound
            storage = tag.getInteger("storage")
        }
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, x, y, z)
    }

    override def addHarvestContents(ist:ListBuffer[ItemStack])
    {
        val stack = new ItemStack(getBlock, 1, getMetaData)
        if (storage > 0)
        {
            val tag = new NBTTagCompound
            tag.setInteger("storage", storage)
            tag.setInteger("rstorage", getStorageScaled(8))
            stack.setTagCompound(tag)
        }
        ist += stack
    }

    override def update()
    {
        super.update()

        tryChargeBattery()
        tryDischargeBattery()

        updateRendersIfNeeded()
    }

    private var s = 0
    def updateRendersIfNeeded()
    {
        val s2 = getStorageScaled(8)
        if (s != s2) sendStorage()
        s = s2
    }

    override def getMaxStorage = 8000
    override def getDrawSpeed = 100
    override def getDrawCeil = 900
    override def getDrawFloor = 800
    def getChargeSpeed = 25

    def tryDischargeBattery()
    {
        val stack = getStackInSlot(1)
        if (stack != null) stack.getItem match
        {
            case b:TItemBattery =>
                val toDraw = math.min(getMaxStorage-storage, getChargeSpeed)
                val (newStack, drawn) = b.drawPower(stack, toDraw)
                setInventorySlotContents(1, newStack)
                storage += drawn
            case _ =>
        }
    }

    def tryChargeBattery()
    {
        val stack = getStackInSlot(0)
        if (stack != null) stack.getItem match
        {
            case b:TItemBattery =>
                val toAdd = math.min(storage, getChargeSpeed)
                val (newStack, added) = b.addPower(stack, toAdd)
                setInventorySlotContents(0, newStack)
                storage -= added
            case _ =>
        }
    }

    override def openGui(player:EntityPlayer)
    {
        GuiBatteryBox.open(player, createContainer(player), _.writeCoord(x, y, z))
    }

    override def createContainer(player:EntityPlayer) =
        new ContainerBatteryBox(player, this)
}

class ContainerBatteryBox(p:EntityPlayer, tile:TileBatteryBox) extends ContainerPoweredMachine(p, tile)
{
    {
        addSlotToContainer(new Slot3(tile, 0, 80, 31))
        addSlotToContainer(new Slot3(tile, 1, 80, 53))
        addPlayerInv(p, 8, 89)
    }

    private var st = -1
    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- crafters)
        {
            if (st != tile.storage) i.asInstanceOf[ICrafting]
                    .sendProgressBarUpdate(this, 3, tile.storage)
        }
        st = tile.storage
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.storage = bar
        case _ => super.updateProgressBar(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int) =
    {
        if (from == 0 || from == 1) tryMergeItemStack(stack, 2, 38, true)
        else stack.getItem match
        {
            case b:TItemBattery => tryMergeItemStack(stack, 0, 2, b.nonEmpty)
            case _ => false
        }
    }
}

class GuiBatteryBox(tile:TileBatteryBox, c:ContainerBatteryBox) extends NodeGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiBatteryBox.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(57, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(57, 26, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.storage == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(112, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(112, 26, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.cond.charge > tile.getDrawCeil && tile.storage < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(65, 52, 199, 18, 48, 18)
        else if (tile.cond.charge < tile.getDrawFloor && tile.storage > 0)
            GuiDraw.drawTexturedModalRect(65, 30, 199, 0, 48, 18)

        GuiDraw.drawString("Battery Box", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, Colors.GREY.argb, false)
    }
}

object GuiBatteryBox extends TGuiBuilder
{
    override def getID = ExpansionProxy.batteryBoxGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileBatteryBox => new GuiBatteryBox(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderBatteryBox extends TCubeMapRender with IItemRenderer
{
    var bottom:IIcon = _
    var top:IIcon = _
    val sides = new Array[IIcon](9)

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileBatteryBox])
        val i = if (te != null) sides(te.getStorageScaled(8))
        else sides(0)
        (0, 0, new MultiIconTransformation(bottom, top, i, i, i, i))
    }

    override def getInvData = (0, 0, new MultiIconTransformation(bottom, top, sides(0), sides(0), sides(0), sides(0)))

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => sides(0)
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:machines/batterybox/bottom")
        top = reg.registerIcon("projectred:machines/batterybox/top")
        for (i <- 0 until 9)
            sides(i) = reg.registerIcon("projectred:machines/batterybox/side"+i)
    }

    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true
    override def handleRenderType(stack:ItemStack, t:ItemRenderType) =
    {
        stack.getItemDamage == 5 && stack.hasTagCompound
    }

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        val rst = item.getTagCompound.getInteger("rstorage")
        val icon = sides(rst)
        val iconT = new MultiIconTransformation(bottom, top, icon, icon, icon, icon)

        t match
        {
            case ENTITY => render(-0.5, -0.5, -0.5, 1)
            case EQUIPPED => render(0, 0, 0, 1)
            case EQUIPPED_FIRST_PERSON => render(0, 0, 0, 1)
            case INVENTORY => render(-0.5, -0.5, -0.5, 1)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, s:Double)
        {
            val t = new Scale(s) `with` new Translation(x, y, z)

            TextureUtils.bindAtlas(0)
            CCRenderState.reset()
            CCRenderState.setDynamic()
            CCRenderState.pullLightmap()
            CCRenderState.startDrawing()
            TCubeMapRender.models(0)(0).render(t, iconT)
            CCRenderState.draw()
        }
    }
}