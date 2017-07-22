/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.MultiIconTransformation
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IContainerListener, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.ListBuffer

trait TPowerStorage extends TileMachine with TPoweredMachine
{
    var powerStored = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("storage", powerStored)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        powerStored = tag.getInteger("storage")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeInt(powerStored)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        powerStored = in.readInt()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 5 =>
            powerStored = in.readInt()
            markRender()
        case _ => super.read(in, key)
    }

    def sendStorage()
    {
        writeStream(5).writeInt(powerStored).sendToChunk(this)
    }

    def getStorageScaled(i:Int) = math.min(i, i*powerStored/getMaxStorage)

    def getMaxStorage:Int
    def getDrawSpeed:Int
    def getDrawCeil:Int
    def getDrawFloor:Int

    abstract override def updateServer()
    {
        super.updateServer()

        if (cond.charge > getDrawCeil && powerStored < getMaxStorage)
        {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-powerStored)
            cond.drawPower(n*1000)
            powerStored += n
        }
        else if (cond.charge < getDrawFloor && powerStored > 0)
        {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, powerStored)
            cond.applyPower(n*1000)
            powerStored -= n
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

    override protected val storage = new Array[ItemStack](2)
    override def getInventoryStackLimit = 1
    override def getName = "battery_box"
    override def getDisplayName = super.getDisplayName

    def getSlotsForFace(s:EnumFacing) = s match
    {
        case EnumFacing.UP => Array(0) // input
        case _ => Array(1) // output
    }
    def canInsertItem(slot:Int, itemstack:ItemStack, side:EnumFacing) = true
    def canExtractItem(slot:Int, itemstack:ItemStack, side:EnumFacing) = true

    override def isItemValidForSlot(slot:Int, item:ItemStack) =
        item != null && item.getItem.isInstanceOf[TItemBattery]

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    override def onBlockPlaced(side:Int, player:EntityPlayer, stack:ItemStack)
    {
        super.onBlockPlaced(side, player, stack)
        if (stack.hasTagCompound)
        {
            val tag = stack.getTagCompound
            powerStored = tag.getInteger("storage")
        }
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }

    override def addHarvestContents(ist:ListBuffer[ItemStack])
    {
        val stack = new ItemStack(getBlock, 1, getBlockMetadata)
        if (powerStored > 0)
        {
            val tag = new NBTTagCompound
            tag.setInteger("storage", powerStored)
            tag.setInteger("rstorage", getStorageScaled(8))
            stack.setTagCompound(tag)
        }
        ist += stack
    }

    override def updateServer()
    {
        super.updateServer()

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
                val toDraw = math.min(getMaxStorage-powerStored, getChargeSpeed)
                val (newStack, drawn) = b.drawPower(stack, toDraw)
                setInventorySlotContents(1, newStack)
                powerStored += drawn
            case _ =>
        }
    }

    def tryChargeBattery()
    {
        val stack = getStackInSlot(0)
        if (stack != null) stack.getItem match
        {
            case b:TItemBattery =>
                val toAdd = math.min(powerStored, getChargeSpeed)
                val (newStack, added) = b.addPower(stack, toAdd)
                setInventorySlotContents(0, newStack)
                powerStored -= added
            case _ =>
        }
    }

    override def openGui(player:EntityPlayer)
    {
        GuiBatteryBox.open(player, createContainer(player), _.writePos(getPos))
    }

    override def createContainer(player:EntityPlayer) =
        new ContainerBatteryBox(player, this)
}

class ContainerBatteryBox(p:EntityPlayer, tile:TileBatteryBox) extends ContainerPoweredMachine(tile)
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
        for (i <- listeners)
        {
            if (st != tile.powerStored) i.asInstanceOf[IContainerListener]
                    .sendProgressBarUpdate(this, 3, tile.powerStored)
        }
        st = tile.powerStored
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.powerStored = bar
        case _ => super.updateProgressBar(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (from == 0 || from == 1) {//if item in battery box
            if (tryMergeItemStack(stack, 2, 11, true)) return true //to hotbar
            if (tryMergeItemStack(stack, 11, 38, true)) return true //then to player inventory
        }

        stack.getItem match {
            case b:TItemBattery => tryMergeItemStack(stack, 0, 2, b.nonEmpty) //to discharge slot if battery not empty
            case _ => false
        }
    }
}

class GuiBatteryBox(tile:TileBatteryBox, c:ContainerBatteryBox) extends NodeGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiBatteryBox.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(57, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(57, 26, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.powerStored == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(112, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(112, 26, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.cond.charge > tile.getDrawCeil && tile.powerStored < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(65, 52, 199, 18, 48, 18)
        else if (tile.cond.charge < tile.getDrawFloor && tile.powerStored > 0)
            GuiDraw.drawTexturedModalRect(65, 30, 199, 0, 48, 18)

        GuiDraw.drawString("Battery Box", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, EnumColour.GRAY.argb, false)
    }
}

object GuiBatteryBox extends TGuiFactory
{
    val background = new ResourceLocation("projectred:textures/gui/battery_box.png")
    override def getID = ExpansionProxy.batteryBoxGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.world.getTileEntity(data.readPos) match
        {
            case t:TileBatteryBox => new GuiBatteryBox(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderBatteryBox extends SimpleBlockRenderer
{
    import java.lang.{Integer => JInt}

    import org.apache.commons.lang3.tuple.Triple
    import mrtjp.projectred.expansion.BlockProperties._

    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    val sides = new Array[TextureAtlasSprite](9)

    override def handleState(state: IExtendedBlockState, world:IBlockAccess, pos:BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
        case t:TileBatteryBox =>
            state.withProperty(UNLISTED_CHARGE_PROPERTY, t.getStorageScaled(8).asInstanceOf[JInt])
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val c = state.getValue(UNLISTED_CHARGE_PROPERTY)
        val i = sides(c)
        Triple.of(0, 0, new MultiIconTransformation(bottom, top, i, i, i, i))
    }

    override def getItemTransforms(stack: ItemStack) = {
        val sideIcon:TextureAtlasSprite =
            if(stack.hasTagCompound && stack.getTagCompound.hasKey("rstorage"))
                sides(stack.getTagCompound.getInteger("rstorage")) else sides(0)

        Triple.of(0,0, new MultiIconTransformation(bottom, top, sideIcon, sideIcon, sideIcon, sideIcon))
    }

    override def shouldCull() = true

    def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => sides(0)
    }

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/top"))
        for (i <- 0 until 9)
            sides(i) = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/side"+i))
    }
}
