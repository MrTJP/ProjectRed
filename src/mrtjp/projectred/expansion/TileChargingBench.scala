/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui.{GuiLib, Slot3, TGuiBuilder, _}
import mrtjp.core.inventory.{InvWrapper, TInventory}
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

class TileChargingBench extends TileMachine with TPoweredMachine with TGuiMachine with TInventory with ISidedInventory
{
    var storage = 0
    var isCharged = false

    private var slotRoundRobin = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("storage", storage)
        tag.setByte("srr", slotRoundRobin.toByte)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        storage = tag.getInteger("storage")
        slotRoundRobin = tag.getByte("srr")
        isCharged = cond.canWork
        oldIC = isCharged
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        isCharged = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 5 =>
            isCharged = in.readBoolean()
            markRender()
        case _ => super.read(in, key)
    }

    def sendIsCharged()
    {
        writeStream(5).writeBoolean(isCharged).sendToChunk()
    }

    override def size = 16
    override def name = "charging_bench"
    override def stackLimit = 1

    override def canExtractItem(slot:Int, stack:ItemStack, side:Int) = true
    override def canInsertItem(slot:Int, stack:ItemStack, side:Int) = side == 1
    override def getAccessibleSlotsFromSide(side:Int) = side match
    {
        case 1 => 0 until 8 toArray
        case 2|3|4|5 => 8 until 16 toArray
        case _ => Array.emptyIntArray
    }

    override def isItemValidForSlot(slot:Int, item:ItemStack) =
        slot < 8 && item.getItem.isInstanceOf[IChargable]

    override def openGui(player:EntityPlayer) =
        GuiChargingBench.open(player, createContainer(player), _.writeCoord(x, y, z))

    override def createContainer(player:EntityPlayer) = new ContainerChargingBench(player, this)

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    def getStorageScaled(i:Int) = math.min(i, i*storage/getMaxStorage)

    def getMaxStorage = 4000
    def getDrawSpeed = 150
    def getDrawCeil = 600
    def getChargeSpeed = 15

    override def update()
    {
        super.update()

        if (cond.charge > getDrawCeil && storage < getMaxStorage)
        {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-storage)
            cond.drawPower(n*1000)
            storage += n
        }

        for (i <- 0 until 8) if (storage > 0)
            tryChargeSlot((slotRoundRobin+i)%8)
        slotRoundRobin = (slotRoundRobin+1)%8

        if (world.getTotalWorldTime%10 == 0) updateRendersIfNeeded()
    }

    def tryChargeSlot(i:Int)
    {
        val stack = getStackInSlot(i)
        if (stack != null) stack.getItem match
        {
            case ic:IChargable =>
                val toAdd = math.min(storage, getChargeSpeed)
                val (newStack, added) = ic.addPower(stack, toAdd)
                if (ic.isFullyCharged(newStack) && dropStackDown(newStack))
                    setInventorySlotContents(i, null)
                else setInventorySlotContents(i, newStack)
                storage -= added
            case _ =>
        }
    }

    def dropStackDown(stack:ItemStack) =
    {
        val wr = InvWrapper.wrap(this).setSlotsFromRange(8 until 16).setInternalMode(true)
        val i = wr.injectItem(stack, true)
        i > 0
    }

    def containsUncharged:Boolean =
    {
        for (i <- 0 until 8)
        {
            val stack = getStackInSlot(i)
            if (stack != null) stack.getItem match
            {
                case ic:IChargable if !ic.isFullyCharged(stack) => return true
                case _ =>
            }
        }
        false
    }

    private var oldIC = false
    def updateRendersIfNeeded()
    {
        isCharged = cond.canWork
        if (oldIC != isCharged) sendIsCharged()
        oldIC = isCharged
    }
}

class ContainerChargingBench(p:EntityPlayer, tile:TileChargingBench) extends ContainerPoweredMachine(p, tile)
{
    {
        var id = 0
        for ((x, y) <- GuiLib.createSlotGrid(88, 17, 4, 2, 0, 0))
        {
            addSlotToContainer(new Slot3(tile, id, x, y))
            id += 1
        }
        for ((x, y) <- GuiLib.createSlotGrid(88, 57, 4, 2, 0, 0))
        {
            addSlotToContainer(new Slot3(tile, id, x, y))
            id += 1
        }
        addPlayerInv(p, 8, 101)
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
        if (0 until 8 contains from) tryMergeItemStack(stack, 16, 38, false)
        else if (8 until 16 contains from) tryMergeItemStack(stack, 16, 38, true)
        else stack.getItem match
        {
            case ic:IChargable => tryMergeItemStack(stack, 0, 8, false)
            case _ => false
        }
    }
}

class GuiChargingBench(tile:TileChargingBench, c:ContainerChargingBench) extends NodeGui(c, 176, 183)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiCharger.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(14, 17, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(14, 27, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.storage == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(41, 17, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(41, 27, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.cond.charge > tile.getDrawCeil && tile.storage < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(26, 48, 199, 0, 10, 8)

        if (tile.containsUncharged && tile.storage > 0)
            GuiDraw.drawTexturedModalRect(63, 29, 210, 0, 17, 10)

        GuiDraw.drawString("Charging Bench", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 91, Colors.GREY.argb, false)
    }
}

object GuiChargingBench extends TGuiBuilder
{
    override def getID = ExpansionProxy.chargingBenchBui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileChargingBench => new GuiChargingBench(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderChargingBench extends TCubeMapRender
{
    var bottom:IIcon = null
    var top1:IIcon = null
    var top2:IIcon = null
    var side1:IIcon = null
    var side2:IIcon = null

    var iconT1:UVTransformation = null
    var iconT2:UVTransformation = null

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileChargingBench])
        if (te != null) (0, 0, if (te.isCharged) iconT2 else iconT1)
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top1
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:mechanical/charger/bottom")
        top1 = reg.registerIcon("projectred:mechanical/charger/top1")
        top2 = reg.registerIcon("projectred:mechanical/charger/top2")
        side1 = reg.registerIcon("projectred:mechanical/charger/side1")
        side2 = reg.registerIcon("projectred:mechanical/charger/side2")

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}