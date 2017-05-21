/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui.{GuiLib, Slot3,_}
import mrtjp.core.inventory.{InvWrapper, TInventory}
import mrtjp.core.item.ItemKey
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IContainerListener, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TileChargingBench extends TileMachine with TPoweredMachine with TGuiMachine with TInventory with ISidedInventory
{
    var powerStorage = 0
    var isCharged = false

    private var slotRoundRobin = 0

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setInteger("storage", powerStorage)
        tag.setByte("srr", slotRoundRobin.toByte)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        powerStorage = tag.getInteger("storage")
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
        writeStream(5).writeBoolean(isCharged).sendToChunk(this)
    }

    override protected val storage = new Array[ItemStack](16)
    override def getInventoryStackLimit = 1
    override def getName = "charging_bench"

    override def getDisplayName = super.getDisplayName
    import net.minecraft.util.EnumFacing._
    override def canExtractItem(slot:Int, stack:ItemStack, side:EnumFacing) = true
    override def canInsertItem(slot:Int, stack:ItemStack, side:EnumFacing) = side == UP
    override def getSlotsForFace(side:EnumFacing) = side match
    {
        case UP => 0 until 8 toArray
        case NORTH|SOUTH|WEST|EAST => 8 until 16 toArray
        case _ => Array.emptyIntArray
    }

    override def isItemValidForSlot(slot:Int, item:ItemStack) =
        slot < 8 && item.getItem.isInstanceOf[IChargable]

    override def openGui(player:EntityPlayer) =
        GuiChargingBench.open(player, createContainer(player), _.writePos(getPos))

    override def createContainer(player:EntityPlayer) = new ContainerChargingBench(player, this)

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    def getStorageScaled(i:Int) = math.min(i, i*powerStorage/getMaxStorage)

    def getMaxStorage = 4000
    def getDrawSpeed = 150
    def getDrawCeil = 600
    def getChargeSpeed = 15

    override def updateServer()
    {
        super.updateServer()

        if (cond.charge > getDrawCeil && powerStorage < getMaxStorage)
        {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-powerStorage)
            cond.drawPower(n*1000)
            powerStorage += n
        }

        for (i <- 0 until 8) if (powerStorage > 0)
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
                val toAdd = math.min(powerStorage, getChargeSpeed)
                val (newStack, added) = ic.addPower(stack, toAdd)
                if (ic.isFullyCharged(newStack) && dropStackDown(newStack))
                    setInventorySlotContents(i, null)
                else setInventorySlotContents(i, newStack)
                powerStorage -= added
            case _ =>
        }
    }

    def dropStackDown(stack:ItemStack) =
    {
        val wr = InvWrapper.wrap(this).setSlotsFromRange(8 until 16).setInternalMode(true)
        val i = wr.injectItem(ItemKey.get(stack), stack.stackSize)
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

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }
}

class ContainerChargingBench(p:EntityPlayer, tile:TileChargingBench) extends ContainerPoweredMachine(tile)
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
        for (i <- listeners)
        {
            if (st != tile.powerStorage) i.asInstanceOf[IContainerListener]
                    .sendProgressBarUpdate(this, 3, tile.powerStorage)
        }
        st = tile.powerStorage
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.powerStorage = bar
        case _ => super.updateProgressBar(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (0 until 8 contains from) { //from input slots
            if (tryMergeItemStack(stack, 25, 52, false)) return true //to player inv
            if (tryMergeItemStack(stack, 16, 25, false)) return true //then to hotbar
        }
        else if (8 until 16 contains from) { //from output slots
            if (tryMergeItemStack(stack, 16, 25, true)) return true //to hotbar inversed
            if (tryMergeItemStack(stack, 25, 52, true)) return true //then to player inv inversed
        }

        stack.getItem match { //from player inv
            case _:IChargable => tryMergeItemStack(stack, 0, 8, false)
            case _ => false
        }
    }
}

class GuiChargingBench(tile:TileChargingBench, c:ContainerChargingBench) extends NodeGui(c, 176, 183)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiChargingBench.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(14, 17, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(14, 27, 176, 10, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.powerStorage == tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(41, 17, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(41, 27, 184, 10, 14, 48, tile.getStorageScaled(48))

        if (tile.cond.charge > tile.getDrawCeil && tile.powerStorage < tile.getMaxStorage)
            GuiDraw.drawTexturedModalRect(26, 48, 199, 0, 10, 8)

        if (tile.containsUncharged && tile.powerStorage > 0)
            GuiDraw.drawTexturedModalRect(63, 29, 210, 0, 17, 10)

        GuiDraw.drawString("Charging Bench", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 91, EnumColour.GRAY.argb, false)
    }
}

object GuiChargingBench extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/charger.png")
    override def getID = ExpansionProxy.chargingBenchBui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.worldObj.getTileEntity(data.readPos()) match
        {
            case t:TileChargingBench => new GuiChargingBench(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderChargingBench extends SimpleBlockRenderer
{

    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.core.util.CCLConversions._
    import mrtjp.projectred.expansion.BlockProperties._
    var bottom:TextureAtlasSprite = null
    var top1:TextureAtlasSprite = null
    var top2:TextureAtlasSprite = null
    var side1:TextureAtlasSprite = null
    var side2:TextureAtlasSprite = null

    var iconT1:UVTransformation = null
    var iconT2:UVTransformation = null

    override def handleState(state: IExtendedBlockState, tileEntity: TileEntity): IExtendedBlockState = tileEntity match {
        case t:TileChargingBench => state.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val charged:JBool = state.getValue(UNLISTED_CHARGED_PROPERTY)
        createTriple(0, 0, if(charged) iconT2 else iconT1)
    }

    override def getItemTransforms(stack: ItemStack) = createTriple(0, 0, iconT1)

    override def shouldCull() = true

    def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top1
        case _ => side1
    }

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/bottom"))
        top1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/top1"))
        top2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/top2"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/side1"))
        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/side2"))

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}
