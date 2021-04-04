/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.inventory.container.ICCLContainerFactory
import codechicken.lib.texture.TextureUtils
import mrtjp.core.gui.{GuiLib, Slot3, _}
import mrtjp.core.inventory.{InvWrapper, TInventory, TInventoryCapablilityTile}
import mrtjp.core.item.ItemKey
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.item.IChargable
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.inventory.ISidedInventory
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.text.ITextComponent
import net.minecraft.util.{Direction, ResourceLocation}

import scala.jdk.CollectionConverters._

class TileChargingBench extends TileMachine(ExpansionContent.chargingBenchTile.get) with TPoweredMachine with TGuiMachine with TInventory with ISidedInventory with TInventoryCapablilityTile
{
    var powerStorage = 0
    var isCharged = false

    private var slotRoundRobin = 0

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        tag.putInt("storage", powerStorage)
        tag.putByte("srr", slotRoundRobin.toByte)
        saveInv(tag)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        powerStorage = tag.getInt("storage")
        slotRoundRobin = tag.getByte("srr")
        isCharged = cond.canWork
        oldIC = isCharged
        loadInv(tag)
    }

    override def writeDesc(out:MCDataOutput):Unit = {
        super.writeDesc(out)
        out.writeBoolean(isCharged)
    }

    override def readDesc(in:MCDataInput):Unit = {
        super.readDesc(in)
        isCharged = in.readBoolean()
    }


    override def readUpdate(key:Int, in:MCDataInput):Unit = key match {
        case 5 =>
            isCharged = in.readBoolean()
//            markRender()
        case _ => super.readUpdate(key, in)
    }

    def sendIsCharged():Unit = {
        sendUpdate(5, _.writeBoolean(isCharged))
    }

    override protected val storage = Array.fill(16)(ItemStack.EMPTY)//new Array[ItemStack](16)
    override def getInventoryStackLimit = 1
    override def nbtSaveName = "charging_bench"

    override def canExtractItem(slot:Int, stack:ItemStack, side:Direction):Boolean = true
    override def canInsertItem(slot:Int, stack:ItemStack, side:Direction):Boolean = side == Direction.UP
    override def getSlotsForFace(side:Direction):Array[Int] = side match  {
        case Direction.UP => (0 until 8).toArray
        case Direction.NORTH|Direction.SOUTH|Direction.WEST|Direction.EAST => (8 until 16).toArray
        case _ => Array.emptyIntArray
    }

    override def isItemValidForSlot(slot:Int, item:ItemStack):Boolean =
        slot < 8 && item.getItem.isInstanceOf[IChargable]

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerChargingBench =
        new ContainerChargingBench(playerInv, this, windowId)

    override def doesRotate = false

    def getStorageScaled(i:Int):Int = math.min(i, i*powerStorage/getMaxStorage)

    def getMaxStorage = 4000
    def getDrawSpeed = 150
    def getDrawCeil = 600
    def getChargeSpeed = 15

    override def updateServer():Unit = {
        super.updateServer()

        if (cond.charge > getDrawCeil && powerStorage < getMaxStorage) {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-powerStorage)
            cond.drawPower(n*1000)
            powerStorage += n
        }

        for (i <- 0 until 8) if (powerStorage > 0)
            tryChargeSlot((slotRoundRobin+i)%8)
        slotRoundRobin = (slotRoundRobin+1)%8

        if (world.getGameTime%10 == 0) updateRendersIfNeeded()
    }

    def tryChargeSlot(i:Int):Unit = {
        val stack = getStackInSlot(i)
        if (!stack.isEmpty) stack.getItem match {
            case ic:IChargable =>
                val toAdd = math.min(powerStorage, getChargeSpeed)
                val (newStack, added) = ic.addPower(stack, toAdd)
                if (ic.isFullyCharged(newStack) && dropStackDown(newStack))
                    setInventorySlotContents(i, ItemStack.EMPTY)
                else setInventorySlotContents(i, newStack)
                powerStorage -= added
            case _ =>
        }
    }

    def dropStackDown(stack:ItemStack):Boolean = {
        val wr = InvWrapper.wrapInternal(this, 8 until 16)
        val i = wr.injectItem(ItemKey.get(stack), stack.getCount)
        i > 0
    }

    def containsUncharged:Boolean = {
        for (i <- 0 until 8) {
            val stack = getStackInSlot(i)
            if (!stack.isEmpty) stack.getItem match {
                case ic:IChargable if !ic.isFullyCharged(stack) => return true
                case _ =>
            }
        }
        false
    }

    private var oldIC = false
    def updateRendersIfNeeded():Unit = {
        isCharged = cond.canWork
        if (oldIC != isCharged) {
            sendIsCharged()
            pushState()
        }
        oldIC = isCharged
    }

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(world, getPos)
    }
}

class ContainerChargingBench(playerInv:PlayerInventory, val tile:TileChargingBench, windowId:Int) extends ContainerPoweredMachine(tile, ExpansionContent.chargingBenchContainer.get, windowId)
{
    {
        var id = 0
        for ((x, y) <- GuiLib.createSlotGrid(88, 17, 4, 2, 0, 0)) {
            addSlot(new Slot3(tile, id, x, y))
            id += 1
        }
        for ((x, y) <- GuiLib.createSlotGrid(88, 57, 4, 2, 0, 0)) {
            addSlot(new Slot3(tile, id, x, y))
            id += 1
        }
        addPlayerInv(playerInv, 8, 101)
    }

    private var st = -1
    override def detectAndSendChanges() {
        super.detectAndSendChanges()
        for (i <- listeners.asScala) {
            if (st != tile.powerStorage)
                i.sendWindowProperty(this, 3, tile.powerStorage)
        }
        st = tile.powerStorage
    }

    override def updateProgressBar(id:Int, bar:Int):Unit = id match {
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

object ContainerChargingBench extends ICCLContainerFactory[ContainerChargingBench] {
    override def create(windowId:Int, inventory:PlayerInventory, packet:MCDataInput):ContainerChargingBench = {
        inventory.player.world.getTileEntity(packet.readPos()) match {
            case t:TileChargingBench => t.createMenu(windowId, inventory, inventory.player)
            case _ => null
        }
    }
}

class GuiChargingBench(c:ContainerChargingBench, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 183, playerInv, title)
{
    private def drawVerticalTank(x:Int, y:Int, u:Int, v:Int, w:Int, h:Int, prog:Int):Unit = {
        blit(x, y+h-prog, u, v+h-prog, w, prog)
    }

    override def drawBack_Impl(mouse:Point, frame:Float):Unit = {
        TextureUtils.changeTexture(GuiChargingBench.background)
        blit(0, 0, 0, 0, size.width, size.height)

        if (c.tile.cond.canWork)
            blit(14, 17, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(this, 14, 27, 176, 10, 7, 48, c.tile.cond.getChargeScaled(48))

        if (c.tile.powerStorage == c.tile.getMaxStorage)
            blit(41, 17, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(this, 41, 27, 184, 10, 14, 48, c.tile.getStorageScaled(48))

        if (c.tile.cond.charge > c.tile.getDrawCeil && c.tile.powerStorage < c.tile.getMaxStorage)
            blit(26, 48, 199, 0, 10, 8)

        if (c.tile.containsUncharged && c.tile.powerStorage > 0)
            blit(63, 29, 210, 0, 17, 10)

        font.drawString(title.getFormattedText, 8, 6, EnumColour.GRAY.argb)
        font.drawString(playerInv.getDisplayName.getFormattedText, 8, 91, EnumColour.GRAY.argb)
    }
}

object GuiChargingBench
{
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/charging_bench.png")

    def register():Unit = {
        ScreenManager.registerFactory(
            ExpansionContent.chargingBenchContainer.get(),
            (cont:ContainerChargingBench, inv, text) => new GuiChargingBench(cont, inv, text))
    }
//    override def getID = ExpansionProxy.chargingBenchBui
//
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        player.world.getTileEntity(data.readPos()) match
//        {
//            case t:TileChargingBench => new GuiChargingBench(t, t.createContainer(player))
//            case _ => null
//        }
//    }
}
//
//object RenderChargingBench extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    import java.lang.{Boolean => JBool}
//
//    var bottom:TextureAtlasSprite = null
//    var top1:TextureAtlasSprite = null
//    var top2:TextureAtlasSprite = null
//    var side1:TextureAtlasSprite = null
//    var side2:TextureAtlasSprite = null
//
//    var iconT1:UVTransformation = null
//    var iconT2:UVTransformation = null
//
//    override def handleState(state: IExtendedBlockState, world: IBlockAccess, pos: BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
//        case t:TileChargingBench => state.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
//        case _ => state
//    }
//
//    override def getWorldTransforms(state: IExtendedBlockState) = {
//        val charged:JBool = state.getValue(UNLISTED_CHARGED_PROPERTY)
//        Triple.of(0, 0, if(charged) iconT2 else iconT1)
//    }
//
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)
//
//    override def shouldCull() = true
//
//    def getIcon(side:Int, meta:Int) = side match
//    {
//        case 0 => bottom
//        case 1 => top1
//        case _ => side1
//    }
//
//    override def registerIcons(reg:TextureMap)
//    {
//        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/bottom"))
//        top1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/top1"))
//        top2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/top2"))
//        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/side1"))
//        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/charger/side2"))
//
//        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
//        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
//    }
//}
