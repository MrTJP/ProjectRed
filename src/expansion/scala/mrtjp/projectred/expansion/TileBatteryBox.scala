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
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.gui._
import mrtjp.core.inventory.{TInventory, TInventoryCapablilityTile}
import mrtjp.core.vec.Point
import mrtjp.core.world.Messenger
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.CoreTile
import mrtjp.projectred.expansion.item.{IChargable, TChargableBatteryItem}
import net.minecraft.block.BlockState
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.LivingEntity
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.inventory.ISidedInventory
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.text.ITextComponent
import net.minecraft.util.{Direction, ResourceLocation}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

trait TPowerStorage extends TileMachine with TPoweredMachine with TInventoryCapablilityTile
{
    var powerStored = 0

    override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        tag.putInt("storage", powerStored)
    }

    override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        powerStored = tag.getInt("storage")
    }

    override def writeDesc(out:MCDataOutput):Unit = {
        super.writeDesc(out)
        out.writeInt(powerStored)
    }

    override def readDesc(in:MCDataInput):Unit = {
        super.readDesc(in)
        powerStored = in.readInt()
    }

    override def readUpdate(key:Int, in:MCDataInput):Unit = key match {
        case 5 =>
            powerStored = in.readInt()
//            markRender()
        case _ => super.readUpdate(key, in)
    }

    def sendStorage():Unit = {
        sendUpdate(5, _.writeInt(powerStored))
    }

    def getStorageScaled(i:Int):Int = math.min(i, i*powerStored/getMaxStorage)

    def getMaxStorage:Int
    def getDrawSpeed:Int
    def getDrawCeil:Int
    def getDrawFloor:Int

    abstract override def updateServer():Unit = {
        super.updateServer()

        if (cond.charge > getDrawCeil && powerStored < getMaxStorage) {
            var n = math.min(cond.charge-getDrawCeil, getDrawSpeed)/10
            n = math.min(n, getMaxStorage-powerStored)
            cond.drawPower(n*1000)
            powerStored += n

        } else if (cond.charge < getDrawFloor && powerStored > 0) {
            var n = math.min(getDrawFloor-cond.charge, getDrawSpeed)/10
            n = math.min(n, powerStored)
            cond.applyPower(n*1000)
            powerStored -= n
        }
    }
}

class TileBatteryBox extends TileMachine(ExpansionContent.batteryBoxTile.get) with TPowerStorage with TGuiMachine with TInventory with ISidedInventory
{
    override protected val storage:Array[ItemStack] = Array.fill(2)(ItemStack.EMPTY)//new Array[ItemStack](2)

    override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        saveInv(tag)
    }

    override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        loadInv(tag)
        s = getStorageScaled(8)
    }

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerBatteryBox =
        new ContainerBatteryBox(playerInv, this, windowId)

    override def getMaxStackSize = 1
    override def nbtSaveName = "battery_box"

    override def getSlotsForFace(s:Direction):Array[Int] = s match {
        case Direction.UP => Array(0) // input
        case _ => Array(1) // output
    }

    override def canPlaceItemThroughFace(slot:Int, itemstack:ItemStack, side:Direction) = true
    override def canTakeItemThroughFace(slot:Int, itemstack:ItemStack, side:Direction) = true

    override def canPlaceItem(slot:Int, item:ItemStack):Boolean =
        !item.isEmpty && item.getItem.isInstanceOf[IChargable]

    override def onBlockPlaced(player:LivingEntity, stack:ItemStack):Unit = {
        super.onBlockPlaced(player, stack)
        if (stack.hasTag) {
            powerStored = stack.getTag.getInt("storage")
        }
    }

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(level, getBlockPos)
    }

    override def addHarvestContents(ist:ListBuffer[ItemStack]):Unit = {
        val stack = new ItemStack(ExpansionContent.batteryBoxBlock.get)
        if (powerStored > 0) {
            val tag = stack.getOrCreateTag()
            tag.putInt("storage", powerStored)
            tag.putInt("rstorage", getStorageScaled(8))
        }
        ist += stack
    }

    override def updateServer():Unit = {
        super.updateServer()

        tryChargeBattery()
        tryDischargeBattery()

        updateRendersIfNeeded()
    }

    private var s = 0
    def updateRendersIfNeeded():Unit = {
        val s2 = getStorageScaled(8)
        if (s != s2) {
            s = s2
            sendStorage()
            pushState()
        }
    }

    override def covertToBlockState(state: BlockState): BlockState = {
        state.setValue(BaseMachineBlock.CHARGE_LEVEL_PROPERTY, Int.box(s))
    }

    override def getMaxStorage = 8000
    override def getDrawSpeed = 100
    override def getDrawCeil = 900
    override def getDrawFloor = 800
    def getChargeSpeed = 25

    def tryDischargeBattery():Unit = {
        val stack = getItem(1)
        if (!stack.isEmpty) stack.getItem match {
            case b:IChargable =>
                val toDraw = math.min(getMaxStorage-powerStored, getChargeSpeed)
                val (newStack, drawn) = b.drawPower(stack, toDraw)
                setItem(1, newStack)
                powerStored += drawn
            case _ =>
        }
    }

    def tryChargeBattery():Unit = {
        val stack = getItem(0)
        if (!stack.isEmpty) stack.getItem match {
            case b:IChargable =>
                val toAdd = math.min(powerStored, getChargeSpeed)
                val (newStack, added) = b.addPower(stack, toAdd)
                setItem(0, newStack)
                powerStored -= added
            case _ =>
        }
    }
}

class ContainerBatteryBox(playerInv:PlayerInventory, val tile:TileBatteryBox, windowId:Int) extends ContainerPoweredMachine(tile, ExpansionContent.batteryBoxContainer.get(), windowId)
{
    {
        addSlot(new Slot3(tile, 0, 80, 31))
        addSlot(new Slot3(tile, 1, 80, 53))
        addPlayerInv(playerInv, 8, 89)
    }

    private var st = -1
    override def broadcastChanges()
    {
        super.broadcastChanges()
        for (i <- containerListeners.asScala) {
            if (st != tile.powerStored)
                i.setContainerData(this, 3, tile.powerStored)
        }
        st = tile.powerStored
    }

    override def setData(id:Int, bar:Int):Unit = id match {
        case 3 => tile.powerStored = bar
        case _ => super.setData(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (from == 0 || from == 1) {//if item in battery box
            if (tryMergeItemStack(stack, 2, 11, true)) return true //to hotbar
            if (tryMergeItemStack(stack, 11, 38, true)) return true //then to player inventory
        }

        stack.getItem match {
            case b:TChargableBatteryItem => tryMergeItemStack(stack, 0, 2, b.nonEmpty) //to discharge slot if battery not empty
            case _ => false
        }
    }
}

object ContainerBatteryBox extends ICCLContainerFactory[ContainerBatteryBox]
{
    override def create(windowId:Int, inv:PlayerInventory, packet:MCDataInput):ContainerBatteryBox = {
        inv.player.level.getBlockEntity(packet.readPos()) match {
            case t:TileBatteryBox => t.createMenu(windowId, inv, inv.player)
            case _ => null
        }
    }
}

class GuiBatteryBox(c:ContainerBatteryBox, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 171, playerInv, title)
{
    override def drawBack_Impl(stack:MatrixStack, mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiBatteryBox.background)
        blit(stack, 0, 0, 0, 0, size.width, size.height)

        if (c.tile.cond.canWork)
            blit(stack, 57, 16, 176, 1, 7, 9)
        GuiLib.drawVerticalTank(stack, this, 57, 26, 176, 10, 7, 48, c.tile.cond.getChargeScaled(48))

        if (c.tile.powerStored == c.tile.getMaxStorage)
            blit(stack, 112, 16, 184, 1, 14, 9)
        GuiLib.drawVerticalTank(stack, this, 112, 26, 184, 10, 14, 48, c.tile.getStorageScaled(48))

        if (c.tile.cond.charge > c.tile.getDrawCeil && c.tile.powerStored < c.tile.getMaxStorage)
            blit(stack, 65, 52, 199, 18, 48, 18)
        else if (c.tile.cond.charge < c.tile.getDrawFloor && c.tile.powerStored > 0)
            blit(stack, 65, 30, 199, 0, 48, 18)

        getFontRenderer.draw(stack, title, 8, 6, EnumColour.GRAY.argb)
        getFontRenderer.draw(stack, playerInv.getDisplayName, 8, 79, EnumColour.GRAY.argb)
    }
}

object GuiBatteryBox
{
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/battery_box.png")

    def register():Unit = {
        ScreenManager.register(
            ExpansionContent.batteryBoxContainer.get(),
            (cont:ContainerBatteryBox, inv, text) => new GuiBatteryBox(cont, inv, text))
    }

//    override def getID = ExpansionProxy.batteryBoxGui
//
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        player.world.getTileEntity(data.readPos) match
//        {
//            case t:TileBatteryBox => new GuiBatteryBox(t, t.createContainer(player))
//            case _ => null
//        }
//    }
}

//object RenderBatteryBox extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    import java.lang.{Integer => JInt}
//
//    var bottom:TextureAtlasSprite = _
//    var top:TextureAtlasSprite = _
//    val sides = new Array[TextureAtlasSprite](9)
//
//    override def handleState(state: IExtendedBlockState, world:IBlockAccess, pos:BlockPos): IExtendedBlockState = world.getTileEntity(pos) match {
//        case t:TileBatteryBox =>
//            state.withProperty(UNLISTED_CHARGE_PROPERTY, t.getStorageScaled(8).asInstanceOf[JInt])
//        case _ => state
//    }
//
//    override def getWorldTransforms(state: IExtendedBlockState) = {
//        val c = state.getValue(UNLISTED_CHARGE_PROPERTY)
//        val i = sides(c)
//        Triple.of(0, 0, new MultiIconTransformation(bottom, top, i, i, i, i))
//    }
//
//    override def getItemTransforms(stack: ItemStack) = {
//        val sideIcon:TextureAtlasSprite =
//            if(stack.hasTagCompound && stack.getTagCompound.hasKey("rstorage"))
//                sides(stack.getTagCompound.getInteger("rstorage")) else sides(0)
//
//        Triple.of(0,0, new MultiIconTransformation(bottom, top, sideIcon, sideIcon, sideIcon, sideIcon))
//    }
//
//    override def shouldCull() = true
//
//    def getIcon(side:Int, meta:Int) = side match
//    {
//        case 0 => bottom
//        case 1 => top
//        case _ => sides(0)
//    }
//
//    override def registerIcons(reg:TextureMap)
//    {
//        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/bottom"))
//        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/top"))
//        for (i <- 0 until 9)
//            sides(i) = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/batterybox/side"+i))
//    }
//}
