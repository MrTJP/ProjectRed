/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.inventory.container.ICCLContainerFactory
import codechicken.lib.texture.TextureUtils
import mrtjp.core.gui._
import mrtjp.core.inventory.{TInventory, TInventoryCapablilityTile}
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.item.PlanItem
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.inventory.ISidedInventory
import net.minecraft.inventory.container.Slot
import net.minecraft.item.ItemStack
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.text.ITextComponent
import net.minecraft.util.{Direction, ResourceLocation}

import scala.jdk.CollectionConverters._

class TileAutoCrafter extends TileMachine(ExpansionContent.autoCraftingBenchTile.get) with TPoweredMachine with TInventory with ISidedInventory with TGuiMachine with TInventoryCapablilityTile
{
    var planSlot = 0

    private var recipeNeedsRefresh = true
    val craftHelper = new CraftingResultTestHelper

    /**
     * 0 until 9 - Plans
     * 9 until 27 - Storage
     */
    override protected val storage:Array[ItemStack] = Array.fill(27)(ItemStack.EMPTY)

    private var cycleTimer1 = getUnpoweredCycleTimer
    private var cycleTimer2 = getPoweredCycleTimer

    override def save(tag:CompoundNBT):Unit = {
        super.save(tag)
        saveInv(tag)
        tag.putInt("cyt1", cycleTimer1)
        tag.putInt("cyt2", cycleTimer2)
    }

    override def load(tag:CompoundNBT):Unit = {
        super.load(tag)
        loadInv(tag)
        cycleTimer1 = tag.getInt("cyt1")
        cycleTimer2 = tag.getInt("cyt2")
    }

    override def readUpdate(key:Int, in:MCDataInput):Unit = key match {
        case 2 => cyclePlanSlot()
        case _ => super.readUpdate(key, in)
    }

    def sendCyclePlanSlot():Unit = {
        sendUpdate(2, _ => ())
    }

    def getUnpoweredCycleTimer = 40
    def getPoweredCycleTimer = 10
    def getCraftsPerPowerCycle = 5

    override def doesOrient = false
    override def doesRotate = false

    override def getInventoryStackLimit = 64
    override def nbtSaveName = "auto_crafting_bench"

    override def canExtractItem(slot:Int, item:ItemStack, side:Direction):Boolean = 9 until 27 contains slot
    override def canInsertItem(slot:Int, item:ItemStack, side:Direction):Boolean = 9 until 27 contains slot
    override def getSlotsForFace(side:Direction):Array[Int] = (9 until 27).toArray

    override def updateServer():Unit = {
        super.updateServer()

        if (cond.canWork) {
            cycleTimer2 -= 1
            if (cycleTimer2%(getPoweredCycleTimer/getCraftsPerPowerCycle) == 0)
                if (tryCraft()) cond.drawPower(1000)
            if (cycleTimer2 <= 0) {
                cycleTimer2 = getPoweredCycleTimer
                cyclePlanSlot()
                cond.drawPower(100)
            }
        } else {
            cycleTimer1 -= 1
            if (cycleTimer1 <= 0) {
                cycleTimer1 = getUnpoweredCycleTimer
                tryCraft()
            }
        }
    }

    def cyclePlanSlot():Unit = {
        val start = planSlot
        do planSlot = (planSlot+1)%9
        while (planSlot != start && getStackInSlot(planSlot).isEmpty)
        if (planSlot != start) refreshRecipe()
    }

    def refreshRecipe():Unit = {
        craftHelper.clear()

        val plan = getStackInSlot(planSlot)
        if (!plan.isEmpty && PlanItem.hasRecipeInside(plan)) {
            val inputs = PlanItem.loadPlanInputs(plan)
            craftHelper.loadInputs(inputs)
            craftHelper.findRecipeFromInputs(world)
        }
    }

    override def markDirty():Unit = {
        super.markDirty()
        recipeNeedsRefresh = true
    }

    def tryCraft():Boolean = {
        if (recipeNeedsRefresh) {
            refreshRecipe()
            recipeNeedsRefresh = false
        }

        if (craftHelper.recipe != null) {
            craftHelper.loadStorage((9 until 27).map(getStackInSlot).toArray, true)

            if (craftHelper.consumeAndCraftToStorage(world, 64)) {
                craftHelper.unloadStorage(this, {_ + 9})
                true
            } else
                false

        } else
            false
    }

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(world, getPos)
    }

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerAutoCrafter =
        new ContainerAutoCrafter(playerInv, this, windowId)
}

class ContainerAutoCrafter(player:PlayerInventory, val tile:TileAutoCrafter, windowId:Int) extends ContainerPoweredMachine(tile, ExpansionContent.autoCraftingBenchContainer.get, windowId)
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(98, 22, 3, 3, 0, 0).zipWithIndex) {
            val slot = new Slot(tile, i, x, y) {
                override def isItemValid(stack:ItemStack):Boolean =
                    stack.getItem.isInstanceOf[PlanItem] && PlanItem.hasRecipeInside(stack)
            }
            addSlot(slot)
        }

        for (((x, y), i) <- GuiLib.createSlotGrid(8, 80, 9, 2, 0, 0).zipWithIndex)
            addSlot(new Slot(tile, i+9, x, y))

        addPlayerInv(player, 8, 130)
    }

    private var slot = -1

    override def detectAndSendChanges():Unit = {
        super.detectAndSendChanges()
        for (i <- listeners.asScala) {
            if (slot != tile.planSlot) i.sendWindowProperty(this, 3, tile.planSlot)
            slot = tile.planSlot
        }
    }

    override def updateProgressBar(id:Int, bar:Int):Unit = id match {
        case 3 => tile.planSlot = bar
        case _ => super.updateProgressBar(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean = {
        if (0 until 9 contains from) //plan slots
        {
            if (tryMergeItemStack(stack, 36, 63, false)) return true //to player inv
            if (tryMergeItemStack(stack, 27, 36, false)) return true //to hotbar
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[PlanItem])
                if (tryMergeItemStack(stack, 0, 9, false)) return true //merge to plan

            if (tryMergeItemStack(stack, 27, 36, true)) return true //to hotbar reversed
            if (tryMergeItemStack(stack, 36, 63, true)) return true //to player inv reversed
        }
        else if (27 until 63 contains from) //player inventory
        {
            if (stack.getItem.isInstanceOf[PlanItem]) {
                if (tryMergeItemStack(stack, 0, 9, false)) return true //merge to plan
            } else
                if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
        }

        false
    }
}

object ContainerAutoCrafter extends ICCLContainerFactory[ContainerAutoCrafter]
{
    override def create(windowId:Int, inv:PlayerInventory, packet:MCDataInput):ContainerAutoCrafter = {
        inv.player.world.getTileEntity(packet.readPos()) match {
            case t:TileAutoCrafter => t.createMenu(windowId, inv, inv.player)
            case _ => null
        }
    }
}

class GuiAutoCrafter(c:ContainerAutoCrafter, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 212, playerInv, title)
{
    {
        val cycle = new IconButtonNode {
            override def drawButton(mouseover:Boolean) {
                TextureUtils.changeTexture(GuiAutoCrafter.background)
                blit(position.x, position.y, 176, 0, 14, 14)
            }
        }
        cycle.position = Point(59, 41)
        cycle.size = Size(14, 14)
        cycle.clickDelegate = {() => c.tile.sendCyclePlanSlot()}
        addChild(cycle)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        TextureUtils.changeTexture(GuiAutoCrafter.background)
        blit(0, 0, 0, 0, size.width, size.height)

        if (c.tile.cond.canWork)
            blit(16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(this, 16, 26, 177, 27, 7, 48, c.tile.cond.getChargeScaled(48))

        if (c.tile.cond.flow == -1)
            blit(27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(this, 27, 26, 185, 27, 7, 48, c.tile.cond.getFlowScaled(48))

        val plan = c.tile.getStackInSlot(c.tile.planSlot)
        if (!plan.isEmpty && PlanItem.hasRecipeInside(plan))
            ItemDisplayNode.renderItem(this, Point(152, 58), Size(16, 16), zPosition, true, PlanItem.loadPlanOutput(plan))

        getFontRenderer.drawString(title.getFormattedText, 8, 6, EnumColour.GRAY.argb)
        getFontRenderer.drawString(playerInv.getDisplayName.getFormattedText, 8, 120, EnumColour.GRAY.argb)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (Minecraft.getInstance().gameSettings.keyBindSneak.isPressed)
            drawPlanOutputOverlay(c.inventorySlots.asScala)

        TextureUtils.changeTexture(GuiAutoCrafter.background)

        val Point(sx, sy) = Point(18, 18).multiply(c.tile.planSlot%3, c.tile.planSlot/3).add(98, 22).subtract(3)
        blit(sx, sy, 193, 0, 22, 22)
    }

    private def drawPlanOutputOverlay(slots:Iterable[Slot]) {
        for (slot <- slots) if (slot.getHasStack) {
            val stack = slot.getStack
            if (PlanItem.hasRecipeInside(stack)) {
                val output = PlanItem.loadPlanOutput(stack)
                val colour = EnumColour.LIGHT_BLUE.argb(0xCC)
                fillGradient(slot.xPos, slot.yPos, slot.xPos+16, slot.yPos+16, colour, colour)
                ItemDisplayNode.renderItem(this, Point(slot.xPos+1, slot.yPos+1), Size(14, 14), 100, true, output)
            }
        }
    }
}

object GuiAutoCrafter
{
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/auto_crafting_bench.png")

    def register():Unit = {
        ScreenManager.registerFactory(
            ExpansionContent.autoCraftingBenchContainer.get(),
            (cont:ContainerAutoCrafter, inv, text) => new GuiAutoCrafter(cont, inv, text))
    }
}

//object RenderAutoCrafter extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    var bottom:TextureAtlasSprite = _
//    var top:TextureAtlasSprite = _
//    var side1:TextureAtlasSprite = _
//    var side2:TextureAtlasSprite = _
//
//    var iconT:UVTransformation = _
//
//    override def getWorldTransforms(state: IExtendedBlockState) = Triple.of(0, 0, iconT)
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT)
//    override def shouldCull() = true
//
//    def getIcon(side:Int, meta:Int) = side match
//    {
//        case 0 => bottom
//        case 1 => top
//        case _ => side1
//    }
//
//    override def registerIcons(reg:TextureMap)
//    {
//        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/bottom"))
//        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/top"))
//        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/side1"))
//        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/side2"))
//
//        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
//    }
//}
