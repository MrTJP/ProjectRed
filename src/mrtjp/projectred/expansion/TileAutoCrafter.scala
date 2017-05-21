/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.util.CCLConversions
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{IContainerListener, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import org.lwjgl.input.Keyboard

class TileAutoCrafter extends TileMachine with TPoweredMachine with TInventory with ISidedInventory with TGuiMachine
{
    var planSlot = 0

    private var recipeNeedsRefresh = true
    val craftHelper = new CraftingResultTestHelper

    private var cycleTimer1 = getUnpoweredCycleTimer
    private var cycleTimer2 = getPoweredCycleTimer

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
        tag.setInteger("cyt1", cycleTimer1)
        tag.setInteger("cyt2", cycleTimer2)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
        cycleTimer1 = tag.getInteger("cyt1")
        cycleTimer2 = tag.getInteger("cyt2")
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 2 => cyclePlanSlot()
        case _ => super.read(in, key)
    }

    def sendCyclePlanSlot()
    {
        writeStream(2).sendToServer()
    }

    def getUnpoweredCycleTimer = 40
    def getPoweredCycleTimer = 10
    def getCraftsPerPowerCycle = 5

    override def getBlock = ProjectRedExpansion.machine2

    override def doesOrient = false
    override def doesRotate = false

    /**
     * 0 until 9 - Plans
     * 9 until 27 - Storage
     */
    override protected val storage = new Array[ItemStack](27)

    override def getInventoryStackLimit = 64

    override def getName = "auto_bench"

    override def getDisplayName = super.getDisplayName
    override def canExtractItem(slot:Int, item:ItemStack, side:EnumFacing) = 9 until 27 contains slot
    override def canInsertItem(slot:Int, item:ItemStack, side:EnumFacing) = 9 until 27 contains slot
    override def getSlotsForFace(side:EnumFacing) = (9 until 27).toArray

    override def updateServer()
    {
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

    def cyclePlanSlot()
    {
        val start = planSlot
        do planSlot = (planSlot+1)%9
        while (planSlot != start && getStackInSlot(planSlot) == null)
        if (planSlot != start) refreshRecipe()
    }

    def refreshRecipe()
    {
        craftHelper.clear()

        val plan = getStackInSlot(planSlot)
        if (plan != null && ItemPlan.hasRecipeInside(plan)) {
            val inputs = ItemPlan.loadPlanInputs(plan)

            craftHelper.loadInputs(inputs)
            craftHelper.findRecipeFromInputs(world)
        }
    }

    override def markDirty()
    {
        super.markDirty()
        recipeNeedsRefresh = true
    }

    def tryCraft():Boolean =
    {
        if (recipeNeedsRefresh) {
            refreshRecipe()
            recipeNeedsRefresh = false
        }

        if (craftHelper.recipe != null) {
            craftHelper.loadStorage((9 until 27).map(getStackInSlot).toArray, true)
            if (craftHelper.consumeAndCraftToStorage(world, 64))
                craftHelper.unloadStorage(this, {_ + 9})
            return true
        }

        false
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }

    override def openGui(player:EntityPlayer)
    {
        GuiAutoCrafter.open(player, createContainer(player), _.writePos(getPos))
    }

    override def createContainer(player:EntityPlayer) = new ContainerAutoCrafter(player, this)
}

class ContainerAutoCrafter(player:EntityPlayer, tile:TileAutoCrafter) extends ContainerPoweredMachine(tile)
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(98, 22, 3, 3, 0, 0).zipWithIndex)
        {
            val s = new Slot3(tile, i, x, y)
            s.canPlaceDelegate = {s =>
                s.getItem.isInstanceOf[ItemPlan] && ItemPlan.hasRecipeInside(s)
            }
            addSlotToContainer(s)
        }

        for (((x, y), i) <- GuiLib.createSlotGrid(8, 80, 9, 2, 0, 0).zipWithIndex)
            addSlotToContainer(new Slot3(tile, i+9, x, y))

        addPlayerInv(player, 8, 130)
    }

    var slot = -1

    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()
        import scala.collection.JavaConversions._
        for (i <- listeners)
        {
            val ic = i.asInstanceOf[IContainerListener]

            if (slot != tile.planSlot) ic.sendProgressBarUpdate(this, 3, tile.planSlot)
            slot = tile.planSlot
        }
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 3 => tile.planSlot = bar
        case _ => super.updateProgressBar(id, bar)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (0 until 9 contains from) //plan slots
        {
            if (tryMergeItemStack(stack, 36, 63, false)) return true //to player inv
            if (tryMergeItemStack(stack, 27, 36, false)) return true //to hotbar
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[ItemPlan])
                if (tryMergeItemStack(stack, 0, 9, false)) return true //merge to plan

            if (tryMergeItemStack(stack, 27, 36, true)) return true //to hotbar reversed
            if (tryMergeItemStack(stack, 36, 63, true)) return true //to player inv reversed
        }
        else if (27 until 63 contains from) //player inventory
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) {
                if (tryMergeItemStack(stack, 0, 9, false)) return true //merge to plan
            } else
                if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
        }

        false
    }
}

class GuiAutoCrafter(tile:TileAutoCrafter, c:ContainerAutoCrafter) extends NodeGui(c, 176, 212)
{
    {
        val cycle = new IconButtonNode {
            override def drawButton(mouseover:Boolean) {
                TextureUtils.changeTexture(GuiAutoCrafter.background)
                drawTexturedModalRect(position.x, position.y, 176, 0, 14, 14)
            }
        }
        cycle.position = Point(59, 41)
        cycle.size = Size(14, 14)
        cycle.clickDelegate = {() => tile.sendCyclePlanSlot()}
        addChild(cycle)
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        TextureUtils.changeTexture(GuiAutoCrafter.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(16, 26, 177, 27, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.cond.flow == -1)
            GuiDraw.drawTexturedModalRect(27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(27, 26, 185, 27, 7, 48, tile.cond.getFlowScaled(48))

        val plan = tile.getStackInSlot(tile.planSlot)
        if (plan != null && ItemPlan.hasRecipeInside(plan))
            ItemDisplayNode.renderItem(Point(152, 58), Size(16, 16), zPosition, true, ItemPlan.loadPlanOutput(plan))

        GuiDraw.drawString("Auto Crafting Bench", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 120, EnumColour.GRAY.argb, false)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            GuiProjectBench.drawPlanOutputOverlay(c.slots)

        TextureUtils.changeTexture(GuiAutoCrafter.background)

        val Point(sx, sy) = Point(18, 18).multiply(tile.planSlot%3, tile.planSlot/3).add(98, 22).subtract(3)
        GuiDraw.drawTexturedModalRect(sx, sy, 193, 0, 22, 22)
    }
}

object GuiAutoCrafter extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/auto_bench.png")

    override def getID = ExpansionProxy.autoCrafterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.worldObj.getTileEntity(data.readPos()) match
        {
            case t:TileAutoCrafter => new GuiAutoCrafter(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderAutoCrafter extends SimpleBlockRenderer
{
    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var side2:TextureAtlasSprite = _

    var iconT:UVTransformation = _

    override def getWorldTransforms(state: IExtendedBlockState) = CCLConversions.createTriple(0, 0, iconT)
    override def getItemTransforms(stack: ItemStack) = CCLConversions.createTriple(0, 0, iconT)
    override def shouldCull() = true

    def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => side1
    }

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/top"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/side1"))
        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/autobench/side2"))

        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
    }
}
