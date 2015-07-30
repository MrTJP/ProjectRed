/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.{InvWrapper, TInventory}
import mrtjp.core.item.{ItemEquality, ItemKey, ItemKeyStack, ItemQueue}
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.{Point, Size}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{ICrafting, ISidedInventory, InventoryCrafting}
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, IRecipe}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}
import org.lwjgl.input.Keyboard

import scala.collection.JavaConversions._

class TileAutoCrafter extends TileMachine with TPoweredMachine with TInventory with ISidedInventory with TGuiMachine
{
    var planSlot = 0

    var currentRecipe:IRecipe = null
    var currentInputs = new ItemQueue
    var currentOutput:ItemKeyStack = null

    private val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
    private var recipeNeedsRefresh = true
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
    override def size = 27
    override def name = "auto_bench"

    override def canExtractItem(slot:Int, item:ItemStack, side:Int) = 9 until 27 contains slot
    override def canInsertItem(slot:Int, item:ItemStack, side:Int) = 9 until 27 contains slot
    override def getAccessibleSlotsFromSide(side:Int) = (9 until 27).toArray

    override def update()
    {
        super.update()

        if (recipeNeedsRefresh)
        {
            refreshRecipe()
            recipeNeedsRefresh = false
        }

        if (cond.canWork)
        {
            cycleTimer2 -= 1
            if (cycleTimer2%(getPoweredCycleTimer/getCraftsPerPowerCycle) == 0)
                if (tryCraft()) cond.drawPower(1000)
            if (cycleTimer2 <= 0)
            {
                cycleTimer2 = getPoweredCycleTimer
                cyclePlanSlot()
                cond.drawPower(100)
            }
        }
        else
        {
            cycleTimer1 -= 1
            if (cycleTimer1 <= 0)
            {
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
        currentRecipe = null
        currentInputs.clear()
        currentOutput = null

        val plan = getStackInSlot(planSlot)
        if (plan != null && ItemPlan.hasRecipeInside(plan))
        {
            val inputs = ItemPlan.loadPlanInputs(plan)
            for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, inputs(i))
            val recipes = CraftingManager.getInstance().getRecipeList.asInstanceOf[JList[IRecipe]]
            currentRecipe = recipes.find(_.matches(invCrafting, world)).orNull
            if (currentRecipe != null)
            {
                inputs.map{ItemKey.getOrNull}.filter(_ != null).foreach(currentInputs.add(_, 1))
                currentOutput = ItemKeyStack.getOrNull(currentRecipe.getCraftingResult(invCrafting))
            }
        }
    }

    override def markDirty()
    {
        super.markDirty()
        recipeNeedsRefresh = true
    }

    def tryCraft():Boolean =
    {
        if (currentRecipe != null && checkSpaceForOutput)
            if (currentInputs.result.forall(p => containsEnoughResource(p._1, p._2)))
            {
                for ((item, amount) <- currentInputs.result)
                    eatResource(item, amount)
                produceOutput()
                return true
            }
        false
    }

    def containsEnoughResource(item:ItemKey, amount:Int):Boolean =
    {
        val eq = new ItemEquality
        eq.matchMeta = !item.makeStack(0).isItemStackDamageable
        eq.matchNBT = false
        eq.matchOre = currentRecipe.isInstanceOf[ShapedOreRecipe] || currentRecipe.isInstanceOf[ShapelessOreRecipe]

        var found = 0
        for (i <- 9 until 27)
        {
            val s = getStackInSlot(i)
            if (s != null && eq.matches(item, ItemKey.get(s)))
            {
                found += s.stackSize
                if (found >= amount) return true
            }
        }
        false
    }

    def checkSpaceForOutput =
    {
        val w = InvWrapper.wrap(this).setInternalMode(true).setSlotsFromRange(9 until 27)
        w.getSpaceForItem(currentOutput.key) >= currentOutput.stackSize
    }

    def produceOutput()
    {
        val w = InvWrapper.wrap(this).setInternalMode(true).setSlotsFromRange(9 until 27)
        w.injectItem(currentOutput.key, currentOutput.stackSize)
    }

    def eatResource(item:ItemKey, amount:Int)
    {
        val eq = new ItemEquality
        eq.matchMeta = !item.makeStack(0).isItemStackDamageable
        eq.matchNBT = false
        eq.matchOre = currentRecipe.isInstanceOf[ShapedOreRecipe] || currentRecipe.isInstanceOf[ShapelessOreRecipe]

        var left = amount
        for (i <- 9 until 27)
        {
            val s = getStackInSlot(i)
            if (s != null && eq.matches(item, ItemKey.get(s)))
            {
                val toRem = math.min(s.stackSize, left)
                s.stackSize -= toRem
                left -= toRem
                if (s.stackSize <= 0) setInventorySlotContents(i, null) else markDirty()
                if (left <= 0) return
            }
        }
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, x, y, z)
    }

    override def openGui(player:EntityPlayer)
    {
        GuiAutoCrafter.open(player, createContainer(player), _.writeCoord(x, y, z))
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
        for (i <- crafters)
        {
            val ic = i.asInstanceOf[ICrafting]

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
            tryMergeItemStack(stack, 27, 63, false) //merge to inventory
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) tryMergeItemStack(stack, 0, 9, false) //merge to plan
            else tryMergeItemStack(stack, 27, 63, false) //merge to inventory
        }
        else if (27 until 63 contains from) //player inventory
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) tryMergeItemStack(stack, 0, 9, false) //merge to plan
            else tryMergeItemStack(stack, 9, 27, false) //merge to storage
        }
        else false
    }
}

class GuiAutoCrafter(tile:TileAutoCrafter, c:ContainerAutoCrafter) extends NodeGui(c, 176, 212)
{
    {
        val cycle = new IconButtonNode {
            override def drawButton(mouseover:Boolean) {
                PRResources.guiAutoCrafter.bind()
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
        PRResources.guiAutoCrafter.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        val Point(sx, sy) = Point(18, 18).multiply(tile.planSlot%3, tile.planSlot/3).add(98, 22).subtract(3)
        GuiDraw.drawTexturedModalRect(sx, sy, 193, 0, 22, 22)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(16, 26, 177, 27, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.cond.flow == -1)
            GuiDraw.drawTexturedModalRect(27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(27, 26, 185, 27, 7, 48, tile.cond.getFlowScaled(48))

        val plan = tile.getStackInSlot(tile.planSlot)
        if (plan != null && ItemPlan.hasRecipeInside(plan))
            ItemDisplayNode.renderItem(Point(152, 58), Size(16, 16), zPosition, true, ItemPlan.loadPlanOutput(plan))

        GuiDraw.drawString("Auto Crafting Bench", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 120, Colors.GREY.argb, false)
    }

    override def drawFront_Impl(mouse:Point, rframe:Float)
    {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            GuiProjectBench.drawPlanOutputOverlay(c.slots)
    }
}

object GuiAutoCrafter extends TGuiBuilder
{
    override def getID = ExpansionProxy.autoCrafterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileAutoCrafter => new GuiAutoCrafter(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderAutoCrafter extends TCubeMapRender
{
    var bottom:IIcon = _
    var top:IIcon = _
    var side1:IIcon = _
    var side2:IIcon = _

    var iconT:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) = (0, 0, iconT)
    override def getInvData = (0, 0, iconT)

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:mechanical/autobench/bottom")
        top = reg.registerIcon("projectred:mechanical/autobench/top")
        side1 = reg.registerIcon("projectred:mechanical/autobench/side1")
        side2 = reg.registerIcon("projectred:mechanical/autobench/side2")

        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
    }
}