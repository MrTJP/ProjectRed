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
import cpw.mods.fml.common.FMLCommonHandler
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.item.{ItemEquality, ItemKey, ItemKeyStack}
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.{Point, Size}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{ISidedInventory, InventoryCraftResult, InventoryCrafting, SlotCrafting}
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, IRecipe}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}

import scala.collection.JavaConversions._

class TileProjectBench extends TileMachine with TInventory with ISidedInventory with TGuiMachine
{
    val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
    val invResult = new InventoryCraftResult

    var isPlanRecipe = false
    var currentRecipe:IRecipe = null
    var currentInputs = new Array[ItemStack](9)

    private var recipeNeedsUpdate = false

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 => writePlan()
        case _ => super.read(in, key)
    }

    def sendWriteButtonAction()
    {
        writeStream(1).sendToServer()
    }

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false
    override def doesOrient = false

    override def size = 28 //0-8 crafting, 9-26 ingredients, 27 plan, 28 result
    override def name = "project_bench"

    override def canExtractItem(slot:Int, item:ItemStack, side:Int) = 9 until 27 contains slot
    override def canInsertItem(slot:Int, item:ItemStack, side:Int) = 9 until 27 contains slot
    override def getAccessibleSlotsFromSide(side:Int) = 9 until 27 toArray

    override def update(){updateRecipeIfNeeded()}
    override def updateClient(){updateRecipeIfNeeded()}

    def updateRecipeIfNeeded()
    {
        if (!recipeNeedsUpdate) return
        recipeNeedsUpdate = false
        updateRecipe()
    }

    def updateRecipe()
    {
        isPlanRecipe = false
        currentRecipe = null
        currentInputs.transform(null)
        invResult.setInventorySlotContents(0, null)

        if ((0 until 9).exists(getStackInSlot(_) != null))
        {
            for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, getStackInSlot(i))
            matchAndSetRecipe()
        }
        else
        {
            val plan = getStackInSlot(27)
            if (plan != null && ItemPlan.hasRecipeInside(plan))
            {
                val inputs = ItemPlan.loadPlanInputs(plan)
                for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, inputs(i))
                matchAndSetRecipe()
                if (currentRecipe != null) isPlanRecipe = true
            }
        }

        def matchAndSetRecipe()
        {
            val recipes = CraftingManager.getInstance().getRecipeList.asInstanceOf[JList[IRecipe]]
            currentRecipe = recipes.find(_.matches(invCrafting, world)).orNull
            if (currentRecipe != null)
            {
                invResult.setInventorySlotContents(0, currentRecipe.getCraftingResult(invCrafting))
                for (i <- 0 until 9)
                    currentInputs(i) = {
                        val s = invCrafting.getStackInSlot(i)
                        if (s != null) s.copy else null
                    }
            }
        }
    }

    def writePlan()
    {
        if (currentRecipe != null && !isPlanRecipe)
        {
            val out = invResult.getStackInSlot(0)
            if (out != null)
            {
                val stack = getStackInSlot(27)
                if (stack != null)
                    ItemPlan.savePlan(stack, (0 until 9).map(getStackInSlot).toArray, out)
            }
        }
    }

    override def markDirty()
    {
        super.markDirty()
        recipeNeedsUpdate = true
    }

    override def openGui(player:EntityPlayer)
    {
        GuiProjectBench.open(player, createContainer(player), _.writeCoord(x, y, z))
    }

    override def createContainer(player:EntityPlayer) = new ContainerProjectBench(player, this)
}

class SlotProjectCrafting(player:EntityPlayer, tile:TileProjectBench, idx:Int, x:Int, y:Int)
        extends SlotCrafting(player, tile.invCrafting, tile.invResult, idx, x, y) with TSlot3
{
    override def canTakeStack(player:EntityPlayer):Boolean =
    {
        if (tile.isPlanRecipe)
        {
            val storage = (9 until 27).map {i =>
                val s = tile.getStackInSlot(i)
                if (s != null) s.copy else null
            }.toArray

            return RecipeSearch.searchFor(player.worldObj, tile.currentRecipe, tile.currentInputs, storage)
        }
        super.canTakeStack(player)
    }

    override def onPickupFromSlot(player:EntityPlayer, stack:ItemStack)
    {
        onCrafting(stack)

        val storage = ((9 until 27)++(0 until 9)).map {i =>
            val s = tile.getStackInSlot(i)
            if (s != null) s.copy else null
        }.toArray

        if (RecipeSearch.searchFor(player.worldObj, tile.currentRecipe, tile.currentInputs, storage))
        {
            val orderedStorage = storage.drop(18)++storage.take(18)
            for (i <- orderedStorage.indices)
            {
                val stack = orderedStorage(i)
                if (stack == null || stack.stackSize <= 0)
                    tile.setInventorySlotContents(i, null)
                else tile.setInventorySlotContents(i, stack)
            }
        }

        val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
        for (i <- 0 until 9)
            invCrafting.setInventorySlotContents(i, tile.currentInputs(i))
        FMLCommonHandler.instance().firePlayerCraftingEvent(player, stack, invCrafting)

        tile.updateRecipe()
    }
}

object RecipeSearch
{
    def searchFor(world:World, recipe:IRecipe, inputs:Array[ItemStack], storage:Array[ItemStack]):Boolean =
    {
        val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
        for (i <- 0 until 9)
        {
            val item = inputs(i)
            if (item != null)
            {
                if (!eatResource(recipe, item, storage)) return false
                invCrafting.setInventorySlotContents(i, item)
            }
        }
        recipe.matches(invCrafting, world)
    }

    def eatResource(recipe:IRecipe, stack1:ItemStack, storage:Array[ItemStack]):Boolean =
    {
        for (i <- storage.indices)
        {
            val stack2 = storage(i)
            if (stack2 != null && ingredientMatch(recipe, stack1, stack2))
            {
                if (stack2.getItem.hasContainerItem(stack2))
                {
                    storage(i) = stack2.getItem.getContainerItem(stack2)
                    return true
                }
                else if (stack2.stackSize >= 1)
                {
                    stack2.stackSize -= 1
                    return true
                }
            }
        }
        false
    }

    def ingredientMatch(recipe:IRecipe, stack1:ItemStack, stack2:ItemStack) =
    {
        val eq = new ItemEquality
        eq.matchMeta = !stack1.isItemStackDamageable
        eq.matchNBT = false
        eq.matchOre = recipe.isInstanceOf[ShapedOreRecipe] || recipe.isInstanceOf[ShapelessOreRecipe]
        eq.matches(ItemKey.get(stack1), ItemKey.get(stack2))
    }
}

class ContainerProjectBench(player:EntityPlayer, tile:TileProjectBench) extends NodeContainer
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex)
            addSlotToContainer(new Slot3(tile, i, x, y))

        for (((x, y), i) <- GuiLib.createSlotGrid(8, 76, 9, 2, 0, 0).zipWithIndex)
            addSlotToContainer(new Slot3(tile, i+9, x, y))

        val plan = new Slot3(tile, 27, 17, 36)
        plan.canPlaceDelegate = {_.getItem.isInstanceOf[ItemPlan]}
        plan.slotLimitCalculator = {() => 1}
        addSlotToContainer(plan)

        val output = new SlotProjectCrafting(player, tile, 28, 143, 36)
        output.canPlaceDelegate = {_ => false}
        addSlotToContainer(output)

        addPlayerInv(player, 8, 120)
    }

    override def slotClick(id:Int, mouse:Int, shift:Int, player:EntityPlayer) =
    {
        var mode = shift
        if (id == 28 && mode == 6) mode = 0
        super.slotClick(id, mouse, mode, player)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (0 until 9 contains from) //crafting grid
        {
            tryMergeItemStack(stack, 9, 27, false) //merge to storage
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) tryMergeItemStack(stack, 27, 28, false) //merge to plan
            else tryMergeItemStack(stack, 29, 65, false) //merge to inventory
        }
        else if (from == 27) //plan slot
        {
            tryMergeItemStack(stack, 9, 27, false) //merge to storage
        }
        else if (from == 28) //output slot
        {
            tryMergeItemStack(stack, 29, 65, true) //merge to inventory
        }
        else if (29 until 65 contains from) //player inventory
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) tryMergeItemStack(stack, 27, 28, false) //merge to plan
            else tryMergeItemStack(stack, 9, 27, false) //merge to storage
        }
        else false
    }
}

class GuiProjectBench(tile:TileProjectBench, c:ContainerProjectBench) extends NodeGui(c, 176, 202)
{
    var displayStacks = Seq.empty[ItemDisplayNode]

    {
        val write = new IconButtonNode {
            override def drawButton(mouseover:Boolean) {
                PRResources.guiProjectbench.bind()
                drawTexturedModalRect(position.x, position.y, 176, 0, 14, 14)
            }
        }
        write.position = Point(18, 56)
        write.size = Size(14, 14)
        write.clickDelegate = {() => tile.sendWriteButtonAction()}
        addChild(write)

        for ((x, y) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0))
        {
            val d = new ItemDisplayNode
            d.position = Point(x, y)
            d.size = Size(16, 16)
            d.hidden = true
            d.backgroundColour = Colors.GREY.argb
            d.drawNumber = false
            d.drawTooltip = false
            addChild(d)
            displayStacks :+= d
        }
    }

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        val inputs = if (tile.isPlanRecipe) tile.currentInputs
        else null

        for (i <- 0 until 9)
        {
            val disp = displayStacks(i)
            val stack = if (inputs == null) null else inputs(i)
            if (stack == null)
            {
                disp.hidden = true
                disp.stack = null
            }
            else
            {
                disp.hidden = false
                disp.stack = ItemKeyStack.get(stack)
            }
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        PRResources.guiProjectbench.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)
        GuiDraw.drawString("Project Bench", 8, 6, Colors.GREY.argb, false)
    }
}

object GuiProjectBench extends TGuiBuilder
{
    override def getID = ExpansionProxy.projectbenchGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileProjectBench => new GuiProjectBench(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderProjectBench extends TCubeMapRender
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
        bottom = reg.registerIcon("projectred:mechanical/projectbench/bottom")
        top = reg.registerIcon("projectred:mechanical/projectbench/top")
        side1 = reg.registerIcon("projectred:mechanical/projectbench/side1")
        side2 = reg.registerIcon("projectred:mechanical/projectbench/side2")

        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
    }
}