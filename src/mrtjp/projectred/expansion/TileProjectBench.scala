/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import java.util.{List => JList}

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui._
import mrtjp.core.inventory.TInventory
import mrtjp.core.item.{ItemEquality, ItemKey}
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory._
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{CraftingManager, IRecipe}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.World
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.common.FMLCommonHandler
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.{ShapedOreRecipe, ShapelessOreRecipe}
import org.lwjgl.input.Keyboard

import scala.collection.JavaConversions._

class TileProjectBench extends TileMachine with TInventory with ISidedInventory with TGuiMachine {
    val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
    val invResult = new InventoryCraftResult

    var isPlanRecipe = false
    var currentRecipe: IRecipe = null
    var currentInputs = new Array[ItemStack](9)

    private var recipeNeedsUpdate = true

    override def save(tag: NBTTagCompound) {
        super.save(tag)
        saveInv(tag)
    }

    override def load(tag: NBTTagCompound) {
        super.load(tag)
        loadInv(tag)
    }

    override def read(in: MCDataInput, key: Int) = key match {
        case 1 => writePlan()
        case 2 => clearGrid(in.readInt())
        case _ => super.read(in, key)
    }

    def sendWriteButtonAction() {
        writeStream(1).sendToServer()
    }

    def sendClearGridAction(id: Int) {
        writeStream(2).writeInt(id).sendToServer()
    }

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    override def doesOrient = false

    override def size = 28

    //0-8 crafting, 9-26 ingredients, 27 plan, 28 result
    override def name = "project_bench"

    override def getDisplayName = super.getDisplayName

    override def canExtractItem(slot: Int, item: ItemStack, side: EnumFacing) = 9 until 27 contains slot

    override def canInsertItem(slot: Int, item: ItemStack, side: EnumFacing) = 9 until 27 contains slot

    override def getSlotsForFace(side: EnumFacing) = 9 until 27 toArray

    override def updateServer() {
        updateRecipeIfNeeded()
    }

    override def updateClient() {
        updateRecipeIfNeeded()
    }

    def updateRecipeIfNeeded() {
        if (!recipeNeedsUpdate) return
        recipeNeedsUpdate = false
        updateRecipe()
    }

    def updateRecipe() {
        isPlanRecipe = false
        currentRecipe = null
        currentInputs.transform(_ => null)
        invResult.setInventorySlotContents(0, null)

        if ((0 until 9).exists(getStackInSlot(_) != null)) {
            for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, getStackInSlot(i))
            matchAndSetRecipe()
        }
        else {
            val plan = getStackInSlot(27)
            if (plan != null && ItemPlan.hasRecipeInside(plan)) {
                val inputs = ItemPlan.loadPlanInputs(plan)
                for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, inputs(i))
                matchAndSetRecipe()
                if (currentRecipe != null) isPlanRecipe = true
            }
        }

        def matchAndSetRecipe() {
            val recipes = CraftingManager.getInstance().getRecipeList.asInstanceOf[JList[IRecipe]]
            currentRecipe = recipes.find(_.matches(invCrafting, world)).orNull
            if (currentRecipe != null) {
                invResult.setInventorySlotContents(0, currentRecipe.getCraftingResult(invCrafting))
                for (i <- 0 until 9)
                    currentInputs(i) = {
                        val s = invCrafting.getStackInSlot(i)
                        if (s != null) s.copy else null
                    }
            }
        }
    }

    def writePlan() {
        if (currentRecipe != null && !isPlanRecipe) {
            val out = invResult.getStackInSlot(0)
            if (out != null) {
                val stack = getStackInSlot(27)
                if (stack != null)
                    ItemPlan.savePlan(stack, (0 until 9).map(getStackInSlot).toArray, out)
            }
        }
    }

    def clearGrid(id: Int) {
        world.getEntityByID(id) match {
            case p: EntityPlayer => p.openContainer match {
                case c: ContainerProjectBench =>
                    c.transferAllFromGrid()
                case _ =>
            }
            case _ =>
        }
    }

    override def markDirty() {
        super.markDirty()
        recipeNeedsUpdate = true
    }

    override def onBlockRemoval() {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }

    override def openGui(player: EntityPlayer) {
        GuiProjectBench.open(player, createContainer(player), _.writePos(getPos))
    }

    override def createContainer(player: EntityPlayer) = new ContainerProjectBench(player, this)
}

class SlotProjectCrafting(player: EntityPlayer, tile: TileProjectBench, idx: Int, x: Int, y: Int)
    extends SlotCrafting(player, tile.invCrafting, tile.invResult, idx, x, y) with TSlot3 {
    override def canTakeStack(player: EntityPlayer): Boolean = {
        if (tile.isPlanRecipe) {
            val storage = (9 until 27).map { i =>
                val s = tile.getStackInSlot(i)
                if (s != null) s.copy else null
            }.toArray

            return searchFor(player.worldObj, tile.currentRecipe, tile.currentInputs, storage)
        }

        //copied from super for obfuscation bug
        canRemoveDelegate()
    }

    override def onPickupFromSlot(player: EntityPlayer, stack: ItemStack) {
        onCrafting(stack)

        val storage = ((9 until 27) ++ (0 until 9)).map { i =>
            val s = tile.getStackInSlot(i)
            if (s != null) s.copy else null
        }.toArray

        if (searchFor(player.worldObj, tile.currentRecipe, tile.currentInputs, storage)) {
            val orderedStorage = storage.drop(18) ++ storage.take(18)
            for (i <- orderedStorage.indices) {
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

    def searchFor(world: World, recipe: IRecipe, inputs: Array[ItemStack], storage: Array[ItemStack]): Boolean = {
        i = 0
        val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
        for (i <- 0 until 9) {
            val item = inputs(i)
            if (item != null) {
                if (!eatResource(recipe, item, storage)) return false
                invCrafting.setInventorySlotContents(i, item)
            }
        }
        recipe.matches(invCrafting, world)
    }

    private var i = 0

    private def eatResource(recipe: IRecipe, stack1: ItemStack, storage: Array[ItemStack]): Boolean = {
        def increment() = {
            i = (i + 1) % storage.length; i
        }

        if (i < 18) i = 0 else increment()
        val start = i
        do {
            val stack2 = storage(i)
            if (stack2 != null && ingredientMatch(recipe, stack1, stack2)) {
                if (stack2.getItem.hasContainerItem(stack2)) {
                    val cStack = stack2.getItem.getContainerItem(stack2)
                    storage(i) = if (cStack.getItemDamage < cStack.getMaxDamage) cStack else null
                    return true
                }
                else if (stack2.stackSize >= 1) {
                    stack2.stackSize -= 1
                    return true
                }
            }
        }
        while (increment() != start)
        false
    }

    private def ingredientMatch(recipe: IRecipe, stack1: ItemStack, stack2: ItemStack) = {
        val eq = new ItemEquality
        eq.matchMeta = !stack1.isItemStackDamageable
        eq.matchNBT = false
        eq.matchOre = recipe.isInstanceOf[ShapedOreRecipe] || recipe.isInstanceOf[ShapelessOreRecipe]
        eq.matches(ItemKey.get(stack1), ItemKey.get(stack2))
    }

    //Following 3 methods copy-pasted from TSlot3 for obfuscation issues
    override def getSlotStackLimit: Int = slotLimitCalculator()

    override def isItemValid(stack: ItemStack): Boolean = canPlaceDelegate(stack)

    override def onSlotChanged() {
        super.onSlotChanged()
        slotChangeDelegate()
        slotChangeDelegate2()
    }
}

class ContainerProjectBench(player: EntityPlayer, tile: TileProjectBench) extends NodeContainer {
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex)
            addSlotToContainer(new Slot3(tile, i, x, y))

        for (((x, y), i) <- GuiLib.createSlotGrid(8, 76, 9, 2, 0, 0).zipWithIndex)
            addSlotToContainer(new Slot3(tile, i + 9, x, y))

        val plan = new Slot3(tile, 27, 17, 36)
        plan.canPlaceDelegate = {
            _.getItem.isInstanceOf[ItemPlan]
        }
        plan.slotLimitCalculator = { () => 1 }
        addSlotToContainer(plan)

        val output = new SlotProjectCrafting(player, tile, 28, 143, 36)
        output.canPlaceDelegate = { _ => false }
        addSlotToContainer(output)

        addPlayerInv(player, 8, 126)
    }

    def transferAllFromGrid() {
        for (i <- 0 until 9) if (getSlot(i).getHasStack)
            transferStackInSlot(player, i)
        detectAndSendChanges()
    }

    override def slotClick(id: Int, mouse: Int, shift: ClickType, player: EntityPlayer) = {
        var mode = shift
        if (id == 28 && mode == ClickType.PICKUP_ALL) mode = ClickType.PICKUP
        super.slotClick(id, mouse, mode, player)
    }

    override def doMerge(stack: ItemStack, from: Int): Boolean = {
        if (0 until 9 contains from) //crafting grid
        {
            if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
            if (tryMergeItemStack(stack, 29, 65, false)) return true //merge to inventory)
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) {
                if (getSlot(27).getStack != null && ItemKey.get(getSlot(27).getStack) != ItemKey.get(stack))
                    transferStackInSlot(player, 27) //transfer existing stack

                if (tryMergeItemStack(stack, 27, 28, false)) return true //merge to plan
            }
            if (tryMergeItemStack(stack, 29, 65, false)) return true //merge to inventory
        }
        else if (from == 27) //plan slot
        {
            if (tryMergeItemStack(stack, 9, 27, true)) return true //merge to storage
            if (tryMergeItemStack(stack, 29, 65, false)) return true //merge to inventory)
        }
        else if (from == 28) //output slot
        {
            if (tryMergeItemStack(stack, 29, 65, true)) return true //merge to inventory
            if (tryMergeItemStack(stack, 9, 27, true)) return true //merge to storage
        }
        else if (29 until 65 contains from) //player inventory
        {
            if (stack.getItem.isInstanceOf[ItemPlan]) {
                if (getSlot(27).getStack != null && ItemKey.get(getSlot(27).getStack) != ItemKey.get(stack))
                    transferStackInSlot(player, 27) //transfer existing stack

                if (tryMergeItemStack(stack, 27, 28, false)) return true //merge to plan
            }
            if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
        }

        false
    }
}

class GuiProjectBench(tile: TileProjectBench, c: ContainerProjectBench) extends NodeGui(c, 176, 208) {
    {
        val write = new IconButtonNode {
            override def drawButton(mouseover: Boolean) {
                TextureUtils.changeTexture(GuiProjectBench.background)
                GuiDraw.drawTexturedModalRect(position.x, position.y, 176, 0, 14, 14)
            }
        }
        write.position = Point(18, 56)
        write.size = Size(14, 14)
        write.clickDelegate = { () => tile.sendWriteButtonAction() }
        addChild(write)

        val clear = new IconButtonNode {
            override def drawButton(mouseover: Boolean) {
                TextureUtils.changeTexture(GuiProjectBench.background)
                GuiDraw.drawTexturedModalRect(position.x, position.y, 176, 15, 8, 8)
            }
        }
        clear.position = Point(37, 17)
        clear.size = Size(8, 8)
        clear.clickDelegate = { () => tile.sendClearGridAction(Minecraft.getMinecraft.thePlayer.getEntityId) }
        addChild(clear)
    }

    override def drawBack_Impl(mouse: Point, rframe: Float) {
        TextureUtils.changeTexture(GuiProjectBench.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        if (tile.isPlanRecipe) for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex) {
            val stack = tile.currentInputs(i)
            if (stack != null) {
                GuiDraw.drawRect(x, y, 16, 16, EnumColour.GRAY.argb)
                ItemDisplayNode.renderItem(Point(x, y), Size(16, 16), zPosition, false, stack)
            }
        }

        GuiDraw.drawString("Project Bench", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 116, EnumColour.GRAY.argb, false)
    }

    override def drawFront_Impl(mouse: Point, rframe: Float) {
        if (Keyboard.isKeyDown(Keyboard.KEY_LSHIFT) || Keyboard.isKeyDown(Keyboard.KEY_RSHIFT))
            GuiProjectBench.drawPlanOutputOverlay(c.slots)
    }
}

object GuiProjectBench extends TGuiBuilder {
    val background = new ResourceLocation("projectred", "textures/gui/project_bench.png")

    override def getID = ExpansionProxy.projectbenchGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player: EntityPlayer, data: MCDataInput) = {
        player.worldObj.getTileEntity(data.readPos) match {
            case t: TileProjectBench => new GuiProjectBench(t, t.createContainer(player))
            case _ => null
        }
    }

    def drawPlanOutputOverlay(slots: Iterable[TSlot3]) {
        for (slot <- slots) if (slot.getHasStack) {
            val stack = slot.getStack
            if (ItemPlan.hasRecipeInside(stack)) {
                val output = ItemPlan.loadPlanOutput(stack)
                GuiDraw.drawRect(slot.xDisplayPosition, slot.yDisplayPosition, 16, 16, EnumColour.LIGHT_BLUE.argb(0xCC))
                ItemDisplayNode.renderItem(Point(slot.xDisplayPosition + 1, slot.yDisplayPosition + 1), Size(14, 14), 0, true, output)
            }
        }
    }
}

object RenderProjectBench extends SimpleBlockRenderer {
    val instance = RenderProjectBench

    import mrtjp.core.util.CCLConversions._
    var bottom: TextureAtlasSprite = _
    var top: TextureAtlasSprite = _
    var side1: TextureAtlasSprite = _
    var side2: TextureAtlasSprite = _

    var iconT: UVTransformation = _



    override def getWorldTransforms(state: IExtendedBlockState) = createTriple(0, 0, iconT)
    override def getItemTransforms(stack: ItemStack) = createTriple(0, 0, iconT)
    override def shouldCull() = true

    override def registerIcons(map: TextureMap) {
        bottom = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/bottom"))
        top = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/top"))
        side1 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/side1"))
        side2 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/side2"))

        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
    }
}
