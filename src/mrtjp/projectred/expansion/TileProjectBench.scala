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
import mrtjp.core.inventory.{ArrayWrapInventory, InvWrapper, TInventory}
import mrtjp.core.item.ItemKey
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
import org.lwjgl.input.Keyboard

import scala.collection.JavaConversions._

class TileProjectBench extends TileMachine with TInventory with ISidedInventory with TGuiMachine
{
    val craftHelper = new CraftingResultTestHelper
    var isPlanRecipe = false

    private var recipeNeedsUpdate = true
//    var currentRecipe:IRecipe = null
//    var currentInputs = new Array[ItemStack](9)

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

    //0-8 crafting, 9-26 ingredients, 27 plan, 28 result
    override protected val storage = new Array[ItemStack](28)

    override def getInventoryStackLimit = 64

    override def getName = "project_bench"

    override def getDisplayName = super.getDisplayName

    override def canExtractItem(slot: Int, item: ItemStack, side: EnumFacing) = 9 until 27 contains slot

    override def canInsertItem(slot: Int, item: ItemStack, side: EnumFacing) = 9 until 27 contains slot

    override def getSlotsForFace(side: EnumFacing) = 9 until 27 toArray

    override def updateServer() {
        updateRecipeIfNeeded()
        transferExcessToStorage()
    }

    override def updateClient() {
//        updateRecipeIfNeeded()
    }

    def updateRecipeIfNeeded() {
        if (!recipeNeedsUpdate) return
        recipeNeedsUpdate = false
        updateRecipe()
    }

    def updateRecipe() {
        isPlanRecipe = false
//        currentRecipe = null
//        currentInputs.transform(_ => null)
//        invResult.setInventorySlotContents(0, null)
        craftHelper.clear()

        if ((0 until 9).exists(getStackInSlot(_) != null)) {
            craftHelper.loadInputs((0 until 9).map(getStackInSlot).toArray)
            craftHelper.findRecipeFromInputs(world)
            if (craftHelper.recipe != null)
                craftHelper.loadResultFromRecipe()

//            for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, getStackInSlot(i))
//            matchAndSetRecipe()
        }
        else {
            val plan = getStackInSlot(27)
            if (plan != null && ItemPlan.hasRecipeInside(plan)) {
                val inputs = ItemPlan.loadPlanInputs(plan)
//                for (i <- 0 until 9) invCrafting.setInventorySlotContents(i, inputs(i))
//                matchAndSetRecipe()
                craftHelper.loadInputs(inputs)
                craftHelper.findRecipeFromInputs(world)
                if (craftHelper.recipe != null) {
                    isPlanRecipe = true
                    craftHelper.loadResultFromRecipe()
                }
            }
        }

//        def matchAndSetRecipe() {
//            val recipes = CraftingManager.getInstance().getRecipeList
//            currentRecipe = recipes.find(_.matches(invCrafting, world)).orNull
//            if (currentRecipe != null) {
//                invResult.setInventorySlotContents(0, currentRecipe.getCraftingResult(invCrafting))
//                for (i <- 0 until 9)
//                    currentInputs(i) = {
//                        val s = invCrafting.getStackInSlot(i)
//                        if (s != null) s.copy else null
//                    }
//            }
//        }
    }

    def writePlan()
    {
        updateRecipeIfNeeded()

        if (craftHelper.recipe != null && !isPlanRecipe) {
            val out = craftHelper.recipe.getCraftingResult(craftHelper.invCrafting)
            if (out != null) {
                val stack = getStackInSlot(27)
                if (stack != null)
                    ItemPlan.savePlan(stack, (0 until 9).map(getStackInSlot).toArray, out)
            }
        }
    }

    def clearGrid(id: Int)
    {
        world.getEntityByID(id) match {
            case p: EntityPlayer => p.openContainer match {
                case c: ContainerProjectBench =>
                    c.transferAllFromGrid()
                    updateRecipeIfNeeded()
                case _ =>
            }
            case _ =>
        }
    }

    def transferExcessToStorage()
    {
        var w:InvWrapper = null
        for (i <- 0 until 9) {
            val s = getStackInSlot(i)
            if (s != null && s.stackSize > 1) {
                if (w == null)
                    w = InvWrapper.wrap(this).setInternalMode(true).setSlotsFromRange(9 until 27)

                val toMove = math.max(1, s.stackSize/8)
                val ins = w.injectItem(ItemKey.get(s), toMove)
                if (ins > 0) {
                    s.stackSize -= ins
                    setInventorySlotContents(i, s)
                }
            }
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

class CraftingResultTestHelper
{
    var recipe:IRecipe = null
    val invCrafting = new InventoryCrafting(new NodeContainer, 3, 3)
    val invResult = new InventoryCraftResult
    private var storage:Array[ItemStack] = null

    def clear()
    {
        recipe = null
        for (i <- 0 until 9)
            invCrafting.setInventorySlotContents(i, null)
        invResult.setInventorySlotContents(0, null)
        storage = null
    }

    def setRecipe(r:IRecipe)
    {
        recipe = r
    }

    def findRecipeFromInputs(w:World)
    {
        val recipes = CraftingManager.getInstance.getRecipeList
        recipe = recipes.find(_.matches(invCrafting, w)).orNull
    }

    def loadResultFromRecipe()
    {
        if (recipe != null)
            invResult.setInventorySlotContents(0, recipe.getCraftingResult(invCrafting))
    }

    def loadInputs(inputs:Array[ItemStack])
    {
        for (i <- 0 until 9) {
            val in = inputs(i)
            invCrafting.setInventorySlotContents(i, if (in == null) null else in.copy)
        }
    }

    def loadStorage(storage:Array[ItemStack], copy:Boolean)
    {
        this.storage = new Array[ItemStack](storage.length)
        for (i <- storage.indices) {
            val s = storage(i)
            this.storage(i) = if (s != null && copy) s.copy else s
        }
    }

    def consumeAndCraft(w:World):(ItemStack, Array[ItemStack]) =
    {
        if (!recipe.matches(invCrafting, w)) return (null, null)

        val result = recipe.getCraftingResult(invCrafting)
        if (result == null) return (null, null)

        for (i <- 0 until 9) {
            val prevInput = invCrafting.getStackInSlot(i)
            if (prevInput != null && !eatIngredient(0, { input =>
                invCrafting.setInventorySlotContents(i, input)
                val resultSame = recipe.matches(invCrafting, w) && ItemStack.areItemStacksEqual(recipe.getCraftingResult(invCrafting), result)
                if (resultSame)
                    true
                else {
                    invCrafting.setInventorySlotContents(i, prevInput)
                    false
                }
            })) return (null, null)
        }

        (result, recipe.getRemainingItems(invCrafting))
    }

    private def eatIngredient(startIdx:Int, matchFunc:ItemStack => Boolean):Boolean =
    {
        var i = startIdx
        def increment() = {
            i = (i + 1) % storage.length; i
        }
        do {
            val stack2 = storage(i)
            if (stack2 != null && matchFunc(stack2)) {
                if (stack2.stackSize >= 1) {
                    stack2.stackSize -= 1
                    if (stack2.stackSize <= 0)
                        storage(i) = null
                    return true
                }
            }
        } while (increment() != startIdx)
        false
    }

    def consumeAndCraftToStorage(w:World, slotLimit:Int):Boolean =
    {
        val (result, remaining) = consumeAndCraft(w)

        if (result == null)
            return false

        val wr = InvWrapper.wrap(new ArrayWrapInventory(storage, "", slotLimit)).setInternalMode(true)

        for (stack <- Seq(result) ++ remaining.filter(_ != null)) {
            val i = wr.injectItem(ItemKey.get(stack), stack.stackSize)
            if (i < stack.stackSize)
                return false
        }

        true
    }

    def unloadStorage(inv:IInventory, idxToSlot:Int => Int)
    {
        for (i <- storage.indices)
            inv.setInventorySlotContents(idxToSlot(i), storage(i))
    }
}

class SlotProjectCrafting(player: EntityPlayer, tile: TileProjectBench, idx: Int, x: Int, y: Int)
    extends SlotCrafting(player, tile.craftHelper.invCrafting, tile.craftHelper.invResult, idx, x, y) with TSlot3 {

    override def canTakeStack(player: EntityPlayer): Boolean = {
        if (tile.isPlanRecipe) {
            val storage = (9 until 27).map { i =>
                val s = tile.getStackInSlot(i)
                if (s != null) s.copy else null
            }.toArray

//            craftHelper.setRecipe(tile.currentRecipe)
//            craftHelper.loadInputs(tile.currentInputs)
            tile.craftHelper.loadStorage(storage, true)

            val (res, _) = tile.craftHelper.consumeAndCraft(player.worldObj)

            return res != null
        }

        //copied from super for obfuscation bug
        canRemoveDelegate()
    }

    override def onPickupFromSlot(player:EntityPlayer, stack:ItemStack)
    {
        val order = (9 until 27) ++ (0 until 9)
        val storage = order.map {tile.getStackInSlot}.toArray

//        craftHelper.setRecipe(tile.currentRecipe)
//        craftHelper.loadInputs(tile.currentInputs)
        tile.craftHelper.loadStorage(storage, true)
        val (_, rem) = tile.craftHelper.consumeAndCraft(player.worldObj)

        tile.craftHelper.unloadStorage(tile, order.apply)
//
//        for (i <- 0 until tile.getSizeInventory) { //TODO create this type of cleaning func inside TInventory?
//            val stack = tile.getStackInSlot(i)
//            if (stack != null && stack.stackSize <= 0)
//                tile.setInventorySlotContents(i, null)
//        }

        FMLCommonHandler.instance().firePlayerCraftingEvent(player, stack, tile.craftHelper.invCrafting)
        onCrafting(stack)

        for (i <- 0 until 9) {
            val istack = tile.getStackInSlot(i)
            val rstack = rem(i)
            if (rstack != null) {
                if (!tile.isPlanRecipe && istack == null) {
                    tile.setInventorySlotContents(i, rstack)
                } else if (!addToStorageSlots(rstack) && !player.inventory.addItemStackToInventory(rstack))
                    player.dropItem(rstack, false)
            }
        }

        def addToStorageSlots(stack:ItemStack):Boolean = {
            val w = InvWrapper.wrap(tile).setInternalMode(true).setSlotsFromRange(9 until 27)
            val item = ItemKey.get(stack)
            stack.stackSize -= w.injectItem(item, stack.stackSize)
            stack.stackSize == 0
        }

        tile.updateRecipe()
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

    override def update_Impl()
    {
        tile.updateRecipeIfNeeded()
    }

    override def drawBack_Impl(mouse: Point, rframe: Float) {
        TextureUtils.changeTexture(GuiProjectBench.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        val plan = tile.getStackInSlot(27)
        if (tile.isPlanRecipe && plan != null) {
            val inputs = ItemPlan.loadPlanInputs(plan)
            for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex) {
                val stack = inputs(i)
                if (stack != null) {
                    GuiDraw.drawRect(x, y, 16, 16, EnumColour.GRAY.argb)
                    ItemDisplayNode.renderItem(Point(x, y), Size(16, 16), zPosition, false, stack)
                }
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

object GuiProjectBench extends TGuiFactory {
    val background = new ResourceLocation("projectred", "textures/gui/project_bench.png")

    override def getID = ExpansionProxy.projectbenchGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player: EntityPlayer, data: MCDataInput) = {
        player.worldObj.getTileEntity(data.readPos) match {
            case t: TileProjectBench => new GuiProjectBench(t, t.createContainer(player))
            case _ => null
        }
    }

    def drawPlanOutputOverlay(slots:Iterable[TSlot3]) {
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
