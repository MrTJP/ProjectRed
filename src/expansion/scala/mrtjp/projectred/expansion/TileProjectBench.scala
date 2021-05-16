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
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.gui._
import mrtjp.core.inventory.{ArrayWrapInventory, InvWrapper, TInventory, TInventoryCapablilityTile}
import mrtjp.core.item.ItemKey
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.item.PlanItem
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory}
import net.minecraft.inventory._
import net.minecraft.inventory.container.{ClickType, Container, CraftingResultSlot, Slot}
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.{ICraftingRecipe, IRecipeType}
import net.minecraft.nbt.CompoundNBT
import net.minecraft.util.text.ITextComponent
import net.minecraft.util.{Direction, NonNullList, ResourceLocation}
import net.minecraft.world.World
import net.minecraftforge.common.ForgeHooks

import scala.jdk.CollectionConverters._

class TileProjectBench extends TileMachine(ExpansionContent.projectBenchTile.get) with TInventory with ISidedInventory with TInventoryCapablilityTile with TGuiMachine
{
    val craftHelper = new CraftingResultTestHelper
    var isPlanRecipe = false

    private var recipeNeedsUpdate = true

    //0-8 crafting, 9-26 ingredients, 27 plan, 28 result
    override protected val storage:Array[ItemStack] = Array.fill(28)(ItemStack.EMPTY)//new Array[ItemStack](28)

    override def saveToNBT(tag:CompoundNBT) = {
        super.saveToNBT(tag)
        saveInv(tag)
    }

    override def loadFromNBT(tag:CompoundNBT) = {
        super.loadFromNBT(tag)
        loadInv(tag)
    }

    override def readUpdate(key:Int, in: MCDataInput):Unit = key match {
        case 1 => in.readInt(); writePlan()
        case 2 => clearGrid(in.readInt())
        case _ => super.readUpdate(key, in)
    }

    def sendWriteButtonAction():Unit = {
        sendUpdate(1, _.writeInt(0))
    }

    def sendClearGridAction(id: Int) {
        sendUpdate(2, _.writeInt(id))
    }

    override def doesRotate = false

    override def doesOrient = false

    override def getMaxStackSize = 64

    override def nbtSaveName = "project_bench"

    override def canTakeItemThroughFace(slot: Int, item: ItemStack, side:Direction):Boolean = 9 until 27 contains slot
    override def canPlaceItemThroughFace(slot: Int, item: ItemStack, side:Direction):Boolean = 9 until 27 contains slot
    override def getSlotsForFace(side:Direction):Array[Int] = (9 until 27).toArray

    override def updateServer():Unit = {
        updateRecipeIfNeeded()
        transferExcessToStorage()
    }

    override def updateClient():Unit = {
//        updateRecipeIfNeeded()
    }

    def updateRecipeIfNeeded():Unit = {
        if (!recipeNeedsUpdate) return
        recipeNeedsUpdate = false
        updateRecipe()
    }

    def updateRecipe():Unit = {
        isPlanRecipe = false
        craftHelper.clear()

        if ((0 until 9).exists(!getItem(_).isEmpty)) {
            craftHelper.loadInputs((0 until 9).map(getItem).toArray)
            craftHelper.findRecipeFromInputs(level)
            if (craftHelper.recipe.isDefined)
                craftHelper.loadResultFromRecipe()
        } else {
            val plan = getItem(27)
            if (!plan.isEmpty && PlanItem.hasRecipeInside(plan)) {
                val inputs = PlanItem.loadPlanInputs(plan)

                craftHelper.loadInputs(inputs)
                craftHelper.findRecipeFromInputs(level)
                if (craftHelper.recipe.isDefined) {
                    isPlanRecipe = true
                    craftHelper.loadResultFromRecipe()
                }
            }
        }
    }

    def writePlan():Unit = {
        updateRecipeIfNeeded()

        if (craftHelper.recipe.isDefined && !isPlanRecipe) {
            val out = craftHelper.recipe.get.assemble(craftHelper.invCrafting)
            if (!out.isEmpty) {
                val stack = getItem(27)
                if (!stack.isEmpty)
                    PlanItem.savePlan(stack, (0 until 9).map(getItem).toArray, out)
            }
        }
    }

    def clearGrid(id: Int):Unit = {
        level.getEntity(id) match {
            case p:PlayerEntity => p.containerMenu match {
                case c: ContainerProjectBench =>
                    c.transferAllFromGrid()
                    updateRecipeIfNeeded()
                case _ =>
            }
            case _ =>
        }
    }

    def transferExcessToStorage():Unit = {
        var w:InvWrapper = null
        for (i <- 0 until 9) {
            val s = getItem(i)
            if (!s.isEmpty && s.getCount > 1) {
                if (w == null)
                    w = InvWrapper.wrapInternal(this, 9 until 27)

                val toMove = math.max(1, s.getCount/8)
                val ins = w.injectItem(ItemKey.get(s), toMove)
                if (ins > 0) {
                    s.shrink(ins)
                    setItem(i, s)
                }
            }
        }
    }

    override def setChanged():Unit = {
        super.setChanged()
        recipeNeedsUpdate = true
    }

    override def onBlockRemoved():Unit = {
        super.onBlockRemoved()
        dropInvContents(level, getBlockPos)
    }

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerProjectBench =
        new ContainerProjectBench(playerInv, this, windowId)
}

class CraftingResultTestHelper
{
    var recipe:Option[ICraftingRecipe] = None

    val invCrafting = new CraftingInventory(new Container(null, -1) {
        override def stillValid(playerIn:PlayerEntity):Boolean = false
    }, 3, 3)
    val invResult = new CraftResultInventory

    private var storage:Array[ItemStack] = Array.empty

    def clear():Unit = {
        recipe = None
        for (i <- 0 until 9)
            invCrafting.setItem(i, ItemStack.EMPTY)
        invResult.setItem(0, ItemStack.EMPTY)
        storage = Array.empty
    }

    def setRecipe(r:ICraftingRecipe):Unit = {
        recipe = Some(r)
    }

    def findRecipeFromInputs(w:World):Unit = {
        val r = w.getRecipeManager.getRecipeFor(IRecipeType.CRAFTING, invCrafting, w)
        recipe = if (r.isPresent) Some(r.get) else None
    }

    def loadResultFromRecipe():Unit = {
        recipe match {
            case Some(r) =>
                val result = r.assemble(invCrafting)
                invResult.setItem(0, result)
            case _ =>
        }
    }

    def loadInputs(inputs:Array[ItemStack]):Unit = {
        for (i <- 0 until 9)
            invCrafting.setItem(i, inputs(i).copy)
    }

    def loadStorage(storage:Array[ItemStack], copy:Boolean):Unit = {
        this.storage = new Array[ItemStack](storage.length)
        for (i <- storage.indices) {
            val s = storage(i)
            this.storage(i) = if (copy) s.copy else s
        }
    }

    def consumeAndCraft(w:World):(ItemStack, NonNullList[ItemStack]) = {
        recipe match {
            case Some(r) if r.matches(invCrafting, w) =>
                val result = r.assemble(invCrafting)
                if (!result.isEmpty) {
                    val ingredientsAvailable = (0 until 9).forall { i =>
                        val prevInput = invCrafting.getItem(i)
                        prevInput.isEmpty || eatIngredient(0, { input =>
                            invCrafting.setItem(i, input.copy)
                            //TODO also compare the two inputs. Potential here for invalid matches:
                            //If two recipes exist such that A + B = D and C + B = D,
                            //an input matrix of A + B could consume ingredient C
                            val resultSame = r.matches(invCrafting, w) && ItemStack.matches(r.assemble(invCrafting), result)
                            if (!prevInput.hasContainerItem || !input.hasContainerItem) //For container items, leave a copy behind so it can be damaged
                                invCrafting.setItem(i, prevInput)
                            resultSame
                        })
                    }

                    if (ingredientsAvailable)
                        (result, r.getRemainingItems(invCrafting))
                    else
                        (ItemStack.EMPTY, null)
                } else
                    (ItemStack.EMPTY, null)
            case _ => (ItemStack.EMPTY, null)
        }
    }

    private def eatIngredient(startIdx:Int, matchFunc:ItemStack => Boolean):Boolean = {
        var i = startIdx
        var found = false
        def increment():Int = {
            i = (i + 1) % storage.length; i
        }
        do {
            val stack2 = storage(i)
            if (!stack2.isEmpty && matchFunc(stack2)) {
                if (stack2.getCount >= 1) {
                    stack2.shrink(1)
                    found = true
                }
            }
        } while (increment() != startIdx && !found)
        found
    }

    def consumeAndCraftToStorage(w:World, slotLimit:Int):Boolean = {
        consumeAndCraft(w) match {
            case (result, remaining) if !result.isEmpty =>
                val wr = InvWrapper.wrapInternal(new ArrayWrapInventory(storage, "", slotLimit))
                (Seq(result) ++ remaining.asScala.filter(!_.isEmpty)).forall { stack =>
                    wr.injectItem(ItemKey.get(stack), stack.getCount) == stack.getCount
                }
            case _ => false
        }
    }

    def unloadStorage(inv:IInventory, idxToSlot:Int => Int):Unit = {
        for (i <- storage.indices)
            inv.setItem(idxToSlot(i), storage(i))
    }
}

class SlotProjectCrafting(player:PlayerEntity, tile: TileProjectBench, idx: Int, x: Int, y: Int)
    extends CraftingResultSlot(player, tile.craftHelper.invCrafting, tile.craftHelper.invResult, idx, x, y) {

    override def mayPickup(player:PlayerEntity): Boolean = {
        if (tile.isPlanRecipe) {
            val storage = (9 until 27).map {tile.getItem}.toArray

            tile.craftHelper.loadStorage(storage, true)

            val (res, _) = tile.craftHelper.consumeAndCraft(player.level)
            !res.isEmpty
        } else
            true //If not plan, ingredients are for sure available in the 3x3 (or else this slot would be empty)
    }

    override def onTake(player:PlayerEntity, stack:ItemStack):ItemStack = {
        val order = (9 until 27) ++ (0 until 9)
        val storage = order.map {tile.getItem}.toArray

        tile.craftHelper.loadStorage(storage, true)
        val (_, rem) = tile.craftHelper.consumeAndCraft(player.level)
        tile.craftHelper.unloadStorage(tile, order.apply)



//        FMLCommonHandler.instance().firePlayerCraftingEvent(player, stack, tile.craftHelper.invCrafting)
        checkTakeAchievements(stack)
        ForgeHooks.setCraftingPlayer(player)
        val nonnulllist = player.level.getRecipeManager.getRecipesFor(IRecipeType.CRAFTING, tile.craftHelper.invCrafting, player.level)
        ForgeHooks.setCraftingPlayer(null)


        for (i <- 0 until 9) {
            val istack = tile.getItem(i)
            val rstack = rem.get(i)
            if (!rstack.isEmpty) {
                if (!tile.isPlanRecipe && istack.isEmpty) {
                    tile.setItem(i, rstack)
                } else if (!addToStorageSlots(rstack) && !player.inventory.add(rstack))
                    player.drop(rstack, false)
            }
        }

        def addToStorageSlots(stack:ItemStack):Boolean = {
            val w = InvWrapper.wrapInternal(tile, 9 until 27)
            val item = ItemKey.get(stack)
            stack.shrink(w.injectItem(item, stack.getCount))
            stack.isEmpty
        }

        tile.updateRecipe()
        stack
    }
//
//    //Following 3 methods copy-pasted from TSlot3 for obfuscation issues
//    override def getSlotStackLimit: Int = slotLimitCalculator()
//
//    override def isItemValid(stack: ItemStack): Boolean = canPlaceDelegate(stack)
//
//    override def onSlotChanged() {
//        super.onSlotChanged()
//        slotChangeDelegate()
//        slotChangeDelegate2()
//    }
}

class ContainerProjectBench(playerInv:PlayerInventory, val tile:TileProjectBench, windowId:Int) extends NodeContainer(ExpansionContent.projectBenchContainer.get, windowId)
{
    {
        for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex)
            addSlot(new Slot3(tile, i, x, y))

        for (((x, y), i) <- GuiLib.createSlotGrid(8, 76, 9, 2, 0, 0).zipWithIndex)
            addSlot(new Slot3(tile, i + 9, x, y))

        val plan = new Slot3(tile, 27, 17, 36)
        plan.canPlaceDelegate = {
            _.getItem.isInstanceOf[PlanItem]
        }
        plan.slotLimitCalculator = { () => 1 }
        addSlot(plan)

        val output = new SlotProjectCrafting(playerInv.player, tile, 28, 143, 36)
        addSlot(output)

        addPlayerInv(playerInv, 8, 126)
    }



    def transferAllFromGrid() {
        for (i <- 0 until 9) if (getSlot(i).hasItem)
            quickMoveStack(playerInv.player, i)
        broadcastChanges()
    }

    override def clicked(id:Int, dragType:Int, clickType:ClickType, player:PlayerEntity):ItemStack = {
        var mode = clickType
        if (id == 28 && mode == ClickType.PICKUP_ALL) mode = ClickType.PICKUP
        super.clicked(id, dragType, mode, player)
    }



    override def quickMoveStack(player:PlayerEntity, i:Int):ItemStack = {
        if (i == 28 && !getSlot(28).mayPickup(player))
            ItemStack.EMPTY
        else
            super.quickMoveStack(player, i)
    }

    override def doMerge(stack: ItemStack, from: Int): Boolean = {
        if (0 until 9 contains from) //crafting grid
        {
            if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
            if (tryMergeItemStack(stack, 29, 65, false)) return true //merge to inventory)
        }
        else if (9 until 27 contains from) //storage
        {
            if (stack.getItem.isInstanceOf[PlanItem]) {
                if (!getSlot(27).getItem.isEmpty && ItemKey.get(getSlot(27).getItem) != ItemKey.get(stack))
                    quickMoveStack(playerInv.player, 27) //transfer existing stack

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
            if (stack.getItem.isInstanceOf[PlanItem]) {
                if (!getSlot(27).getItem.isEmpty && ItemKey.get(getSlot(27).getItem) != ItemKey.get(stack))
                    quickMoveStack(playerInv.player, 27) //transfer existing stack

                if (tryMergeItemStack(stack, 27, 28, false)) return true //merge to plan
            }
            if (tryMergeItemStack(stack, 9, 27, false)) return true //merge to storage
        }

        false
    }
}

object ContainerProjectBench extends ICCLContainerFactory[ContainerProjectBench]
{
    override def create(windowId:Int, inv:PlayerInventory, packet:MCDataInput):ContainerProjectBench = {
        inv.player.level.getBlockEntity(packet.readPos()) match {
            case t:TileProjectBench => t.createMenu(windowId, inv, inv.player)
            case _ => null
        }
    }
}

class GuiProjectBench(c:ContainerProjectBench, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 208, playerInv, title) {
    {
        val write = new IconButtonNode {
            override def drawButton(stack:MatrixStack, mouseover: Boolean) {
                TextureUtils.changeTexture(GuiProjectBench.background)
                blit(stack, position.x, position.y, 176, 0, 14, 14)
            }
        }
        write.position = Point(18, 56)
        write.size = Size(14, 14)
        write.clickDelegate = { () => c.tile.sendWriteButtonAction() }
        addChild(write)

        val clear = new IconButtonNode {
            override def drawButton(stack:MatrixStack, mouseover: Boolean) {
                TextureUtils.changeTexture(GuiProjectBench.background)
                blit(stack, position.x, position.y, 176, 15, 8, 8)
            }
        }
        clear.position = Point(37, 17)
        clear.size = Size(8, 8)
        clear.clickDelegate = { () => c.tile.sendClearGridAction(Minecraft.getInstance().player.getId) }
        addChild(clear)
    }

    override def update_Impl()
    {
        c.tile.updateRecipeIfNeeded()
    }

    override def drawBack_Impl(mStack:MatrixStack, mouse: Point, rframe: Float) {
        TextureUtils.changeTexture(GuiProjectBench.background)
        blit(mStack, 0, 0, 0, 0, size.width, size.height)

        val plan = c.tile.getItem(27)
        if (c.tile.isPlanRecipe && !plan.isEmpty) {
            val inputs = PlanItem.loadPlanInputs(plan)
            for (((x, y), i) <- GuiLib.createSlotGrid(48, 18, 3, 3, 0, 0).zipWithIndex) {
                val stack = inputs(i)
                if (!stack.isEmpty) {
                    fillGradient(mStack, x, y, x+16, y+16, EnumColour.GRAY.argb, EnumColour.GRAY.argb)
                    ItemDisplayNode.renderItem(mStack, this, Point(x, y), Size(16, 16), zPosition, false, stack)
                }
            }
        }

        getFontRenderer.draw(mStack, title, 8, 6, EnumColour.GRAY.argb)
        getFontRenderer.draw(mStack, playerInv.getDisplayName, 8, 116, EnumColour.GRAY.argb)
    }

    override def drawFront_Impl(mStack:MatrixStack, mouse: Point, rframe: Float) {
        if (Minecraft.getInstance().options.keyShift.isDown || true)
            drawPlanOutputOverlay(mStack, c.slots.asScala)
    }

    private def drawPlanOutputOverlay(mStack:MatrixStack, slots:Iterable[Slot]) {
        for (slot <- slots) if (slot.hasItem) {
            val stack = slot.getItem
            if (PlanItem.hasRecipeInside(stack)) {
                val output = PlanItem.loadPlanOutput(stack)
                val colour = EnumColour.LIGHT_BLUE.argb(0xCC)
                fillGradient(mStack, slot.x, slot.y, slot.x+16, slot.y+16, colour, colour)
                ItemDisplayNode.renderItem(mStack, this, Point(slot.x+1, slot.y+1), Size(14, 14), 100, true, output)
            }
        }
    }
}

object GuiProjectBench {//extends TGuiFactory {
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/project_bench.png")

    def register():Unit = {
        ScreenManager.register(
            ExpansionContent.projectBenchContainer.get(),
            (cont:ContainerProjectBench, inv, text) => new GuiProjectBench(cont, inv, text))
    }

    //    override def getID = ExpansionProxy.projectbenchGui

//    @SideOnly(Side.CLIENT)
//    override def buildGui(player: EntityPlayer, data: MCDataInput) = {
//        player.world.getTileEntity(data.readPos) match {
//            case t: TileProjectBench => new GuiProjectBench(t, t.createContainer(player))
//            case _ => null
//        }
//    }
//
}

//object RenderProjectBench extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    var bottom: TextureAtlasSprite = _
//    var top: TextureAtlasSprite = _
//    var side1: TextureAtlasSprite = _
//    var side2: TextureAtlasSprite = _
//
//    var iconT: UVTransformation = _
//
//    override def getWorldTransforms(state: IExtendedBlockState) = Triple.of(0, 0, iconT)
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT)
//    override def shouldCull() = true
//
//    override def registerIcons(map: TextureMap) {
//        bottom = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/bottom"))
//        top = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/top"))
//        side1 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/side1"))
//        side2 = map.registerSprite(new ResourceLocation("projectred:blocks/mechanical/projectbench/side2"))
//
//        iconT = new MultiIconTransformation(bottom, top, side1, side1, side2, side2)
//    }
//}
