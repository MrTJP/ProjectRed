package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.inventory.container.ICCLContainerFactory
import codechicken.lib.texture.TextureUtils
import codechicken.lib.util.ServerUtils
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.gui._
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.gui.ScreenManager
import net.minecraft.entity.player.{PlayerEntity, PlayerInventory, ServerPlayerEntity}
import net.minecraft.item.ItemStack
import net.minecraft.util.text.ITextComponent
import net.minecraft.util.{Direction, ResourceLocation}

class TileInductiveFurnace extends TileProcessingMachine(ExpansionContent.inductionFurnaceTile.get)
{
    override protected val storage:Array[ItemStack] = Array.fill(2)(ItemStack.EMPTY)
    override def getMaxStackSize = 64
    override def nbtSaveName:String = "induction_furnace"

    override def createMenu(windowId:Int, playerInv:PlayerInventory, player:PlayerEntity):ContainerInductiveFurnace =
        new ContainerInductiveFurnace(playerInv, this, windowId)

    def canTakeItemThroughFace(slot:Int, itemstack:ItemStack, side:Direction):Boolean = true
    def canPlaceItemThroughFace(slot:Int, itemstack:ItemStack, side:Direction):Boolean = side == Direction.UP
    def getSlotsForFace(s:Direction):Array[Int] = s match {
        case Direction.UP => Array(0) // input
        case Direction.NORTH|Direction.SOUTH|Direction.WEST|Direction.EAST => Array(1) // output
        case _ => Array.emptyIntArray
    }

    override def canStart:Boolean = {
        // TODO furnace recipies
//        val inSlot = getStackInSlot(0)
//        if (!inSlot.isEmpty) {
//            val r = InductiveFurnaceRecipeLib.getRecipeFor(inSlot)
//            if (r != null) {
//                val stack = r.createOutput
//                val room = InvWrapper.wrapInternal(this, 1 to 1).getSpaceForItem(ItemKey.get(stack))
//                room >= stack.getCount
//            } else
//                false
//        } else
//            false
        false
    }

    override def startWork():Unit = {
//        val r = InductiveFurnaceRecipeLib.getRecipeFor(getStackInSlot(0))
//        if (r != null) {
//            isWorking = true
//            workMax = r.burnTime
//            workRemaining = workMax
//        }
    }

    override def produceResults():Unit = {
//        val in = getStackInSlot(0)
//        val r = InductiveFurnaceRecipeLib.getRecipeFor(in)
//        if (r != null) {
//            val wrap = InvWrapper.wrapInternal(this, 0 to 0)
//            wrap.extractItem(ItemKey.get(in), 1)
//            val out = r.createOutput
//            wrap.setSlotsFromRange(1 to 1).injectItem(ItemKey.get(out), out.getCount)
//        }
    }
}

class ContainerInductiveFurnace(playerInv:PlayerInventory, val tile:TileInductiveFurnace, windowId:Int) extends ContainerProcessingMachine(tile, ExpansionContent.inductionFurnaceContainer.get, windowId)
{
    {
        addSlot(new Slot3(tile, 0, 56, 40))

        val outslot = new Slot3(tile, 1, 115, 40)
        outslot.canPlaceDelegate = {_ => false}
        addSlot(outslot)

        addPlayerInv(playerInv, 8, 89)
    }

    override def doMerge(stack:ItemStack, from:Int):Boolean =
    {
        if (from == 0) { //input slot
            if (tryMergeItemStack(stack, 11, 38, false)) return true //to player inv
            tryMergeItemStack(stack, 2, 11, false) //to hotbar
        } else if (from == 1) { //output slot
            if (tryMergeItemStack(stack, 2, 11, true)) return true //to hotbar reverse
            tryMergeItemStack(stack, 11, 38, true) //to player inv reverse
        } else //from player inventory
            tryMergeItemStack(stack, 0, 1, false) //to furnace input
    }
}

object ContainerInductiveFurnace extends ICCLContainerFactory[ContainerInductiveFurnace] {
    override def create(windowId:Int, inventory:PlayerInventory, packet:MCDataInput):ContainerInductiveFurnace = {
        inventory.player.level.getBlockEntity(packet.readPos()) match {
            case t:TileInductiveFurnace => t.createMenu(windowId, inventory, inventory.player)
        }
    }
}

class GuiInductiveFurnace(c:ContainerInductiveFurnace, playerInv:PlayerInventory, title:ITextComponent) extends NodeGui(c, 176, 171, playerInv, title)
{
    override def drawBack_Impl(stack:MatrixStack, mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiInductiveFurnace.background)
        blit(stack, 0, 0, 0, 0, size.width, size.height)

        val s = c.tile.progressScaled(24)
        blit(stack, 80, 40, 176, 0, s+1, 16)

        if (c.tile.cond.canWork)
            blit(stack, 16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(stack, this, 16, 26, 177, 27, 7, 48, c.tile.cond.getChargeScaled(48))

        if (c.tile.cond.flow == -1)
            blit(stack, 27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(stack, this, 27, 26, 185, 27, 7, 48, c.tile.cond.getFlowScaled(48))

        font.draw(stack, title, 8, 6, EnumColour.GRAY.argb)
        font.draw(stack, playerInv.getDisplayName, 8, 79, EnumColour.GRAY.argb)
    }
}

object GuiInductiveFurnace
{
    val background = new ResourceLocation(ProjectRedExpansion.MOD_ID, "textures/gui/induction_furnace.png")

    def register():Unit = {
        ScreenManager.register(
            ExpansionContent.inductionFurnaceContainer.get(),
            (cont:ContainerInductiveFurnace, inv, text) => new GuiInductiveFurnace(cont, inv, text))
    }
}

//object RenderInductiveFurnace extends SimpleBlockRenderer
//{
//    import org.apache.commons.lang3.tuple.Triple
//
//    import java.lang.{Boolean => JBool, Integer => JInt}
//
//    var bottom:TextureAtlasSprite = _
//    var top:TextureAtlasSprite = _
//    var side1:TextureAtlasSprite = _
//    var side2a:TextureAtlasSprite = _
//    var side2b:TextureAtlasSprite = _
//    var side2c:TextureAtlasSprite = _
//
//    var iconT1:UVTransformation = _
//    var iconT2:UVTransformation = _
//    var iconT3:UVTransformation = _
//
//    override def handleState(state: IExtendedBlockState, world: IBlockAccess, pos: BlockPos): IExtendedBlockState = {
//
//       world.getTileEntity(pos) match {
//            case t:TileInductiveFurnace =>
//                var s = state
//                s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[Integer])
//                s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
//                s = s.withProperty(UNLISTED_WORKING_PROPERTY, t.isWorking.asInstanceOf[JBool])
//                s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
//                s
//            case _ => state
//        }
//    }
//
//    override def getWorldTransforms(state: IExtendedBlockState) = {
//        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
//        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
//        val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
//        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
//        Triple.of(side, rotation,
//            if (isWorking && isCharged) iconT3
//            else if (isCharged) iconT2
//            else iconT1)
//    }
//
//    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)
//    override def shouldCull() = true
//
//    override def registerIcons(reg:TextureMap)
//    {
//        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/bottom"))
//        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/top"))
//        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side1"))
//        side2a = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2a"))
//        side2b = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2b"))
//        side2c = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2c"))
//
//        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
//        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
//        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
//    }
//}
