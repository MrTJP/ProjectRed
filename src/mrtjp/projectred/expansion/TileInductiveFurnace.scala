package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.bakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui._
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TileInductiveFurnace extends TileProcessingMachine
{
    override protected val storage = Array.fill(2)(ItemStack.EMPTY)//new Array[ItemStack](2)
    override def getInventoryStackLimit = 64
    override def getName = "furnace"

    def getBlock = ProjectRedExpansion.machine1

    override def openGui(player:EntityPlayer)
    {
        GuiInductiveFurnace.open(player, createContainer(player), _.writePos(getPos))
    }

    def createContainer(player:EntityPlayer) =
        new ContainerFurnace(player, this)

    import net.minecraft.util.EnumFacing._
    def canExtractItem(slot:Int, itemstack:ItemStack, side:EnumFacing) = true
    def canInsertItem(slot:Int, itemstack:ItemStack, side:EnumFacing) = side == EnumFacing.UP
    def getSlotsForFace(s:EnumFacing) = s match
    {
        case UP => Array(0) // input
        case NORTH|SOUTH|WEST|EAST => Array(1) // output
        case _ => Array.emptyIntArray
    }

    override def canStart:Boolean =
    {
        val inSlot = getStackInSlot(0)
        if (inSlot.isEmpty) return false

        val r = InductiveFurnaceRecipeLib.getRecipeFor(inSlot)
        if (r == null) return false

        val stack = r.createOutput
        val room = InvWrapper.wrapInternal(this, 1 to 1).getSpaceForItem(ItemKey.get(stack))
        room >= stack.getCount
    }

    override def startWork()
    {
        val r = InductiveFurnaceRecipeLib.getRecipeFor(getStackInSlot(0))
        if (r != null)
        {
            isWorking = true
            workMax = r.burnTime
            workRemaining = workMax
        }
    }

    override def produceResults()
    {
        val in = getStackInSlot(0)
        val r = InductiveFurnaceRecipeLib.getRecipeFor(in)
        if (r != null)
        {
            val wrap = InvWrapper.wrapInternal(this, 0 to 0)
            wrap.extractItem(ItemKey.get(in), 1)
            val out = r.createOutput
            wrap.setSlotsFromRange(1 to 1).injectItem(ItemKey.get(out), out.getCount)
        }
    }
}

class ContainerFurnace(p:EntityPlayer, tile:TileInductiveFurnace) extends ContainerProcessingMachine(tile)
{
    {
        addSlotToContainer(new Slot3(tile, 0, 56, 40))

        val outslot = new Slot3(tile, 1, 115, 40)
        outslot.canPlaceDelegate = {_ => false}
        addSlotToContainer(outslot)

        addPlayerInv(p, 8, 89)
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

class GuiInductiveFurnace(tile:TileInductiveFurnace, c:Container) extends NodeGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiInductiveFurnace.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        val s = tile.progressScaled(24)
        drawTexturedModalRect(80, 40, 176, 0, s+1, 16)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(16, 26, 177, 27, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.cond.flow == -1)
            GuiDraw.drawTexturedModalRect(27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(27, 26, 185, 27, 7, 48, tile.cond.getFlowScaled(48))

        GuiDraw.drawString("Inductive Furnace", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, EnumColour.GRAY.argb, false)
    }
}

object GuiInductiveFurnace extends TGuiFactory
{
    val background = new ResourceLocation("projectred", "textures/gui/furnace.png")
    override def getID = ExpansionProxy.furnaceGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        player.world.getTileEntity(data.readPos()) match
        {
            case t:TileInductiveFurnace => new GuiInductiveFurnace(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderInductiveFurnace extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import org.apache.commons.lang3.tuple.Triple
    import mrtjp.projectred.expansion.BlockProperties._

    var bottom:TextureAtlasSprite = _
    var top:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var side2a:TextureAtlasSprite = _
    var side2b:TextureAtlasSprite = _
    var side2c:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _

    override def handleState(state: IExtendedBlockState, world: IBlockAccess, pos: BlockPos): IExtendedBlockState = {

       world.getTileEntity(pos) match {
            case t:TileInductiveFurnace =>
                var s = state
                s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[Integer])
                s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
                s = s.withProperty(UNLISTED_WORKING_PROPERTY, t.isWorking.asInstanceOf[JBool])
                s = s.withProperty(UNLISTED_CHARGED_PROPERTY, t.isCharged.asInstanceOf[JBool])
                s
            case _ => state
        }
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val isWorking = state.getValue(UNLISTED_WORKING_PROPERTY)
        val isCharged = state.getValue(UNLISTED_CHARGED_PROPERTY)
        Triple.of(side, rotation,
            if (isWorking && isCharged) iconT3
            else if (isCharged) iconT2
            else iconT1)
    }

    override def getItemTransforms(stack: ItemStack) = Triple.of(0, 0, iconT1)
    override def shouldCull() = true


    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/bottom"))
        top = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/top"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side1"))
        side2a = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2a"))
        side2b = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2b"))
        side2c = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/indfurnace/side2c"))

        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
    }
}
