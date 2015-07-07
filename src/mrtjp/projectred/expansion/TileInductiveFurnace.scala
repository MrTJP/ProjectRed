package mrtjp.projectred.expansion

import codechicken.lib.data.MCDataInput
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.vec.Point
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

class TileInductiveFurnace extends TileProcessingMachine
{
    override def size = 2
    override def name = "furnace"

    def getBlock = ProjectRedExpansion.machine1

    override def openGui(player:EntityPlayer)
    {
        GuiInductiveFurnace.open(player, createContainer(player), _.writeCoord(x, y, z))
    }

    def createContainer(player:EntityPlayer) =
        new ContainerFurnace(player, this)

    def canExtractItem(slot:Int, itemstack:ItemStack, side:Int) = true
    def canInsertItem(slot:Int, itemstack:ItemStack, side:Int) = side == 1
    def getAccessibleSlotsFromSide(s:Int) = s match
    {
        case 1 => Array(0) // input
        case 2|3|4|5 => Array(1) // output
        case _ => Array.emptyIntArray
    }

    override def canStart:Boolean =
    {
        val inSlot = getStackInSlot(0)
        if (inSlot == null) return false

        val r = InductiveFurnaceRecipeLib.getRecipeFor(inSlot)
        if (r == null) return false

        val stack = r.createOutput
        val room = InvWrapper.wrap(this).setSlotSingle(1).setInternalMode(true).getSpaceForItem(ItemKey.get(stack))
        room >= stack.stackSize
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
            val wrap = InvWrapper.wrap(this).setInternalMode(true).setSlotSingle(0)
            wrap.extractItem(ItemKey.get(in), 1)
            wrap.setSlotSingle(1).injectItem(r.createOutput, true)
        }
    }
}

class ContainerFurnace(p:EntityPlayer, tile:TileInductiveFurnace) extends ContainerProcessingMachine(p, tile)
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
        if (from == 0) tryMergeItemStack(stack, 2, 38, false)
        else if (from == 1) tryMergeItemStack(stack, 2, 38, true)
        else tryMergeItemStack(stack, 0, 1, false)
    }
}

class GuiInductiveFurnace(tile:TileInductiveFurnace, c:Container) extends NodeGui(c, 176, 171)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiFurnace.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, size.width, size.height)

        val s = tile.progressScaled(24)
        drawTexturedModalRect(80, 40, 176, 0, s+1, 16)

        if (tile.cond.canWork)
            GuiDraw.drawTexturedModalRect(16, 16, 177, 18, 7, 9)
        GuiLib.drawVerticalTank(16, 26, 177, 27, 7, 48, tile.cond.getChargeScaled(48))

        if (tile.cond.flow == -1)
            GuiDraw.drawTexturedModalRect(27, 16, 185, 18, 7, 9)
        GuiLib.drawVerticalTank(27, 26, 185, 27, 7, 48, tile.cond.getFlowScaled(48))

        GuiDraw.drawString("Inductive Furnace", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 79, Colors.GREY.argb, false)
    }
}

object GuiInductiveFurnace extends TGuiBuilder
{
    override def getID = ExpansionProxy.furnaceGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        WorldLib.getTileEntity(player.worldObj, data.readCoord()) match
        {
            case t:TileInductiveFurnace => new GuiInductiveFurnace(t, t.createContainer(player))
            case _ => null
        }
    }
}

object RenderInductiveFurnace extends TCubeMapRender
{
    var bottom:IIcon = _
    var top:IIcon = _
    var side1:IIcon = _
    var side2a:IIcon = _
    var side2b:IIcon = _
    var side2c:IIcon = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _
    var iconT3:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TileInductiveFurnace])
        if (te != null) (te.side, te.rotation,
                if (te.isWorking && te.isCharged) iconT3
                else if (te.isCharged) iconT2
                else iconT1)
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(side:Int, meta:Int) = side match
    {
        case 0 => bottom
        case 1 => top
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:mechanical/indfurnace/bottom")
        top = reg.registerIcon("projectred:mechanical/indfurnace/top")
        side1 = reg.registerIcon("projectred:mechanical/indfurnace/side1")
        side2a = reg.registerIcon("projectred:mechanical/indfurnace/side2a")
        side2b = reg.registerIcon("projectred:mechanical/indfurnace/side2b")
        side2c = reg.registerIcon("projectred:mechanical/indfurnace/side2c")

        iconT1 = new MultiIconTransformation(bottom, top, side1, side2a, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top, side1, side2b, side1, side1)
        iconT3 = new MultiIconTransformation(bottom, top, side1, side2c, side1, side1)
    }
}