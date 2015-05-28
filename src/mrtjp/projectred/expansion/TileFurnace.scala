package mrtjp.projectred.expansion

import mrtjp.core.gui.Slot3
import mrtjp.core.inventory.InvWrapper
import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack

class TileFurnace extends TileMachineWorking
{
    override def size = 2
    override def stackLimit = 64
    override def name = "furnace"

    def getBlock = ProjectRedExpansion.machine1

    override def openGui(player:EntityPlayer){}

    def createContainer(player:EntityPlayer) = new WorkingMachineContainer(player, this)
    {
        addSlotToContainer(new Slot3(TileFurnace.this, 0, 44, 37))
        val outslot = new Slot3(TileFurnace.this, 1, 104, 37)
        outslot.canPlaceDelegate = {_ => false}
        addSlotToContainer(outslot)

        addPlayerInv(player, 8, 84)
    }

    def getAccessibleSlotsFromSide(s:Int) = s match
    {
        case 1 => Array(0) // input
        case 2|3|4|5 => Array(1) // output
        case _ => Array()
    }
    def canInsertItem(i:Int, itemstack:ItemStack, j:Int) = i == 0
    def canExtractItem(i:Int, itemstack:ItemStack, j:Int) = i == 1

    override def transfer() {}

    override def endWork()
    {
        val r = FurnaceRecipeLib.getRecipeFor(getStackInSlot(0))
        if (r != null)
        {
            val wrap = InvWrapper.wrap(this).setInternalMode(true).setSlotSingle(0)
            wrap.extractItem(r.inputKey, 1)
            wrap.setSlotSingle(1).injectItem(r.output, true)
        }
    }

    override def startWork()
    {
        val r = FurnaceRecipeLib.getRecipeFor(getStackInSlot(0))
        if (r != null)
        {
            workMax = r.ticks
            workRemaining = workMax
        }
    }

    override def canStart:Boolean =
    {
        val inSlot = getStackInSlot(0)
        if (inSlot == null) return false

        val rec = FurnaceRecipeLib.getRecipeFor(inSlot)
        if (rec == null) return false

        val stack = rec.outputKey
        val room = InvWrapper.wrap(this).setSlotSingle(1).setInternalMode(true).getSpaceForItem(stack.key)
        room >= stack.stackSize
    }
}