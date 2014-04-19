package mrtjp.projectred.expansion

import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.inventory.SpecialContainer.SlotExtended
import mrtjp.projectred.core.inventory.{InvWrapper, SimpleInventory}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack

class TileFurnace extends TileMachineWorking
{
    def getBlock = ProjectRedExpansion.machine1

    def createContainer(player:EntityPlayer) = new WorkingMachineContainer(player, this)
    {
        addPlayerInventory(8, 84)
        addCustomSlot(new SlotExtended(TileFurnace.this, 0, 44, 37)) //input
        addCustomSlot(new SlotExtended(TileFurnace.this, 1, 104, 37).setPlacement(false)) //output
    }

    def guiID = MachineGuiFactory.id_furnace

    def getAccessibleSlotsFromSide(var1:Int) = var1 match
    {
        case _ if inputSeq contains var1 => Array(0) // input
        case _ if outputSeq contains var1 => Array(1) // output
        case _ => Array()
    }
    def canInsertItem(i:Int, itemstack:ItemStack, j:Int) = i == 0
    def canExtractItem(i:Int, itemstack:ItemStack, j:Int) = i == 1

    def inputSeq =
    {
        var seq = Seq[Int]()
        for (i <- 0 until 6)
        {
            val mode = sideConfig(i)
            if (Seq(1, 3) contains mode) seq :+= i
        }
        seq
    }

    def outputSeq =
    {
        var seq = Seq[Int]()
        for (i <- 0 until 6)
        {
            val mode = sideConfig(i)
            if (Seq(2, 3) contains mode) seq :+= i
        }
        seq
    }

    override def createSideConfig = Array(3, 3, 3, 3, 3, 3)
    override def sideInfo = Array("closed", "input", "output", "input + output")

    //0 input
    //1 output
    def createInv = new SimpleInventory(2, "smelt", 64)

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