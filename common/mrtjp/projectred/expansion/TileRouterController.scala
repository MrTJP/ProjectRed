package mrtjp.projectred.expansion

import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.entity.player.EntityPlayer
import mrtjp.projectred.core.{TPowerFlow, PowerConductor}
import mrtjp.projectred.transportation.TControllerLayer
import mrtjp.projectred.core.inventory.{SpecialContainer, SimpleInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import mrtjp.projectred.core.inventory.SpecialContainer.ISlotController

class TileRouterController extends TileMachine with TileGuiMachine with TControllerLayer
{
    val cpuSlot = new SimpleInventory(1, "inv", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemCPU]
    }

    override def getBlockID = ProjectRedExpansion.machine1.blockID

    override def doesRotate = false

    override def guiID = MachineGuiFactory.id_controller

    override def createContainer(player:EntityPlayer) =
    {
        val cont = new SpecialContainer(player.inventory)
        val sc = ISlotController.InventoryRulesController.instance
        cont.addCustomSlot(new SpecialContainer.SlotExtended(cpuSlot, 0, 134, 20).setCheck(sc))
        cont.addPlayerInventory(8, 84)
        cont
    }


    override def update()
    {
        super.update()
        val stack = cpuSlot.getStackInSlot(0)
        if (stack != null)
        {
            assureNBT(stack)
            if (stack.getTagCompound.getDouble("cycles") <= 0)
                cpuSlot.setInventorySlotContents(0, null)
        }
    }

    def assureNBT(stack:ItemStack)
    {
        if (stack != null)
        {
            val tag = if (stack.hasTagCompound) stack.getTagCompound else new NBTTagCompound()
            if (!tag.hasKey("cycles")) tag.setDouble("cycles", 65536)
            stack.setTagCompound(tag)
        }
    }

    override def getPower =
    {
        val stack = cpuSlot.getStackInSlot(0)
        if (stack != null)
        {
            assureNBT(stack)
            stack.getTagCompound.getDouble("cycles")
        }
        else 0
    }

    override def usePower(P:Double) =
    {
        val stack = cpuSlot.getStackInSlot(0)
        if (stack != null)
        {
            assureNBT(stack)
            val newPow = stack.getTagCompound.getDouble("cycles")-P
            stack.getTagCompound.setDouble("cycles", newPow)
            newPow >= 0
        }
        else false
    }
}
