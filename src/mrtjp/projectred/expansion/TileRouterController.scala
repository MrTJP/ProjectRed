package mrtjp.projectred.expansion

import mrtjp.projectred.ProjectRedExpansion
import net.minecraft.entity.player.EntityPlayer
import mrtjp.projectred.core.{TPowerFlow, PowerConductor}
import mrtjp.projectred.transportation.{RoutedJunctionPipePart, IInventoryProvider, TControllerLayer}
import mrtjp.projectred.core.inventory.{SpecialContainer, SimpleInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import mrtjp.projectred.core.inventory.SpecialContainer.ISlotController
import net.minecraft.inventory.{ICrafting, IInventory}
import codechicken.lib.vec.BlockCoord
import mrtjp.projectred.core.libmc.BasicUtils

class TileRouterController extends TileMachine with TileGuiMachine with TControllerLayer
{
    val cpuSlot = new SimpleInventory(1, "inv", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemCPU]
    }

    var client_hasConflict = false

    override def getBlock = ProjectRedExpansion.machine1

    override def doesRotate = false

    override def guiID = MachineGuiFactory.id_controller

    override def createContainer(player:EntityPlayer) = new ControllerCont(this, player)

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        cpuSlot.load(tag, "cpu")
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        cpuSlot.save(tag, "cpu")
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

    def hasControllerConflict =
    {
        BasicUtils.getMultiPart(worldObj, new BlockCoord(this).offset(1), 6) match
        {
            case p:RoutedJunctionPipePart =>
                val r = p.getRouter
                if (r != null && p.maskConnects(0) && r.controllerConflict) true else false
            case _ => false
        }
    }
}

class ControllerCont(tile:TileRouterController, player:EntityPlayer) extends SpecialContainer(player.inventory)
{
    val sc = ISlotController.InventoryRulesController.instance
    addCustomSlot(new SpecialContainer.SlotExtended(tile.cpuSlot, 0, 44, 36).setCheck(sc))
    addPlayerInventory(8, 86)

    var conflict = false

    override def detectAndSendChanges()
    {
        super.detectAndSendChanges()

        val newConflict = tile.hasControllerConflict

        import scala.collection.JavaConversions._
        for (i <- crafters)
        {
            val ic = i.asInstanceOf[ICrafting]
            if (conflict != newConflict) ic.sendProgressBarUpdate(this, 0, if (newConflict) 1 else 2)
        }

        conflict = newConflict
    }

    override def updateProgressBar(id:Int, bar:Int) = id match
    {
        case 0 => tile.client_hasConflict = bar == 1
    }
}
