package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.GuiManager
import mrtjp.projectred.core.libmc.inventory.{SimpleInventory, Slot2, WidgetContainer}
import mrtjp.projectred.transportation.{ItemCPU, ItemCreativeCPU, TControllerLayer}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound

class TileRouterController extends TileMachine with TileGuiMachine with TControllerLayer
{
    val cpuSlot = new SimpleInventory(1, "inv", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && (stack.getItem.isInstanceOf[ItemCPU] || stack.getItem.isInstanceOf[ItemCreativeCPU])

        override def markDirty()
        {
            if (world.isRemote) return
            val old = operational
            operational = isOperational
            if (operational != old) sendOpUpdate()
        }
    }

    var client_hasConflict = false
    var operational = false //TODO sync to client

    override def getBlock = ProjectRedExpansion.machine2

    override def doesRotate = false

    override def openGui(player:EntityPlayer)
    {
        GuiRouterController.open(player, createContainer(player), _.writeCoord(xCoord, yCoord, zCoord))
    }

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
        if (stack != null && !stack.getItem.isInstanceOf[ItemCreativeCPU])
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
            if (stack.getItem.isInstanceOf[ItemCreativeCPU]) {
                Short.MaxValue
            } else {
                assureNBT(stack)
                stack.getTagCompound.getDouble("cycles")
            }
        }
        else 0
    }

    override def usePower(P:Double) =
    {
        val stack = cpuSlot.getStackInSlot(0)
        if (stack != null)
        {
            if (stack.getItem.isInstanceOf[ItemCreativeCPU]) {
                true
            } else {
                assureNBT(stack)
                val newPow = stack.getTagCompound.getDouble("cycles")-P
                stack.getTagCompound.setDouble("cycles", newPow)
                newPow >= 0
            }
        }
        else false
    }

    def isOperational:Boolean = getPower > 0

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeBoolean(operational)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        operational = in.readBoolean()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 2 =>
            operational = in.readBoolean()
            markRender()
        case _ => super.read(in, key)
    }

    def sendOpUpdate()
    {
        writeStreamSend(writeStream(2).writeBoolean(operational))
    }
}

class ControllerCont(tile:TileRouterController, player:EntityPlayer) extends WidgetContainer
{
    this + new Slot2(tile.cpuSlot, 0, 44, 36)
    addPlayerInv(player, 8, 86)
}
