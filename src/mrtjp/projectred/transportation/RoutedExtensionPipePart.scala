package mrtjp.projectred.transportation

import mrtjp.core.gui.{NodeContainer, Slot3}
import mrtjp.core.inventory.SimpleInventory
import mrtjp.core.item.ItemKeyStack
import mrtjp.projectred.core.ItemDataCard
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition

class RoutedExtensionPipePart extends AbstractNetPipe with TNetworkPipe
{
    var cardslot = new SimpleInventory(2, "card", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemDataCard]
    }

    private var lost = List[ItemKeyStack]()
    private var remainingDelay = operationDelay

    /**
     * Lost items handling delay
     */
    private def operationDelay = 40

    override def updateServer()
    {
        super.updateServer()
        remainingDelay -= 1
        if(remainingDelay <= 0)
        {
            remainingDelay = operationDelay
            lostHandleTick()
        }
        handleCardsTick()
    }

    private def handleCardsTick()
    {
        if(cardslot.getStackInSlot(0) != null && cardslot.getStackInSlot(1) == null)
        {
            val stack = cardslot.getStackInSlot(0)
            if(stack.getItem.isInstanceOf[ItemDataCard])
                ItemDataCard.setData(stack, "extension", getRouter.getID.toString)

            cardslot.setInventorySlotContents(0, null)
            cardslot.setInventorySlotContents(1, stack)
            cardslot.markDirty()
        }
    }

    private def lostHandleTick()
    {
        if(lost.isEmpty) return

        import scala.util.control.Breaks._
        while(!lost.isEmpty) breakable
        {
            val stack = lost.head
            lost = lost.tail

            var toRequest = stack.stackSize
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key))
            if(toRequest <= 0)
            {
                lost :+= stack
                break()
            }
            val req = new RequestConsole(RequestFlags.full).setDestination(this)
            val requested = req.makeRequest(ItemKeyStack.get(stack.key, toRequest)).requested
            if(requested < stack.stackSize)
            {
                stack.stackSize -= requested
                lost :+= stack
            }
        }
    }

    override def trackedItemLost(s:ItemKeyStack)
    {
        lost :+= s
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        cardslot.saveInv(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        cardslot.loadInv(tag)
    }

    override def onRemoved()
    {
        if(!world.isRemote) cardslot.dropInvContents(world, x, y, z)
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if(super.activate(player, hit, item)) return true
        if(!player.isSneaking)
        {
            openGui(player)
            true
        }
        else false
    }

    private def openGui(player:EntityPlayer)
    {
        if(world.isRemote) return

        GuiExtensionPipe.open(player, createContainer(player),
            _.writeCoord(x, y, z).writeString(getRouter.getID.toString))
    }

    def createContainer(player:EntityPlayer) =
    {
        val container = new NodeContainer
        container.addSlotToContainer(new Slot3(cardslot, 0, 134, 20))
        val out = new Slot3(cardslot, 1, 134, 50)
        out.canPlaceDelegate = {_ => false}
        container.addSlotToContainer(out)
        container.addPlayerInv(player, 8, 84)
        container
    }
}