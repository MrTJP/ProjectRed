package mrtjp.projectred.transportation

import codechicken.core.IGuiPacketSender
import codechicken.core.ServerUtils
import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.core.ItemDataCard
import mrtjp.projectred.core.inventory.GhostContainer2
import mrtjp.projectred.core.inventory.GhostContainer2.ISlotController
import mrtjp.projectred.core.inventory.GhostContainer2.SlotExtended
import mrtjp.projectred.core.inventory.SimpleInventory
import mrtjp.projectred.core.utils.ItemKeyStack
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.entity.player.EntityPlayerMP
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.MovingObjectPosition
import java.util.LinkedList

class RoutedExtensionPipePart extends RoutedJunctionPipePart
{
    private var lost = List[ItemKeyStack]()
    private var remainingDelay = operationDelay

    /**
     * Lost items handling delay
     */
    private def operationDelay = 40

    override def updateServer()
    {
        remainingDelay -= 1
        if (remainingDelay <= 0)
        {
            remainingDelay = operationDelay
            lostHandleTick()
        }
        handleCardsTick()
    }

    private def handleCardsTick()
    {
        if (cardslot.getStackInSlot(0) != null && cardslot.getStackInSlot(1) == null)
        {
            val stack:ItemStack = cardslot.getStackInSlot(0)
            if (stack.getItem.isInstanceOf[ItemDataCard])
            {
                val data:NBTTagCompound = new NBTTagCompound
                val id = getRouter.getID.toString
                data.setString("id", id)
                val s = "- ID: " + id.split("-")(0) + "..."
                ItemDataCard.nameData(data, "Extension pipe data", s)
                ItemDataCard.saveData(stack, data, "ext_pipe")
            }
            cardslot.setInventorySlotContents(0, null)
            cardslot.setInventorySlotContents(1, stack)
            cardslot.onInventoryChanged()
        }
    }

    private def lostHandleTick()
    {
        if (lost.isEmpty) return

        import mrtjp.projectred.core.utils.LabelBreaks._
        while (!lost.isEmpty) label
        {
            val stack =
            {
                val f = lost(0)
                lost = lost.filterNot(p => p == f)
                f
            }

            var toRequest = stack.stackSize
            toRequest = Math.min(toRequest, getActiveFreeSpace(stack.key))
            if (toRequest <= 0)
            {
                lost :+= stack
                break
            }

            val req = new RequestConsole().setDestination(this)
            req.setPulling(true).setCrafting(true).setPartials(true)
            val requested = req.makeRequest(ItemKeyStack.get(stack.key, toRequest)).requested
            if (requested < stack.stackSize)
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
        cardslot.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        cardslot.load(tag)
    }

    override def onRemoved()
    {
        if (!world.isRemote) cardslot.dropContents(world, x, y, z)
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        openGui(player)
        true
    }

    private def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
        {
            def sendPacket(player:EntityPlayerMP, windowId:Int)
            {
                val p = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_ExtensionPipe_open)
                p.writeCoord(x, y, z)
                p.writeByte(windowId)
                p.writeString(getRouter.getID.toString)
                p.sendToPlayer(player)
            }
        })
    }

    def createContainer(player:EntityPlayer):Container =
    {
        val ghost = new GhostContainer2(player.inventory)
        val sc = ISlotController.InventoryRulesController.instance
        ghost.addCustomSlot(new GhostContainer2.SlotExtended(cardslot, 0, 134, 20).setCheck(sc))
        ghost.addCustomSlot(new GhostContainer2.SlotExtended(cardslot, 1, 134, 50).setPlacement(false))
        ghost.addPlayerInventory(8, 84)
        ghost
    }

    override def getType = "pr_rextension"

    var cardslot = new SimpleInventory(2, "card", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
            stack != null && stack.getItem.isInstanceOf[ItemDataCard]
    }
}