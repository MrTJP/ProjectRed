package mrtjp.projectred.core.libmc.inventory

import java.util.{List => JList}

import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.core.libmc.gui.GuiLib
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{Container, IInventory, Slot}
import net.minecraft.item.ItemStack

class WidgetContainer extends Container
{
    private var canDrag = true
    private var canInteract = true
    private var canShiftClick = true

    override def canInteractWith(var1:EntityPlayer) = canInteract
    override def canDragIntoSlot(par1Slot:Slot) = canDrag

    def setDrag(flag:Boolean):this.type = {canDrag = flag; this}

    def setShift(flag:Boolean):this.type = {canShiftClick = flag; this}

    def setInteract(flag:Boolean):this.type = {canInteract = flag; this}

    def +(s:Slot2):this.type = {addSlotToContainer(s);this}

    @SideOnly(Side.CLIENT)
    def addPlayerInv(x:Int, y:Int):this.type = addPlayerInv(Minecraft.getMinecraft.thePlayer, x, y)
    def addPlayerInv(player:EntityPlayer, x:Int, y:Int):this.type =
    {
        var next = 0
        def up() = {next+=1;next-1}

        for ((x, y) <- GuiLib.createSlotGrid(x, y+58, 9, 1, 0, 0))
            this + new Slot2(player.inventory, up(), x, y) //hotbar

        for ((x, y) <- GuiLib.createSlotGrid(x, y, 9, 3, 0, 0))
            this + new Slot2(player.inventory, up(), x, y) //slots

        this
    }

    override def slotClick(id:Int, mouse:Int, shift:Int, player:EntityPlayer):ItemStack =
    {
        if (id >= 0 && id < inventorySlots.size)
            inventorySlots.get(id).asInstanceOf[Slot] match
            {
                case slot:Slot2 if slot.ghosting => handleGhostClick(slot, mouse, shift, player)
                case _ => super.slotClick(id, mouse, shift, player)
            }
        else null
    }

    private def handleGhostClick(slot:Slot2, mouse:Int, shift:Int, player:EntityPlayer):ItemStack =
    {
        val inSlot = slot.getStack
        val inCursor = player.inventory.getItemStack
        val stackable = InvWrapper.areItemsStackable(inSlot, inCursor)
        if (stackable)
        {
            if (inSlot != null && inCursor == null) slot.putStack(null)
            else if (inSlot == null && inCursor != null)
            {
                val newStack = inCursor.copy
                newStack.stackSize = if (mouse == 0) Math.min(inCursor.stackSize, slot.getSlotStackLimit) else 1
                slot.putStack(newStack)
            }
            else if (inSlot != null)
            {
                val toAdd = if (shift == 1) 10 else 1
                if (mouse == 0) inSlot.stackSize = Math.min(slot.getSlotStackLimit, inSlot.stackSize+toAdd)
                else if (mouse == 1) inSlot.stackSize = Math.max(0, inSlot.stackSize-toAdd)
                if (inSlot.stackSize > 0) slot.putStack(inSlot)
                else slot.putStack(null)
            }
        }
        else
        {
            val newStack = inCursor.copy
            newStack.stackSize = if (mouse == 0) Math.min(inCursor.stackSize, slot.getSlotStackLimit) else 1
            slot.putStack(newStack)
        }

        inCursor
    }

    override def transferStackInSlot(player:EntityPlayer, slotIdx:Int):ItemStack =
    {
        if (!canShiftClick) return null

        val slot = inventorySlots.get(slotIdx).asInstanceOf[Slot]
        if (slot == null) return null

        var invs = getInvs
        val indx = invs.indexOf(slot.inventory)
        if (indx == -1) return null

        invs = invs.drop(indx) ++ invs.take(indx)

        if (!invs.isDefinedAt(1)) return null
        val toInv = invs(1)

        if (slot.getHasStack)
        {
            val wrap = InvWrapper.wrap(toInv).setSlotsAll()
            val stack = slot.getStack
            val stack2 = stack.copy
            val added = wrap.injectItem(stack, true)
            if (added > 0) toInv.markDirty()
            stack.stackSize -= added

            if (stack.stackSize <= 0) slot.putStack(null)
            else slot.onSlotChanged()

            stack2
        }
        else null
    }

    private def getInvs =
    {
        var vec = Vector[IInventory]()

        import scala.collection.JavaConversions._
        for (s:Slot <- inventorySlots.asInstanceOf[JList[Slot]])
            if (!vec.contains(s.inventory)) vec :+= s.inventory

        vec
    }

    override def retrySlotClick(x:Int, y:Int, par3:Boolean, player:EntityPlayer){}

    //Hack to allow empty containers for use with guis without inventories
    override def putStackInSlot(slot:Int, stack:ItemStack)
    {
        if (inventorySlots.isEmpty || inventorySlots.size < slot) return
        else super.putStackInSlot(slot, stack)
    }
}

class Slot2(inv:IInventory, index:Int, x:Int, y:Int) extends Slot(inv, index, x, y)
{
    var remove = true
    var place = true
    var ghosting = false
    var limit = inventory.getInventoryStackLimit

    var check:SlotCheck = InvSlotCheck

    def setRemove(flag:Boolean):this.type = {remove = flag; this}
    def setPlace(flag:Boolean):this.type = {place = flag; this}
    def setGhosting(flag:Boolean):this.type = {ghosting = flag; this}
    def setLimit(lim:Int):this.type = {limit = lim; this}
    def setCheck(c:SlotCheck):this.type = {check = c; this}

    override def getSlotStackLimit = limit

    override def isItemValid(stack:ItemStack) =
        place && (check == null || check.canPlace(this, stack))

    override def canTakeStack(player:EntityPlayer) =
        remove && (check == null || check.canTake(this))
}

trait SlotCheck
{
    def canTake(slot:Slot2):Boolean
    def canPlace(slot:Slot2, stack:ItemStack):Boolean
}

object InvSlotCheck extends SlotCheck
{
    def canTake(slot:Slot2) = true

    def canPlace(slot:Slot2, stack:ItemStack) =
        slot.inventory.isItemValidForSlot(slot.getSlotIndex, stack)
}