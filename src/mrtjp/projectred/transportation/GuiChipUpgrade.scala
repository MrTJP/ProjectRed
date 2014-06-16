package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.FontUtils
import net.minecraft.util.EnumChatFormatting
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.libmc.{ResourceLib, PRColors}
import mrtjp.projectred.core.libmc.gui._
import mrtjp.projectred.core.{PartDefs, ItemPart, GuiIDs, TGuiBuilder}
import net.minecraft.entity.player.EntityPlayer
import codechicken.lib.data.MCDataInput
import mrtjp.projectred.core.libmc.inventory.{Slot2, SimpleInventory, WidgetContainer}
import net.minecraft.item.ItemStack
import cpw.mods.fml.relauncher.{Side, SideOnly}

class ChipUpgradeContainer(player:EntityPlayer) extends WidgetContainer
{
    val upgradeInv = new SimpleInventory(7, "upBus", 1)
    {
        override def isItemValidForSlot(i:Int, stack:ItemStack) =
        {
            if (i == 6)
                stack != null &&
                    stack.getItem.isInstanceOf[ItemRoutingChip] &&
                    stack.hasTagCompound && stack.getTagCompound.hasKey("chipROM")

            else if (stack.getItem.isInstanceOf[ItemPart])
            {
                val slotForMeta = stack.getItemDamage-PartDefs.CHIPUPGRADE_LX.meta
                slotForMeta == i
            }
            else false
        }

        override def markDirty()
        {
            super.markDirty()
            refreshChips()
        }
    }

    val slot = player.inventory.currentItem

    addPlayerInv(player, 8, 86)
    private var s = 0
    private def next = {s += 1; s-1}
    for ((x, y) <- GuiLib.createSlotGrid(8, 18, 1, 3, 2, 2))
        this + new Slot2(upgradeInv, next, x, y)
    for ((x, y) <- GuiLib.createSlotGrid(152, 18, 1, 3, 2, 2))
        this + new Slot2(upgradeInv, next, x, y)
    this + new Slot2(upgradeInv, next, 80, 38)

    override def onContainerClosed(p:EntityPlayer)
    {
        super.onContainerClosed(p)
        for (i <- 0 until upgradeInv.getSizeInventory)
            if (upgradeInv.getStackInSlot(i) != null)
            {
                p.dropPlayerItemWithRandomChoice(upgradeInv.getStackInSlot(i), false)
                upgradeInv.setInventorySlotContents(i, null)
            }
        upgradeInv.markDirty()
    }

    override def +(s:Slot2):this.type =
    {
        if (s.getSlotIndex == slot && s.inventory == player.inventory)
            s.setRemove(false)
        super.+(s)
    }

    private var chip:RoutingChipset = null
    private def refreshChips()
    {
        val stack = upgradeInv.getStackInSlot(6)
        val c = ItemRoutingChip.loadChipFromItemStack(stack)
        if (chip != c) chip = c
    }

    def install()
    {
        val r = chip
        if (r != null)
        {
            val bus = r.upgradeBus
            for (i <- 0 until 6)
            {
                val stack = upgradeInv.getStackInSlot(i)
                if (stack != null)
                {
                    if (i < 3)
                    {
                        if (bus.installL(i, true))
                            upgradeInv.setInventorySlotContents(i, null)
                    }
                    else
                    {
                        if (bus.installR(i-3, true))
                            upgradeInv.setInventorySlotContents(i, null)

                    }
                }
            }
        }

        val chipStack = upgradeInv.getStackInSlot(6)
        ItemRoutingChip.saveChipToItemStack(chipStack, r)
        upgradeInv.setInventorySlotContents(6, chipStack)
        detectAndSendChanges()
    }

    def getChip = chip
}

class GuiChipUpgrade(container:ChipUpgradeContainer) extends WidgetGui(container, 176, 200)
{
    override def runInit_Impl()
    {
        add(new WidgetButtonMC(xSize / 2 - 20, 56, 40, 15).setText("Install").setAction("inst"))
        add(new WidgetDotSelect(67, 45)
        {
            override def buildTooltip(list:ListBuffer[String])
            {
                if (container.getChip != null)
                {
                    val b = container.getChip.upgradeBus
                    if (b.maxL > 0)
                    {
                        list += "L slot"
                        list += (EnumChatFormatting.GRAY.toString+"Latency: "+b.LLatency)
                        list += ""
                        list += (EnumChatFormatting.GRAY.toString+b.Linfo)
                        list += (EnumChatFormatting.YELLOW.toString+b.Lformula)
                    }
                    else list += "not upgradable"
                }
            }
        })
        add(new WidgetDotSelect(110, 45)
        {
            override def buildTooltip(list:ListBuffer[String])
            {
                if (container.getChip != null)
                {
                    val b = container.getChip.upgradeBus
                    if (b.maxR > 0)
                    {
                        list += "R slot"
                        list += (EnumChatFormatting.GRAY+"Latency: "+b.RLatency)
                        list += ""
                        list += (EnumChatFormatting.GRAY+b.Rinfo)
                        list += (EnumChatFormatting.YELLOW+b.Rformula)
                    }
                    else list += "not upgradable"
                }
            }
        })
    }

    override def receiveMessage_Impl(message:String)
    {
        new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_RouterUtil_action).writeString(message).sendToServer()
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        ResourceLib.guiChipUpgrade.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
        if (container.getChip != null)
        {
            val b = container.getChip.upgradeBus
            fontRenderer.drawString(String.valueOf(if (b.LXLatency > 0) b.LXLatency else "-"), 29, 23, PRColors.GREY.rgb)
            fontRenderer.drawString(String.valueOf(if (b.LYLatency > 0) b.LYLatency else "-"), 29, 43, PRColors.GREY.rgb)
            fontRenderer.drawString(String.valueOf(if (b.LZLatency > 0) b.LZLatency else "-"), 29, 63, PRColors.GREY.rgb)
            FontUtils.drawRightString(String.valueOf(if (b.RXLatency > 0) b.RXLatency else "-"), 148, 23, PRColors.GREY.rgb)
            FontUtils.drawRightString(String.valueOf(if (b.RYLatency > 0) b.RYLatency else "-"), 148, 43, PRColors.GREY.rgb)
            FontUtils.drawRightString(String.valueOf(if (b.RZLatency > 0) b.RZLatency else "-"), 148, 63, PRColors.GREY.rgb)
        }
        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(8, 18, 1, 3, 2, 2))
        {
            val color = getColorForSlot(s)
            s+=1
            drawGradientRect(x-2, y+4, x, y+12, color, color)
        }
        for ((x, y) <- GuiLib.createSlotGrid(152, 18, 1, 3, 2, 2))
        {
            val color = getColorForSlot(s)
            s+=1
            drawGradientRect(x+16, y+4, x+18, y+12, color, color)
        }
    }

    private def getColorForSlot(slot:Int):Int =
    {
        if (container.getChip != null)
        {
            val b = container.getChip.upgradeBus
            var hasChip = false
            var canInstall = false
            var canHandle = false
            if (slot < 3)
            {
                hasChip = b.Lset(slot)
                canInstall = b.installL(slot, false)
                canHandle = b.maxL > slot
            }
            else if (slot < 6)
            {
                hasChip = b.Rset(slot - 3)
                canInstall = b.installR(slot - 3, false)
                canHandle = b.maxR > slot - 3
            }
            var color = 0
            if (hasChip) color = 0xff6BF100
            else if (canInstall) color = 0xffBFBF00
            else if (canHandle) color = 0xffA20F06
            else color = 0xff535353
            return color
        }
        0
    }
}

object GuiChipUpgrade extends TGuiBuilder
{
    override def getID = GuiIDs.chipUpgrade

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
        new GuiChipUpgrade(new ChipUpgradeContainer(player))
}