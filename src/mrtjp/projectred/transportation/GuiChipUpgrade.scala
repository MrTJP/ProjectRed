package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.FontUtils
import mrtjp.projectred.core.inventory._
import mrtjp.projectred.transportation.ItemRouterUtility.ChipUpgradeContainer
import net.minecraft.util.EnumChatFormatting
import net.minecraft.util.ResourceLocation
import scala.collection.mutable.ListBuffer
import mrtjp.projectred.core.libmc.{PRColors, BasicGuiUtils}

object GuiChipUpgrade
{
    private final val resource = new ResourceLocation("projectred:textures/gui/chipupgradecontainer.png")
}

class GuiChipUpgrade(container:ChipUpgradeContainer) extends SpecialGuiContainer(container, null, 176, 200)
{
    override def addWidgets()
    {
        add((new WidgetButton(xSize/2-20, 56, 40, 15) with TButtonMCStyle with TButtonTextOverlay).setText("Install").setActionCommand("inst"))
        add(new WidgetDotSelector(67, 45)
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
                    else list += ("not upgradable")
                }
            }
        })
        add(new WidgetDotSelector(110, 45)
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

    override def actionPerformed(ident:String)
    {
        new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_RouterUtil_action).writeString(ident).sendToServer()
    }

    override def drawBackground()
    {
        CCRenderState.changeTexture(GuiChipUpgrade.resource)
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
        import scala.collection.JavaConversions._
        for (coord <- BasicGuiUtils.createSlotArray(8, 18, 1, 3, 2, 2))
        {
            val x = coord.get1
            val y = coord.get2
            val color = getColorForSlot(s)
            s+=1
            drawGradientRect(x-2, y+4, x, y+12, color, color)
        }
        for (coord <- BasicGuiUtils.createSlotArray(152, 18, 1, 3, 2, 2))
        {
            val x = coord.get1
            val y = coord.get2
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