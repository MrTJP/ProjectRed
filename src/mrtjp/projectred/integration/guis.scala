/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.gui.{GuiLib, TGuiBuilder, WidgetButtonMC, WidgetGui}
import mrtjp.core.vec.Point
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayer


class GuiTimer(part:GatePart) extends WidgetGui(256, 55)
{
    val logic = part.getLogic.asInstanceOf[ITimerGuiLogic]

    override def runInit_Impl()
    {
        add(new WidgetButtonMC(5, 25, 40, 20).setText("-10s").setAction("-200"))
        add(new WidgetButtonMC(46, 25, 40, 20).setText("-1s").setAction("-20"))
        add(new WidgetButtonMC(87, 25, 40, 20).setText("-50ms").setAction("-1"))
        add(new WidgetButtonMC(129, 25, 40, 20).setText("+50ms").setAction("+1"))
        add(new WidgetButtonMC(170, 25, 40, 20).setText("+1s").setAction("+20"))
        add(new WidgetButtonMC(211, 25, 40, 20).setText("+10s").setAction("+200"))
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, getZ)
        val s = "Timer interval: "+"%.2f".format(logic.getTimerMax*0.05)+"s"
        val sw = fontRendererObj.getStringWidth(s)
        fontRendererObj.drawString(s, (xSize-sw)/2, 8, 0x404040)
    }

    override def receiveMessage_Impl(message1:String)
    {
        var message = message1
        if (message.startsWith("+")) message = message.substring(1)
        val value = Integer.parseInt(message)

        val packet = new PacketCustom(IntegrationCPH.channel, 1)
        IntegrationCPH.writePartIndex(packet, part)
        packet.writeShort(value)
        packet.sendToServer()
    }
}

object GuiTimer extends TGuiBuilder
{
    override def getID = IntegrationProxy.timerGui

    def open(player:EntityPlayer, gate:GatePart)
    {
        open(player, null, IntegrationCPH.writePartIndex(_, gate))
    }

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val world = Minecraft.getMinecraft.theWorld
        IntegrationCPH.readPartIndex(world, data) match
        {
            case gate:GatePart if gate.getLogic.isInstanceOf[ITimerGuiLogic] => new GuiTimer(gate)
            case _ => null
        }
    }
}

class GuiCounter(part:GatePart) extends WidgetGui(256, 145)
{
    val logic = part.getLogic.asInstanceOf[ICounterGuiLogic]

    override def runInit_Impl()
    {
        for (row <- 0 until 3)
        {
            val y = 16+40*row
            add(new WidgetButtonMC(5, y, 40, 20).setText("-10").setAction(row+"-10"))
            add(new WidgetButtonMC(46, y, 40, 20).setText("-5").setAction(row+"-5"))
            add(new WidgetButtonMC(87, y, 40, 20).setText("-1").setAction(row+"-1"))
            add(new WidgetButtonMC(129, y, 40, 20).setText("+1").setAction(row+"+1"))
            add(new WidgetButtonMC(170, y, 40, 20).setText("+5").setAction(row+"+5"))
            add(new WidgetButtonMC(211, y, 40, 20).setText("+10").setAction(row+"+10"))
        }
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, zLevel)
        var s = "Maximum: "+logic.getCounterMax
        fontRendererObj.drawString(s, (xSize-fontRendererObj.getStringWidth(s))/2, 5, 0x404040)
        s = "Increment: "+logic.getCounterIncr
        fontRendererObj.drawString(s, (xSize-fontRendererObj.getStringWidth(s))/2, 45, 0x404040)
        s = "Decrement: "+logic.getCounterDecr
        fontRendererObj.drawString(s, (xSize-fontRendererObj.getStringWidth(s))/2, 85, 0x404040)
        s = "State: "+logic.getCounterValue
        fontRendererObj.drawString(s, (xSize-fontRendererObj.getStringWidth(s))/2, 125, 0x404040)
    }

    override def update_Impl()
    {
        if (part.tile == null) mc.thePlayer.closeScreen()
    }

    override def receiveMessage_Impl(message1:String)
    {
        var message = message1
        val id = Integer.parseInt(message.substring(0, 1))
        message = message.substring(1)
        if (message.startsWith("+")) message = message.substring(1)
        val value = Integer.parseInt(message)

        val packet = new PacketCustom(IntegrationCPH.channel, 2)
        IntegrationCPH.writePartIndex(packet, part)
        packet.writeByte(id)
        packet.writeShort(value)
        packet.sendToServer()
    }
}

object GuiCounter extends TGuiBuilder
{
    override def getID = IntegrationProxy.counterGui

    def open(player:EntityPlayer, gate:GatePart)
    {
        open(player, null, IntegrationCPH.writePartIndex(_, gate))
    }

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val world = Minecraft.getMinecraft.theWorld
        IntegrationCPH.readPartIndex(world, data) match
        {
            case gate:GatePart if gate.getLogic.isInstanceOf[ICounterGuiLogic] => new GuiCounter(gate)
            case _ => null
        }
    }
}