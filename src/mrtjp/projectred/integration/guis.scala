/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import mrtjp.core.gui.{GuiLib, MCButtonNode, NodeGui, TGuiFactory}
import mrtjp.core.vec.{Point, Size}
import net.minecraft.client.Minecraft
import net.minecraft.entity.player.EntityPlayer
import net.minecraftforge.fml.relauncher.{Side, SideOnly}


class GuiTimer(part:GatePart) extends NodeGui(256, 55)
{
    val logic = part.getLogic[ITimerGuiLogic]

    {
        def createButton(x:Int, y:Int, w:Int, h:Int, text:String, delta:Int) =
        {
            val b = new MCButtonNode
            b.position = Point(x, y)
            b.size = Size(w, h)
            b.text = text
            b.clickDelegate = {() =>
                val packet = new PacketCustom(IntegrationCPH.channel, 1)
                IntegrationCPH.writePartIndex(packet, part)
                packet.writeShort(delta)
                packet.sendToServer()
            }
            addChild(b)
        }

        createButton(5, 25, 40, 20, "-10s", -200)
        createButton(46, 25, 40, 20, "-1s", -20)
        createButton(87, 25, 40, 20, "-50ms", -1)
        createButton(129, 25, 40, 20, "+50ms", 1)
        createButton(170, 25, 40, 20, "+1s", 20)
        createButton(211, 25, 40, 20, "+10s", 200)
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, 0)
        val s = "Timer interval: "+"%.2f".format(logic.getTimerMax*0.05)+"s"
        val sw = fontRendererObj.getStringWidth(s)
        fontRendererObj.drawString(s, (xSize-sw)/2, 8, 0x404040)
    }
}

object GuiTimer extends TGuiFactory
{
    override def getID = IntegrationProxy.timerGui

    def open(player:EntityPlayer, gate:GatePart)
    {
        open(player, null, IntegrationCPH.writePartIndex(_, gate))
    }

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val world = Minecraft.getMinecraft.world
        IntegrationCPH.readPartIndex(world, data) match
        {
            case gate:GatePart if gate.getLogic.isInstanceOf[ITimerGuiLogic] => new GuiTimer(gate)
            case _ => null
        }
    }
}

class GuiCounter(part:GatePart) extends NodeGui(256, 145)
{
    val logic = part.getLogic[ICounterGuiLogic]

    override def onAddedToParent_Impl()
    {
        def createButton(x:Int, y:Int, w:Int, h:Int, id:Int, delta:Int) =
        {
            val b = new MCButtonNode
            b.position = Point(x, y)
            b.size = Size(w, h)
            b.text = (if (delta < 0) "" else "+")+delta
            b.clickDelegate = {() =>
                val packet = new PacketCustom(IntegrationCPH.channel, 2)
                IntegrationCPH.writePartIndex(packet, part)
                packet.writeByte(id)
                packet.writeShort(delta)
                packet.sendToServer()
            }
            addChild(b)
        }

        for (row <- 0 until 3)
        {
            val y = 16+40*row
            createButton(5, y, 40, 20, row, -10)
            createButton(46, y, 40, 20, row, -5)
            createButton(87, y, 40, 20, row, -1)
            createButton(129, y, 40, 20, row, 1)
            createButton(170, y, 40, 20, row, 5)
            createButton(211, y, 40, 20, row, 10)
        }
    }

    override def drawBack_Impl(mouse:Point, frame:Float) =
    {
        GuiLib.drawGuiBox(0, 0, xSize, ySize, 0)
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
        if (part.tile == null) mc.player.closeScreen()
    }
}

object GuiCounter extends TGuiFactory
{
    override def getID = IntegrationProxy.counterGui

    def open(player:EntityPlayer, gate:GatePart)
    {
        open(player, null, IntegrationCPH.writePartIndex(_, gate))
    }

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val world = Minecraft.getMinecraft.world
        IntegrationCPH.readPartIndex(world, data) match
        {
            case gate:GatePart if gate.getLogic.isInstanceOf[ICounterGuiLogic] => new GuiCounter(gate)
            case _ => null
        }
    }
}
