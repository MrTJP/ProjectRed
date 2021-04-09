/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.packet.PacketCustom
import mrtjp.core.gui.{GuiLib, MCButtonNode, NodeGui}
import mrtjp.core.vec.{Point, Size}
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.util.text.StringTextComponent


class GuiTimer(part:GatePart) extends NodeGui(w = 256, h = 55, title = new StringTextComponent("guicontainertimer"))
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
                val packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.INCR_TIMER_FROM_CLIENT)
                IntegrationNetwork.writePartIndex(packet, part)
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
        val sw = getFontRenderer.getStringWidth(s)
        getFontRenderer.drawString(s, (xSize-sw)/2, 8, 0x404040)
    }
}

object GuiTimer {
    def open(player:PlayerEntity, gate:GatePart):Unit = {
        val packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_TIMER_GUI_FROM_SERVER)
        IntegrationNetwork.writePartIndex(packet, gate)
        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
    }
}

//object GuiTimer extends TGuiFactory
//{
//    override def getID = IntegrationProxy.timerGui
//
//    def open(player:PlayerEntity, gate:GatePart)
//    {
//        open(player, null, IntegrationCPH.writePartIndex(_, gate))
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        val world = Minecraft.getMinecraft.world
//        IntegrationCPH.readPartIndex(world, data) match
//        {
//            case gate:GatePart if gate.getLogic.isInstanceOf[ITimerGuiLogic] => new GuiTimer(gate)
//            case _ => null
//        }
//    }
//}

class GuiCounter(part:GatePart) extends NodeGui(w = 256, h = 145, title = new StringTextComponent("guicontainercounter"))
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
                val packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.INCR_COUNTER_FROM_CLIENT)
                IntegrationNetwork.writePartIndex(packet, part)
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
        getFontRenderer.drawString(s, (xSize-getFontRenderer.getStringWidth(s))/2, 5, 0x404040)
        s = "Increment: "+logic.getCounterIncr
        getFontRenderer.drawString(s, (xSize-getFontRenderer.getStringWidth(s))/2, 45, 0x404040)
        s = "Decrement: "+logic.getCounterDecr
        getFontRenderer.drawString(s, (xSize-getFontRenderer.getStringWidth(s))/2, 85, 0x404040)
        s = "State: "+logic.getCounterValue
        getFontRenderer.drawString(s, (xSize-getFontRenderer.getStringWidth(s))/2, 125, 0x404040)
    }

    override def update_Impl()
    {
        if (part.tile == null) mcInst.player.closeScreen()
    }
}

object GuiCounter {
    def open(player:PlayerEntity, gate:GatePart):Unit = {
        val packet = new PacketCustom(IntegrationNetwork.NET_CHANNEL, IntegrationNetwork.OPEN_COUNTER_GUI_FROM_SERVER)
        IntegrationNetwork.writePartIndex(packet, gate)
        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
    }
}

//object GuiCounter extends TGuiFactory
//{
//    override def getID = IntegrationProxy.counterGui
//
//    def open(player:EntityPlayer, gate:GatePart)
//    {
//        open(player, null, IntegrationCPH.writePartIndex(_, gate))
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def buildGui(player:EntityPlayer, data:MCDataInput) =
//    {
//        val world = Minecraft.getMinecraft.world
//        IntegrationCPH.readPartIndex(world, data) match
//        {
//            case gate:GatePart if gate.getLogic.isInstanceOf[ICounterGuiLogic] => new GuiCounter(gate)
//            case _ => null
//        }
//    }
//}
