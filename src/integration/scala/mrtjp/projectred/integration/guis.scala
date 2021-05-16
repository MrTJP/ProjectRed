/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.gui.{MCButtonNode, NodeGui}
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.ProjectRedIntegration
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.util.ResourceLocation
import net.minecraft.util.text.StringTextComponent


class GuiTimer(part:ITimerGuiLogic) extends NodeGui(w = 256, h = 55, title = new StringTextComponent(part.getType.getRegistryName.toString))
{
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

    override def drawBack_Impl(stack:MatrixStack, mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiTimer.background)
        blit(stack, 0, 0, 0, 0, size.width, size.height)

        val s = "Timer interval: "+"%.2f".format(part.getTimerMax*0.05)+"s"
        val sw = getFontRenderer.width(s)
        getFontRenderer.draw(stack, s, (getXSize-sw)/2, 8, 0x404040)
    }
}

object GuiTimer {
    val background = new ResourceLocation(ProjectRedIntegration.MOD_ID, "textures/gui/timer_gate.png")

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

class GuiCounter(part:ICounterGuiLogic) extends NodeGui(w = 256, h = 145, title = new StringTextComponent(part.getType.getRegistryName.toString))
{
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

    override def drawBack_Impl(stack:MatrixStack, mouse:Point, frame:Float) =
    {
        TextureUtils.changeTexture(GuiCounter.background)
        blit(stack, 0, 0, 0, 0, size.width, size.height)

        var s = "Maximum: "+part.getCounterMax
        getFontRenderer.draw(stack, s, (getXSize-getFontRenderer.width(s))/2, 5, 0x404040)
        s = "Increment: "+part.getCounterIncr
        getFontRenderer.draw(stack, s, (getXSize-getFontRenderer.width(s))/2, 45, 0x404040)
        s = "Decrement: "+part.getCounterDecr
        getFontRenderer.draw(stack, s, (getXSize-getFontRenderer.width(s))/2, 85, 0x404040)
        s = "State: "+part.getCounterValue
        getFontRenderer.draw(stack, s, (getXSize-getFontRenderer.width(s))/2, 125, 0x404040)
    }

    override def update_Impl()
    {
        if (part.tile == null) mcInst.player.closeContainer()
    }
}

object GuiCounter {
    val background = new ResourceLocation(ProjectRedIntegration.MOD_ID, "textures/gui/counter_gate.png")

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
