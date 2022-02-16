/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.vec.Point
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.redui.{ButtonNode, RedUIScreen}
import net.minecraft.entity.player.{PlayerEntity, ServerPlayerEntity}
import net.minecraft.util.ResourceLocation
import net.minecraft.util.text.StringTextComponent

class GuiTimer(part:ITimerGuiLogic) extends RedUIScreen(256, 55, new StringTextComponent(part.getType.getRegistryName.toString))
{
    private def createButton(x:Int, y:Int, w:Int, h:Int, text:String, delta:Int):Unit = {
        val b = new ButtonNode
        b.setPosition(x, y)
        b.setSize(w, h)
        b.setButtonText(text)
        b.setClickFunction { () =>
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

    override def drawBack(stack: MatrixStack, mouse: Point, partialFrame: Float): Unit = {
        TextureUtils.changeTexture(GuiTimer.background)
        blit(stack, getFrame.x, getFrame.y, 0, 0, getFrame.width, getFrame.height)

        val s = "Timer interval: "+"%.2f".format(part.getTimerMax*0.05)+"s"
        val sw = getFontRenderer.width(s)
        getFontRenderer.draw(stack, s, getFrame.x + (getFrame.width-sw)/2, getFrame.y + 8, 0x404040)
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

class GuiCounter(part:ICounterGuiLogic) extends RedUIScreen(256, 145, new StringTextComponent(part.getType.getRegistryName.toString))
{
    private def createButton(x:Int, y:Int, w:Int, h:Int, id:Int, delta:Int):Unit = {
        val b = new ButtonNode
        b.setPosition(x, y)
        b.setSize(w, h)
        b.setButtonText((if (delta < 0) "" else "+") + delta)
        b.setClickFunction { () =>
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

    override def drawBack(stack: MatrixStack, mouse: Point, partialFrame: Float): Unit = {
        TextureUtils.changeTexture(GuiCounter.background)
        blit(stack, getFrame.x, getFrame.y, 0, 0, getFrame.width, getFrame.height)

        val x = getFrame.x
        val y = getFrame.y
        val w = getFrame.width

        var s = "Maximum: "+part.getCounterMax
        getFontRenderer.draw(stack, s, x + (w-getFontRenderer.width(s))/2, y + 5, 0x404040)
        s = "Increment: "+part.getCounterIncr
        getFontRenderer.draw(stack, s, x + (w-getFontRenderer.width(s))/2, y + 45, 0x404040)
        s = "Decrement: "+part.getCounterDecr
        getFontRenderer.draw(stack, s, x + (w-getFontRenderer.width(s))/2, y + 85, 0x404040)
        s = "State: "+part.getCounterValue
        getFontRenderer.draw(stack, s, x + (w-getFontRenderer.width(s))/2, y + 125, 0x404040)
    }

    override def update(): Unit = {
        if (part.tile == null) getMinecraft.player.closeContainer()
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
