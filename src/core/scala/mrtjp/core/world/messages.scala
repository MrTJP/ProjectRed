/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.world

import codechicken.lib.packet.PacketCustom
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.core.handler.MrTJPCoreNetwork
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.IRenderTypeBuffer
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.vector.{Quaternion, Vector3f}
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

import scala.collection.mutable

object Messenger
{
    val messages = mutable.ListBuffer[Message]()
    val options = Seq[MailOption](Replace, Combine)

    def createPacket = new PacketCustom(MrTJPCoreNetwork.NET_CHANNEL, MrTJPCoreNetwork.C_ADD_MESSAGE)

    /**
     * Adds a string to the location. To apply an option, add a "/#" + an option
     * char anywhere in the string.
     *
     * f - Override a message already at that location.
     * c - Combine message if one already exists there.
     *
     * @param x
     * @param y
     * @param z
     * @param mail
     */
    def addMessage(x: Double, y: Double, z: Double, mail: String)
    {
        val location = new BlockPos(Math.floor(x).asInstanceOf[Int], Math.floor(y).asInstanceOf[Int], Math.floor(z).asInstanceOf[Int])

        val mess = new Message().set(location, x, y, z, mail)

        options.foreach(op => op.modify(mess))

        if (messages.size > 64) messages.remove(0)

        messages += mess
    }

    @SubscribeEvent
    @OnlyIn(Dist.CLIENT)
    def renderMessages(event:RenderWorldLastEvent)
    {
        val w = Minecraft.getInstance.level
        if (w == null) return
        if (Messenger.messages.isEmpty) return

        val deathTime = System.currentTimeMillis-3000L
        val projectedView = Minecraft.getInstance.gameRenderer.getMainCamera.getPosition
        val buffers = Minecraft.getInstance.renderBuffers().bufferSource()
        val mStack = event.getMatrixStack
        mStack.pushPose()
        mStack.translate(-projectedView.x, -projectedView.y, -projectedView.z)

        for (m <- Messenger.messages.clone())
            if (m == null || m.receivedOn < deathTime) Messenger.messages -= m
            else readMessage(mStack, buffers, m, w.getGameTime + event.getPartialTicks)

        mStack.popPose()
    }

    private def readMessage(mStack:MatrixStack, buffers:IRenderTypeBuffer, m:Message, time:Double)
    {
        var width = 0
        var height = 0
        val lines = m.msg.split("\n")
        val fr = Minecraft.getInstance.font
        for (line <- lines) {
            height += fr.lineHeight + 4
            width = Math.max(width, fr.width(line))
        }

        width += 2
        var scaling: Float = 0.02666667F
        scaling *= 0.6666667F
        val y = (m.y+0.04*Math.sin((m.x.asInstanceOf[Int]^m.z.asInstanceOf[Int])+time/4)+m.yOffset).asInstanceOf[Float]

        mStack.pushPose()
        mStack.translate(m.x + 0.5F, y, m.z + 0.5F)
        val rot = new Quaternion(Vector3f.YP, (8*Math.sin((m.x.asInstanceOf[Int]^m.z.asInstanceOf[Int])+time/6)).asInstanceOf[Float], true)
        rot.mul(Minecraft.getInstance().getEntityRenderDispatcher.cameraOrientation())
        mStack.mulPose(rot)
        mStack.scale(-scaling, -scaling, scaling)
        mStack.translate(0.0F, -10*lines.length, 0.0F)

        val bgOpacity: Float = Minecraft.getInstance.options.getBackgroundOpacity(0.25F)
        val bgColor: Int = (bgOpacity * 255.0F).toInt << 24

        var i = 0
        for (line <- lines) {
            fr.draw(mStack, line, -fr.width(line)/2, 10*i, bgColor)
            i += 1
        }
        mStack.popPose()
    }
}

abstract class MailOption
{
    def modify(mes:Message)
    {
        if (mes.msg contains tag)
        {
            change(mes)
            mes.msg = mes.msg.replace(tag, "")
        }
    }

    def change(mes:Message)

    def tag:String
}

object Replace extends MailOption
{
    override def tag = "/#f"

    override def change(mes: Message)
    {
        for (m <- Messenger.messages.clone()) if (m.location == mes.location)
        {
            Messenger.messages -= m
            return
        }
    }
}

object Combine extends MailOption
{
    override def tag = "/#c"

    override def change(mes: Message)
    {
        for (m <- Messenger.messages.clone()) if (m.location == mes.location)
        {
            Messenger.messages -= m
            mes.msg = m.msg+"\n"+mes.msg
            return
        }
    }
}

class Message
{
    def set(location:BlockPos, x: Double, y: Double, z: Double, msg: String) =
    {
        this.receivedOn = System.currentTimeMillis
        this.msg = msg
        this.location = location
        this.x = x
        this.y = y
        this.z = z
        this
    }

    def addY(y: Float) =
    {
        yOffset += y
        this
    }

    var location:BlockPos = null
    var x = 0.0D
    var y = 0.0D
    var z = 0.0D
    var msg:String = null
    var receivedOn = 0L
    var yOffset = 0F
}

