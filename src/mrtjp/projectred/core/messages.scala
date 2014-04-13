package mrtjp.projectred.core

import codechicken.core.ClientUtils
import codechicken.lib.vec.BlockCoord
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.Tessellator
import net.minecraft.client.renderer.entity.RenderManager
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.event.ForgeSubscribe
import org.lwjgl.opengl.GL11
import scala.collection.mutable

object Messenger
{
    val messages = mutable.ListBuffer[Message]()
    val options = Seq[MailOption](Replace, Combine)

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
        val location = new BlockCoord(Math.floor(x).asInstanceOf[Int], Math.floor(y).asInstanceOf[Int], Math.floor(z).asInstanceOf[Int])

        val mess = new Message().set(location, x, y, z, mail)

        options.foreach(op => op.modify(mess))

        if (messages.size > 64) messages.remove(0)

        messages += mess
    }

    @ForgeSubscribe
    def renderMessages(event: RenderWorldLastEvent)
    {
        val w = Minecraft.getMinecraft.theWorld
        if (w == null) return
        if (Messenger.messages.size == 0) return

        val deathTime = System.currentTimeMillis-3000L

        val view = Minecraft.getMinecraft.renderViewEntity
        val cx = view.lastTickPosX+(view.posX-view.lastTickPosX)*event.partialTicks
        val cy = view.lastTickPosY+(view.posY-view.lastTickPosY)*event.partialTicks
        val cz = view.lastTickPosZ+(view.posZ-view.lastTickPosZ)*event.partialTicks

        GL11.glPushMatrix()
        GL11.glTranslated(-cx, -cy, -cz)
        GL11.glPushAttrib(GL11.GL_BLEND)
        GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glDepthMask(false)
        GL11.glDisable(GL11.GL_DEPTH_TEST)
        GL11.glEnable(GL11.GL_BLEND)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

        for (m <- Messenger.messages.clone())
            if (m == null || m.receivedOn < deathTime) Messenger.messages -= m
            else readMessage(m)

        GL11.glEnable(GL11.GL_LIGHTING)
        GL11.glDisable(GL11.GL_BLEND)
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F)
        GL11.glPopMatrix()
        GL11.glPopAttrib()
    }

    private def readMessage(m:Message)
    {
        var width = 0
        var height = 0
        val lines = m.msg.split("\n")
        val fr = Minecraft.getMinecraft.fontRenderer
        for (line <- lines)
        {
            height += fr.FONT_HEIGHT + 4
            width = Math.max(width, fr.getStringWidth(line))
        }

        width += 2
        var scaling: Float = 0.02666667F
        scaling *= 0.6666667F
        val y = (m.y+0.04*Math.sin((m.x.asInstanceOf[Int]^m.z.asInstanceOf[Int])+ClientUtils.getRenderTime/4)+m.yOffset).asInstanceOf[Float]

        GL11.glPushMatrix()
        GL11.glTranslated(m.x + 0.5F, y, m.z + 0.5F)
        GL11.glNormal3f(0.0F, 1.0F, 0.0F)
        GL11.glRotatef((-RenderManager.instance.playerViewY+8*Math.sin((m.x.asInstanceOf[Int]^m.z.asInstanceOf[Int])+ClientUtils.getRenderTime/6)).asInstanceOf[Float], 0.0F, 1.0F, 0.0F)
        GL11.glRotatef(RenderManager.instance.playerViewX, 1.0F, 0.0F, 0.0F)
        GL11.glScalef(-scaling, -scaling, scaling)
        GL11.glTranslatef(0.0F, -10*lines.length, 0.0F)

        val tess = Tessellator.instance
        val var16 = (lines.length-1)*10
        val var17 = width/2

        GL11.glDisable(GL11.GL_TEXTURE_2D)
        tess.startDrawingQuads()
        tess.setColorRGBA_F(0.0F, 0.0F, 0.0F, 0.25F)
        tess.addVertex(-var17-1, -1.0D, 0.0D)
        tess.addVertex(-var17-1, 8+var16, 0.0D)
        tess.addVertex(var17+1, 8+var16, 0.0D)
        tess.addVertex(var17+1, -1.0D, 0.0D)
        tess.draw

        GL11.glEnable(GL11.GL_TEXTURE_2D)
        var i = 0
        for (line <- lines)
        {
            fr.drawString(line, -fr.getStringWidth(line)/2, 10*i, -1)
            i += 1
        }
        GL11.glPopMatrix()
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
    def set(location: BlockCoord, x: Double, y: Double, z: Double, msg: String) =
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

    var location:BlockCoord = null
    var x = 0.0D
    var y = 0.0D
    var z = 0.0D
    var msg:String = null
    var receivedOn = 0L
    var yOffset = 0F
}