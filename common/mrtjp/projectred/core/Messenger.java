package mrtjp.projectred.core;

import java.util.ArrayList;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderManager;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.World;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.ForgeSubscribe;

import org.lwjgl.opengl.GL11;

import codechicken.core.ClientUtils;
import codechicken.lib.vec.BlockCoord;

public class Messenger
{
    static ArrayList<Message> messages = new ArrayList<Message>();

    /**
     * Adds a string to the location. To apply an option, add a "/#" + an option
     * char anywhere in the string.
     * 
     * f - Override a message already at that location. c - Combine message if
     * one already exists there.
     * 
     * @param x
     * @param y
     * @param z
     * @param mail
     */
    public static void addMessage(double x, double y, double z, String mail)
    {
        BlockCoord location = new BlockCoord((int) Math.floor(x), (int) Math.floor(y), (int) Math.floor(z));
        boolean combine = false;
        boolean override = false;

        if (mail.length() == 0)
            return;
        if (mail.contains("/#f"))
        {
            override = true;
            mail = mail.replace("/#f", "");
        }
        if (mail.contains("/#c"))
        {
            combine = true;
            mail = mail.replace("/#c", "");
        }

        if (messages.size() > 64)
            messages.remove(0);

        ArrayList<Message> readQueue = new ArrayList<Message>();
        ArrayList<Message> removeQueue = new ArrayList<Message>();
        readQueue.addAll(messages);
        float yOffset = 0;
        for (Message m : readQueue)
            if (m.location.equals(location))
                if (override)
                {
                    removeQueue.add(m);
                    break;
                }
                else if (combine)
                {
                    m.msg = m.msg + "\n" + mail;
                    m.receivedOn = System.currentTimeMillis();
                    return;
                }
        messages.removeAll(removeQueue);
        messages.add(new Message().set(location, x, y, z, mail).addY(yOffset));
    }

    @ForgeSubscribe
    public void renderMessages(RenderWorldLastEvent event)
    {
        World w = Minecraft.getMinecraft().theWorld;
        if (w == null)
            return;
        if (messages.size() == 0)
            return;
        long deathTime = System.currentTimeMillis() - 3000L;
        EntityLivingBase view = Minecraft.getMinecraft().renderViewEntity;
        double cx = view.lastTickPosX + (view.posX - view.lastTickPosX) * event.partialTicks;
        double cy = view.lastTickPosY + (view.posY - view.lastTickPosY) * event.partialTicks;
        double cz = view.lastTickPosZ + (view.posZ - view.lastTickPosZ) * event.partialTicks;
        GL11.glPushMatrix();
        GL11.glTranslated(-cx, -cy, -cz);
        GL11.glPushAttrib(GL11.GL_BLEND);

        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDepthMask(false);
        GL11.glDisable(GL11.GL_DEPTH_TEST);
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);

        ArrayList<Message> removeQueue = new ArrayList<Message>();
        ArrayList<Message> readQueue = new ArrayList<Message>();
        readQueue.addAll(messages);

        for (Message m : readQueue)
            if (m.receivedOn < deathTime)
                removeQueue.add(m);
            else
                readMessage(m);

        if (!removeQueue.isEmpty())
            messages.removeAll(removeQueue);

        GL11.glEnable(GL11.GL_LIGHTING);
        GL11.glDisable(GL11.GL_BLEND);
        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);

        GL11.glPopMatrix();
        GL11.glPopAttrib();
    }

    private void readMessage(Message m)
    {
        int width = 0;
        int height = 0;
        String[] lines = m.msg.split("\n");
        FontRenderer fr = Minecraft.getMinecraft().fontRenderer;
        for (String line : lines)
        {
            height += fr.FONT_HEIGHT + 4;
            width = Math.max(width, fr.getStringWidth(line));
        }
        width += 2;

        float scaling = 0.02666667F;
        scaling *= 0.6666667F;
        GL11.glPushMatrix();

        float y = (float) (m.y + 0.04 * Math.sin(((int) m.x ^ (int) m.z) + ClientUtils.getRenderTime() / 4) + m.yOffset);

        GL11.glTranslated(m.x + 0.5F, y, m.z + 0.5F);
        GL11.glNormal3f(0.0F, 1.0F, 0.0F);
        GL11.glRotatef((float) (-RenderManager.instance.playerViewY + 8 * Math.sin(((int) m.x ^ (int) m.z) + ClientUtils.getRenderTime() / 6)), 0.0F, 1.0F, 0.0F);
        GL11.glRotatef(RenderManager.instance.playerViewX, 1.0F, 0.0F, 0.0F);
        GL11.glScalef(-scaling, -scaling, scaling);
        GL11.glTranslatef(0.0F, -10 * lines.length, 0.0F);

        Tessellator tess = Tessellator.instance;
        int var16 = (lines.length - 1) * 10;

        GL11.glDisable(GL11.GL_TEXTURE_2D);
        tess.startDrawingQuads();
        int var17 = width / 2;
        tess.setColorRGBA_F(0.0F, 0.0F, 0.0F, 0.25F);
        tess.addVertex(-var17 - 1, -1.0D, 0.0D);
        tess.addVertex(-var17 - 1, 8 + var16, 0.0D);
        tess.addVertex(var17 + 1, 8 + var16, 0.0D);
        tess.addVertex(var17 + 1, -1.0D, 0.0D);
        tess.draw();
        GL11.glEnable(GL11.GL_TEXTURE_2D);
        int i = 0;
        for (String line : lines)
        {
            fr.drawString(line, -fr.getStringWidth(line) / 2, 10 * i, -1);
            i++;
        }
        GL11.glPopMatrix();
    }

    static class Message
    {
        BlockCoord location;
        double x;
        double y;
        double z;
        String msg;
        long receivedOn;
        float yOffset = 0;

        Message set(BlockCoord location, double x, double y, double z, String msg)
        {
            this.receivedOn = System.currentTimeMillis();
            this.msg = msg;
            this.location = location;
            this.x = x;
            this.y = y;
            this.z = z;
            return this;
        }

        Message addY(float y)
        {
            yOffset += y;
            return this;
        }
    }
}