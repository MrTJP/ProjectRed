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

public class Messenger {
	static ArrayList<Message> messages = new ArrayList<Message>();

	/**
	 * 
	 * @param location
	 * @param mail
	 */
	public static void addMessage(Coords location, String mail) {
		boolean long_lasting = mail.startsWith("\t");
		if (long_lasting) {
			mail = mail.substring(1);
		}

		if (mail.length() == 0) {
			return;
		}

		if ((messages.size() > 10)) {
			messages.remove(0);
		}
		for (Message m : messages) {
			if (m.location.equals(location)) {
				m.set(location, mail, long_lasting);
				return;
			}
			if ((m.location.distanceManhatten(location) == 1)) {
				m.receivedOn = 0L;
			}
		}
		messages.add(new Message().set(location, mail, long_lasting));
	}

	@ForgeSubscribe
	public void renderMessages(RenderWorldLastEvent event) {
		World w = Minecraft.getMinecraft().theWorld;
		if (w == null) {
			return;
		}
		if (messages.size() == 0) {
			return;
		}
		long deathTime = System.currentTimeMillis() - 6000L;
		EntityLivingBase camera = Minecraft.getMinecraft().renderViewEntity;
		double cx = camera.lastTickPosX + (camera.posX - camera.lastTickPosX) * event.partialTicks;
		double cy = camera.lastTickPosY + (camera.posY - camera.lastTickPosY) * event.partialTicks;
		double cz = camera.lastTickPosZ + (camera.posZ - camera.lastTickPosZ) * event.partialTicks;
		GL11.glPushMatrix();
		GL11.glTranslated(-cx, -cy, -cz);
		GL11.glPushAttrib(GL11.GL_BLEND);
		
		GL11.glDisable(GL11.GL_LIGHTING);
		GL11.glDepthMask(false);
		GL11.glDisable(GL11.GL_DEPTH_TEST);
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
		
		ArrayList<Message> removeQueue = new ArrayList<Message>();
		for (Message m : messages) {
			if ((m.receivedOn < deathTime) || (m.location.w != w)) {
				removeQueue.add(m);
			} else {
				readMessage(m);
			}
		}
		messages.removeAll(removeQueue);
		
		GL11.glEnable(GL11.GL_LIGHTING);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		
		GL11.glPopMatrix();
		GL11.glPopAttrib();
	}

	private void readMessage(Message m) {
		int width = 0;
		int height = 0;
		String[] lines = m.msg.split("\n");
		FontRenderer fr = Minecraft.getMinecraft().fontRenderer;
		for (String line : lines) {
			height += fr.FONT_HEIGHT + 2;
			width = Math.max(width, fr.getStringWidth(line));
		}
		width += 2;

		float scaling = 0.02666667F;
		scaling *= 0.6666667F;
		GL11.glPushMatrix();

		float y = m.location.y + .5f;
		GL11.glTranslatef(m.location.x + 0.5F, y, m.location.z + 0.5F);
		GL11.glNormal3f(0.0F, 1.0F, 0.0F);
		GL11.glRotatef(-RenderManager.instance.playerViewY, 0.0F, 1.0F, 0.0F);
		GL11.glRotatef(RenderManager.instance.playerViewX, 1.0F, 0.0F, 0.0F);
		GL11.glScalef(-scaling, -scaling, scaling);
		GL11.glTranslatef(0.0F, -10 * lines.length, 0.0F);

		Tessellator tess = Tessellator.instance;
		int var16 = (lines.length - 1) * 10;

		GL11.glDisable(3553);
		tess.startDrawingQuads();
		int var17 = width / 2;
		tess.setColorRGBA_F(0.0F, 0.0F, 0.0F, 0.25F);
		tess.addVertex(-var17 - 1, -1.0D, 0.0D);
		tess.addVertex(-var17 - 1, 8 + var16, 0.0D);
		tess.addVertex(var17 + 1, 8 + var16, 0.0D);
		tess.addVertex(var17 + 1, -1.0D, 0.0D);
		tess.draw();
		GL11.glEnable(3553);
		int i = 0;
		for (String line : lines) {
			fr.drawString(line, -fr.getStringWidth(line) / 2, 10 * i, -1);
			i++;
		}
		GL11.glPopMatrix();
	}

	static class Message {
		Coords location;
		String msg;
		long receivedOn;

		Message set(Coords locus, String msg, boolean longLife) {
			this.receivedOn = System.currentTimeMillis();
			if (longLife) {
				this.receivedOn += 5000L;
			}
			this.location = locus;
			this.msg = msg;
			return this;
		}
	}
}