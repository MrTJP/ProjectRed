package mrtjp.projectred.core;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.entity.RenderItem;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.inventory.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.client.ForgeHooksClient;

import org.lwjgl.opengl.GL11;

import codechicken.core.IGuiPacketSender;

public class BasicGuiUtils {

	public static String getCuttedString(String input, int maxLength, FontRenderer renderer) {
		if (renderer.getStringWidth(input) < maxLength) {
			return input;
		}
		input += "...";
		while (renderer.getStringWidth(input) > maxLength && input.length() > 0) {
			input = input.substring(0, input.length() - 4) + "...";
		}
		return input;
	}

	/**
	 * Draws the specified string with a shadow.
	 * 
	 * @author RS485
	 * 
	 * @throws SecurityException
	 * @throws NoSuchMethodException
	 * @throws InvocationTargetException
	 * @throws IllegalArgumentException
	 * @throws IllegalAccessException
	 * @throws NoSuchFieldException
	 */
	private static int drawStringWithShadow(FontRenderer fontRenderer, String par1Str, int par2, int par3, int par4) throws Exception {
		Method a = getObfuMethod(fontRenderer.getClass(), "c", "resetStyles");
		a.setAccessible(true);
		a.invoke(fontRenderer);

		Field b = getObfuField(fontRenderer.getClass(), "m", "bidiFlag");
		b.setAccessible(true);
		if (((Boolean) b.get(fontRenderer)).booleanValue()) {
			Method c = getObfuMethod(fontRenderer.getClass(), "c", "bidiReorder", String.class);
			c.setAccessible(true);
			par1Str = (String) c.invoke(fontRenderer, par1Str);
		}
		Method d = getObfuMethod(fontRenderer.getClass(), "b", "renderString", String.class, int.class, int.class, int.class, boolean.class);
		d.setAccessible(true);
		int var5 = ((Integer) d.invoke(fontRenderer, par1Str, par2 + 1, par3 + 1, par4, true)).intValue();

		GL11.glTranslated(0.0D, 0.0D, 1.0D);
		var5 = Math.max(var5, ((Integer) d.invoke(fontRenderer, par1Str, par2, par3, par4, false)).intValue());
		GL11.glTranslated(0.0D, 0.0D, -1.0D);

		return var5;
	}

	private static Field getObfuField(Class<?> clazz, String name1, String name2) throws SecurityException, NoSuchFieldException {
		try {
			return clazz.getDeclaredField(name1);
		} catch (Exception e) {
			return clazz.getDeclaredField(name2);
		}
	}

	private static Method getObfuMethod(Class<?> clazz, String name1, String name2, Class<?>... objects) throws NoSuchMethodException, SecurityException {
		try {
			return clazz.getDeclaredMethod(name1, objects);
		} catch (Exception e) {
			return clazz.getDeclaredMethod(name2, objects);
		}
	}

	private static float zLevel;

	private static void drawGradientRect(int par1, int par2, int par3, int par4, int par5, int par6) {
		float var7 = (par5 >> 24 & 255) / 255.0F;
		float var8 = (par5 >> 16 & 255) / 255.0F;
		float var9 = (par5 >> 8 & 255) / 255.0F;
		float var10 = (par5 & 255) / 255.0F;
		float var11 = (par6 >> 24 & 255) / 255.0F;
		float var12 = (par6 >> 16 & 255) / 255.0F;
		float var13 = (par6 >> 8 & 255) / 255.0F;
		float var14 = (par6 & 255) / 255.0F;
		GL11.glDisable(GL11.GL_TEXTURE_2D);
		GL11.glEnable(GL11.GL_BLEND);
		GL11.glDisable(GL11.GL_ALPHA_TEST);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
		GL11.glShadeModel(GL11.GL_SMOOTH);
		Tessellator var15 = Tessellator.instance;
		var15.startDrawingQuads();
		var15.setColorRGBA_F(var8, var9, var10, var7);
		var15.addVertex(par3, par2, zLevel);
		var15.addVertex(par1, par2, zLevel);
		var15.setColorRGBA_F(var12, var13, var14, var11);
		var15.addVertex(par1, par4, zLevel);
		var15.addVertex(par3, par4, zLevel);
		var15.draw();
		GL11.glShadeModel(GL11.GL_FLAT);
		GL11.glDisable(GL11.GL_BLEND);
		GL11.glEnable(GL11.GL_ALPHA_TEST);
		GL11.glEnable(GL11.GL_TEXTURE_2D);
	}

	/**
	 * Draws a textured rectangle at the stored z-value. Args: x, y, u, v,
	 * width, height
	 */
	public static void drawTexturedModalRect(double par1, double par2, double par3, double par4, double par5, double par6, float zLevel) {
		float var7 = 0.00390625F;
		float var8 = 0.00390625F;
		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(par1 + 0, par2 + par6, zLevel, (float) (par3 + 0) * var7, (float) (par4 + par6) * var8);
		var9.addVertexWithUV(par1 + par5, par2 + par6, zLevel, (float) (par3 + par5) * var7, (float) (par4 + par6) * var8);
		var9.addVertexWithUV(par1 + par5, par2 + 0, zLevel, (float) (par3 + par5) * var7, (float) (par4 + 0) * var8);
		var9.addVertexWithUV(par1 + 0, par2 + 0, zLevel, (float) (par3 + 0) * var7, (float) (par4 + 0) * var8);
		var9.draw();
	}

	/**
	 * Renders the specified text to the screen, center-aligned.
	 */
	public static void drawCenteredString(FontRenderer par1FontRenderer, String par2Str, int par3, int par4, int par5) {
		par1FontRenderer.drawStringWithShadow(par2Str, par3 - par1FontRenderer.getStringWidth(par2Str) / 2, par4, par5);
	}

	public static void drawPlayerInventoryBackground(Minecraft mc, int xOffset, int yOffset) {
		// Player "backpack"
		for (int row = 0; row < 3; row++) {
			for (int column = 0; column < 9; column++) {
				drawSlotBackground(mc, xOffset + column * 18 - 1, yOffset + row * 18 - 1);
			}
		}
		// Player "hotbar"
		for (int i1 = 0; i1 < 9; i1++) {
			drawSlotBackground(mc, xOffset + i1 * 18 - 1, yOffset + 58 - 1);
		}
	}

	public static void drawPlayerHotbarBackground(Minecraft mc, int xOffset, int yOffset) {
		// Player "hotbar"
		for (int i1 = 0; i1 < 9; i1++) {
			drawSlotBackground(mc, xOffset + i1 * 18 - 1, yOffset - 1);
		}
	}

	public static void drawSlotBackground(Minecraft mc, int x, int y) {
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/slot.png"));

		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(x, y + 18, zLevel, 0, 1);
		var9.addVertexWithUV(x + 18, y + 18, zLevel, 1, 1);
		var9.addVertexWithUV(x + 18, y, zLevel, 1, 0);
		var9.addVertexWithUV(x, y, zLevel, 0, 0);
		var9.draw();
	}

	public static void drawBigSlotBackground(Minecraft mc, int x, int y) {
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		mc.renderEngine.func_110577_a(new ResourceLocation("/projectred", "textures/gui/slot-big.png"));

		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(x, y + 26, zLevel, 0, 1);
		var9.addVertexWithUV(x + 26, y + 26, zLevel, 1, 1);
		var9.addVertexWithUV(x + 26, y, zLevel, 1, 0);
		var9.addVertexWithUV(x, y, zLevel, 0, 0);
		var9.draw();
	}

	public static void drawSmallSlotBackground(Minecraft mc, int x, int y) {
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/slot-small.png"));

		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(x, y + 8, zLevel, 0, 1);
		var9.addVertexWithUV(x + 8, y + 8, zLevel, 1, 1);
		var9.addVertexWithUV(x + 8, y, zLevel, 1, 0);
		var9.addVertexWithUV(x, y, zLevel, 0, 0);
		var9.draw();
	}

	public static void renderIconAt(Minecraft mc, int x, int y, float zLevel, Icon icon) {
		GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		mc.renderEngine.func_110577_a(new ResourceLocation("/gui/items.png"));

		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(x, y + 16, zLevel, icon.getMinU(), icon.getMaxV());
		var9.addVertexWithUV(x + 16, y + 16, zLevel, icon.getMaxU(), icon.getMaxV());
		var9.addVertexWithUV(x + 16, y, zLevel, icon.getMaxU(), icon.getMinV());
		var9.addVertexWithUV(x, y, zLevel, icon.getMinU(), icon.getMinV());
		var9.draw();
	}

	public static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag) {
		drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, flag, true, true, true, true);
	}

	public static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag, boolean displayTop, boolean displayLeft, boolean displayBottom, boolean displayRight) {
		if (flag) {
			GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
		}
		mc.renderEngine.func_110577_a(new ResourceLocation("projectred", "textures/gui/GuiBackground.png"));

		if (displayTop) {
			// Top Side
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
			var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
			var9.addVertexWithUV(right - 15, guiTop, zLevel, 0.66, 0);
			var9.addVertexWithUV(guiLeft + 15, guiTop, zLevel, 0.33, 0);
			var9.draw();
		}

		if (displayLeft) {
			// Left Side
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(guiLeft, bottom - 15, zLevel, 0, 0.66);
			var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
			var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
			var9.addVertexWithUV(guiLeft, guiTop + 15, zLevel, 0, 0.33);
			var9.draw();
		}

		if (displayBottom) {
			// Bottom Side
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(guiLeft + 15, bottom, zLevel, 0.33, 1);
			var9.addVertexWithUV(right - 15, bottom, zLevel, 0.66, 1);
			var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
			var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
			var9.draw();
		}

		if (displayRight) {
			// Right Side
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
			var9.addVertexWithUV(right, bottom - 15, zLevel, 1, 0.66);
			var9.addVertexWithUV(right, guiTop + 15, zLevel, 1, 0.33);
			var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
			var9.draw();
		}

		if (displayTop && displayLeft) {
			// Top Left
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(guiLeft, guiTop + 15, zLevel, 0, 0.33);
			var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
			var9.addVertexWithUV(guiLeft + 15, guiTop, zLevel, 0.33, 0);
			var9.addVertexWithUV(guiLeft, guiTop, zLevel, 0, 0);
			var9.draw();
		}

		if (displayBottom && displayLeft) {
			// Bottom Left
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(guiLeft, bottom, zLevel, 0, 1);
			var9.addVertexWithUV(guiLeft + 15, bottom, zLevel, 0.33, 1);
			var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
			var9.addVertexWithUV(guiLeft, bottom - 15, zLevel, 0, 0.66);
			var9.draw();
		}

		if (displayBottom && displayRight) {
			// Bottom Right
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(right - 15, bottom, zLevel, 0.66, 1);
			var9.addVertexWithUV(right, bottom, zLevel, 1, 1);
			var9.addVertexWithUV(right, bottom - 15, zLevel, 1, 0.66);
			var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
			var9.draw();
		}

		if (displayTop && displayRight) {
			// Top Right
			Tessellator var9 = Tessellator.instance;
			var9.startDrawingQuads();
			var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
			var9.addVertexWithUV(right, guiTop + 15, zLevel, 1, 0.33);
			var9.addVertexWithUV(right, guiTop, zLevel, 1, 0);
			var9.addVertexWithUV(right - 15, guiTop, zLevel, 0.66, 0);
			var9.draw();
		}

		// Center
		Tessellator var9 = Tessellator.instance;
		var9.startDrawingQuads();
		var9.addVertexWithUV(guiLeft + 15, bottom - 15, zLevel, 0.33, 0.66);
		var9.addVertexWithUV(right - 15, bottom - 15, zLevel, 0.66, 0.66);
		var9.addVertexWithUV(right - 15, guiTop + 15, zLevel, 0.66, 0.33);
		var9.addVertexWithUV(guiLeft + 15, guiTop + 15, zLevel, 0.33, 0.33);
		var9.draw();
	}

	/**
	 * Draws a solid color rectangle with the specified coordinates and color.
	 */
	public static void drawRect(int par0, int par1, int par2, int par3, int par4) {
		int var5;

		if (par0 < par2) {
			var5 = par0;
			par0 = par2;
			par2 = var5;
		}

		if (par1 < par3) {
			var5 = par1;
			par1 = par3;
			par3 = var5;
		}

		float var10 = (par4 >> 24 & 255) / 255.0F;
		float var6 = (par4 >> 16 & 255) / 255.0F;
		float var7 = (par4 >> 8 & 255) / 255.0F;
		float var8 = (par4 & 255) / 255.0F;
		Tessellator var9 = Tessellator.instance;
		// GL11.glEnable(GL11.GL_BLEND);
		GL11.glDisable(GL11.GL_TEXTURE_2D);
		GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
		GL11.glColor4f(var6, var7, var8, var10);
		var9.startDrawingQuads();
		var9.addVertex(par0, par3, 0.0D);
		var9.addVertex(par2, par3, 0.0D);
		var9.addVertex(par2, par1, 0.0D);
		var9.addVertex(par0, par1, 0.0D);
		var9.draw();
		GL11.glEnable(GL11.GL_TEXTURE_2D);
		// GL11.glDisable(GL11.GL_BLEND);
	}

	public static void renderItemOnGui(GuiItemRenderOptions ren) {
		// Push render matrix
		GL11.glPushMatrix();
		int ppi = 0;
		int column = 0;
		int row = 0;
		FontRenderer fontRenderer = ren.mc.fontRenderer;
		RenderItem renderItem = new RenderItem();
		RenderBlocks renderBlocks = new RenderBlocks();
		renderItem.renderWithColor = ren.opacity == 1f;

		// Render the itemstack
		if (ren._stack != null) {
			if (ren.pulsate) {
				renderItem.renderWithColor = false;
				GL11.glEnable(GL11.GL_BLEND);
				float op = ((float) Math.sin(ProjectRedTickHandler.instance.radianRotation) + 1f);
				op = Math.min(op, ren.maxPulse);
				op = Math.max(op, ren.minPulse);
				GL11.glColor4f(1f, 1f, 1f, op);
			} else {
				GL11.glEnable(GL11.GL_BLEND);
				GL11.glColor4f(1f, 1f, 1f, ren.opacity);
			}
			if (ren.disableEffect) {
				if (!ForgeHooksClient.renderInventoryItem(renderBlocks, ren.mc.renderEngine, ren._stack, renderItem.renderWithColor, renderItem.zLevel, ren.x, ren.y)) {
					renderItem.renderItemIntoGUI(fontRenderer, ren.mc.renderEngine, ren._stack, ren.x, ren.y);
				}
			} else if (!ren.disableEffect) {
				GL11.glTranslated(0, 0, 3.0);
				renderItem.renderItemAndEffectIntoGUI(fontRenderer, ren.mc.renderEngine, ren._stack, ren.x, ren.y);
				GL11.glTranslated(0, 0, -3.0);
			}
		}
		GL11.glEnable(GL11.GL_LIGHTING);

		// Number Drawing
		if (ren.renderCount) {
			String s;
			if (ren._stack.stackSize == 1 && !ren.renderZeroCount) {
				s = "";
			} else if (ren._stack.stackSize < 1000) {
				s = ren._stack.stackSize + "";
			} else if (ren._stack.stackSize < 100000) {
				s = ren._stack.stackSize / 1000 + "K";
			} else if (ren._stack.stackSize < 1000000) {
				s = "0M" + ren._stack.stackSize / 100000;
			} else {
				s = ren._stack.stackSize / 1000000 + "M";
			}

			GL11.glDisable(GL11.GL_LIGHTING);
			GL11.glTranslated(0.0D, 0.0D, 100.0D);
			try {
				drawStringWithShadow(fontRenderer, s, ren.x + 16 - fontRenderer.getStringWidth(s), ren.y + 8, 0xFFFFFF);
				GL11.glTranslated(0.0D, 0.0D, -100.0D);
			} catch (Exception e) {
				GL11.glTranslated(0.0D, 0.0D, -100.0D);
				GL11.glDisable(GL11.GL_DEPTH_TEST);
				fontRenderer.drawStringWithShadow(s, ren.x + 16 - fontRenderer.getStringWidth(s), ren.y + 8, 0xFFFFFF);
				GL11.glEnable(GL11.GL_DEPTH_TEST);
			}
			GL11.glEnable(GL11.GL_LIGHTING);
		}

		// Pop render matrix
		GL11.glDisable(GL11.GL_LIGHTING);
		GL11.glPopMatrix();
	}

	public static class GuiItemRenderOptions {
		public final ItemStack _stack;
		public int x, y;
		private Minecraft mc;
		public float opacity;
		public boolean renderCount;
		public boolean renderZeroCount;
		public boolean disableEffect;
		public boolean pulsate;
		public float maxPulse;
		public float minPulse;

		public GuiItemRenderOptions(ItemStack stack) {
			_stack = stack;
			int x = y = 0;
			mc = Minecraft.getMinecraft();
			opacity = 1f;
			renderCount = false;
			renderZeroCount = false;
			disableEffect = false;
			pulsate = false;
			maxPulse = minPulse = 1f;
		}

		public GuiItemRenderOptions setPos(int xPos, int yPos) {
			x = xPos;
			y = yPos;
			return this;
		}

		public GuiItemRenderOptions setOpacity(float op) {
			opacity = op;
			return this;
		}

		public GuiItemRenderOptions setRenderCount() {
			renderCount = true;
			return this;
		}

		public GuiItemRenderOptions setRenderZeroCount() {
			renderCount = renderZeroCount = true;
			return this;
		}

		public GuiItemRenderOptions setDisableEffect() {
			disableEffect = true;
			return this;
		}

		public GuiItemRenderOptions setPulsate(float min, float max) {
			opacity = 0f;
			pulsate = true;
			maxPulse = max;
			minPulse = min;
			return this;
		}
	}

	/**
	 * Called on the server to open a container.
	 * 
	 * @author Chickenbones
	 * @param player
	 * @param container
	 * @param packetSender
	 */
	public static void openSMPContainer(EntityPlayerMP player, Container container, IGuiPacketSender packetSender) {
		player.incrementWindowID();
		player.closeContainer();
		packetSender.sendPacket(player, player.currentWindowId);
		player.openContainer = container;
		player.openContainer.windowId = player.currentWindowId;
		player.openContainer.addCraftingToCrafters(player);
	}

	/**
	 * Called on client through packet after a container is opened on the server.
	 * @param windowId
	 * @param gui
	 */
	public static void openSMPGui(int windowId, GuiScreen gui) {
		Minecraft.getMinecraft().displayGuiScreen(gui);
		if (windowId != 0) {
			Minecraft.getMinecraft().thePlayer.openContainer.windowId = windowId;
		}
	}

}
