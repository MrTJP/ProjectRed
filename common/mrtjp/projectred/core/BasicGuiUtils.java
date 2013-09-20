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

import codechicken.core.ClientUtils;
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

    private static float zLevel;
    
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
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/slot.png"));

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
        mc.renderEngine.bindTexture(new ResourceLocation("/projectred", "textures/gui/slot-big.png"));

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
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/slot-small.png"));

        Tessellator var9 = Tessellator.instance;
        var9.startDrawingQuads();
        var9.addVertexWithUV(x, y + 8, zLevel, 0, 1);
        var9.addVertexWithUV(x + 8, y + 8, zLevel, 1, 1);
        var9.addVertexWithUV(x + 8, y, zLevel, 1, 0);
        var9.addVertexWithUV(x, y, zLevel, 0, 0);
        var9.draw();
    }

    public static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag) {
        drawGuiBackGround(mc, guiLeft, guiTop, right, bottom, zLevel, flag, true, true, true, true);
    }

    public static void drawGuiBackGround(Minecraft mc, int guiLeft, int guiTop, int right, int bottom, float zLevel, boolean flag, boolean displayTop, boolean displayLeft, boolean displayBottom, boolean displayRight) {
        if (flag)
            GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        mc.renderEngine.bindTexture(new ResourceLocation("projectred", "textures/gui/GuiBackground.png"));

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
                float op = ((float) Math.sin(ClientUtils.getRenderTime() * .25f));
                op = Math.min(op, ren.maxPulse);
                op = Math.max(op, ren.minPulse);
                GL11.glColor4f(1f, 1f, 1f, op);
            } else {
                GL11.glEnable(GL11.GL_BLEND);
                GL11.glColor4f(1f, 1f, 1f, ren.opacity);
            }
            if (ren.disableEffect) {
                if (!ForgeHooksClient.renderInventoryItem(renderBlocks, ren.mc.renderEngine, ren._stack, renderItem.renderWithColor, renderItem.zLevel, ren.x, ren.y))
                    renderItem.renderItemIntoGUI(fontRenderer, ren.mc.renderEngine, ren._stack, ren.x, ren.y);

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
            if (ren._stack.stackSize == 1 && !ren.renderZeroCount)
                s = "";
            else if (ren._stack.stackSize < 1000)
                s = ren._stack.stackSize + "";
            else if (ren._stack.stackSize < 100000)
                s = ren._stack.stackSize / 1000 + "K";
            else if (ren._stack.stackSize < 1000000)
                s = "0M" + ren._stack.stackSize / 100000;
            else
                s = ren._stack.stackSize / 1000000 + "M";

            GL11.glDisable(GL11.GL_LIGHTING);
            GL11.glTranslated(0.0D, 0.0D, 100.0D);
            try {
                fontRenderer.drawStringWithShadow(s, ren.x+16 - fontRenderer.getStringWidth(s), ren.y, 0xFFFFFF);
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
        if (windowId != 0)
            Minecraft.getMinecraft().thePlayer.openContainer.windowId = windowId;
    }

}
