package mrtjp.projectred.core.inventory;

import java.util.LinkedList;
import java.util.List;

import net.minecraft.client.Minecraft;

import org.lwjgl.opengl.GL11;

import codechicken.core.gui.GuiDraw;
import codechicken.lib.render.CCRenderState;

public abstract class WidgetButton extends GhostWidget
{
    public String actionCommand;
    public String text = "";

    public WidgetButton(int x, int y, int l, int w)
    {
        super(x, y, l, w);
    }

    @Override
    public void mouseClicked(int x, int y, int button)
    {
        if (pointInside(x, y) && actionCommand != null)
        {
            sendAction(actionCommand, button);
            Minecraft.getMinecraft().sndManager.playSoundFX("random.click", 1, 1);
        }
    }

    @Override
    public void drawBack(int mousex, int mousey, float frame)
    {
        GL11.glColor4f(1, 1, 1, 1);

        boolean mouseover = pointInside(mousex, mousey);

        drawButtonBackground(mousex, mousey, frame);
        drawButton(mouseover);
    }

    @Override
    public void drawFront(int mousex, int mousey)
    {
        boolean mouseover = pointInside(mousex, mousey);
        if (mouseover)
            GuiDraw.drawMultilineTip(mousex + 12, mousey - 12, getOverlayText());
    }

    public void drawButtonBackground(int mousex, int mousey, float frame)
    {
        renderEngine.bindTexture(guiTex);
        GL11.glColor4f(1, 1, 1, 1);
        boolean mouseover = pointInside(mousex, mousey);
        int state = mouseover ? 2 : 1;
        drawTexturedModalRect(x, y, 0, 46 + state * 20, width / 2, height / 2);// top
                                                                               // left
        drawTexturedModalRect(x + width / 2, y, 200 - width / 2, 46 + state * 20, width / 2, height / 2);// top
                                                                                                         // right
        drawTexturedModalRect(x, y + height / 2, 0, 46 + state * 20 + 20 - height / 2, width / 2, height / 2);// bottom
                                                                                                              // left
        drawTexturedModalRect(x + width / 2, y + height / 2, 200 - width / 2, 46 + state * 20 + 20 - height / 2, width / 2, height / 2);// bottom
                                                                                                                                        // right
    }

    public void drawButton(boolean mouseover)
    {
    }

    public List<String> getOverlayText()
    {
        return new LinkedList<String>();
    }

    public WidgetButton setActionCommand(String string)
    {
        actionCommand = string;
        return this;
    }

    public WidgetButton setText(String text)
    {
        this.text = text;
        return this;
    }

    public String getText()
    {
        return text;
    }

    public static class WidgetSimpleButton extends WidgetButton
    {
        public WidgetSimpleButton(int x, int y, int l, int w)
        {
            super(x, y, l, w);
        }

        @Override
        public void drawButton(boolean mouseover)
        {
            drawCenteredString(fontRenderer, getText(), x + width / 2, y + (height - 8) / 2, mouseover ? 0xFFFFFFA0 : 0xFFE0E0E0);
        }

        @Override
        public List<String> getOverlayText()
        {
            return new LinkedList<String>();
        }
    }

    public static class WidgetDotSelector extends WidgetButton
    {
        public WidgetDotSelector(int x, int y)
        {
            super(x - 4, y - 4, 8, 8);
        }

        @Override
        public void drawButtonBackground(int mousex, int mousey, float f)
        {
            CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");

            GL11.glColor4f(1, 1, 1, 1);
            boolean mouseover = pointInside(mousex, mousey);
            int u = mouseover ? 11 : 1;
            drawTexturedModalRect(x, y, u, 1, 8, 8);
        }
    }

    public static class WidgetCheckBox extends WidgetButton
    {
        private boolean checked;

        public WidgetCheckBox(int x, int y, boolean state)
        {
            super(x - 7, y - 7, 14, 14);
            checked = state;
        }

        public boolean isChecked()
        {
            return checked;
        }

        @Override
        public void mouseClicked(int x, int y, int button)
        {
            if (pointInside(x, y))
            {
                if (actionCommand != null)
                    sendAction(actionCommand, button);

                Minecraft.getMinecraft().sndManager.playSoundFX("random.click", 1, 1);
                boolean old = checked;
                checked = !checked;
                onStateChanged(old);
            }
        }

        @Override
        public void drawButton(boolean mouseover)
        {
            CCRenderState.changeTexture("projectred:textures/gui/guiextras.png");
            int u = checked ? 17 : 1;
            drawTexturedModalRect(x, y, u, 134, 14, 14);
        }

        public WidgetCheckBox setCheck(boolean flag)
        {
            boolean old = checked;
            checked = flag;
            if (old != checked)
                onStateChanged(old);
            return this;
        }

        public void onStateChanged(boolean oldState)
        {
        }
    }
}
