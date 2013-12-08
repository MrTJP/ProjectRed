package mrtjp.projectred.core.inventory;

import java.awt.Point;
import java.util.ArrayList;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiScreen;
import net.minecraft.util.ResourceLocation;

import org.lwjgl.input.Mouse;
import org.lwjgl.opengl.GL11;

import codechicken.core.gui.GuiDraw;

public class GhostGuiScreen extends GuiScreen implements IStackableGui, IGuiActionListener {

    protected static final ResourceLocation RL_extras = new ResourceLocation("projectred:textures/gui/guiextras.png");

    public ArrayList<GhostWidget> widgets = new ArrayList<GhostWidget>();
    GuiScreen previousGui = null;

    public int xSize, ySize, guiTop, guiLeft;

    public GhostGuiScreen() {
        this(176, 166);
    }

    public GhostGuiScreen(int xSize, int ySize) {
        super();
        this.xSize = xSize;
        this.ySize = ySize;
    }

    @Override
    public void initGui() {
        guiTop = (height - ySize) / 2;
        guiLeft = (width - xSize) / 2;
    }

    @Override
    public void keyTyped(char c, int i) {
        if (i == 1 && getPreviousScreen() != null) { // esc
            if (getPreviousScreen() instanceof IStackableGui)
                ((IStackableGui) getPreviousScreen()).prepareReDisplay();

            mc.displayGuiScreen(getPreviousScreen());
            return;
        } else {
            super.keyTyped(c, i);
            for (GhostWidget widget : widgets)
                widget.keyTyped(c, i);
        }
    }

    public void reset() {
        initGui();
        widgets.clear();
        addWidgets();
    }

    @Override
    public void setWorldAndResolution(Minecraft mc, int i, int j) {
        boolean init = this.mc == null;
        super.setWorldAndResolution(mc, i, j);
        if (init)
            addWidgets();
    }

    public void add(GhostWidget widget) {
        widgets.add(widget);
        widget.onAdded(this);
    }

    @Override
    public void drawScreen(int mousex, int mousey, float f) {
        GL11.glTranslated(guiLeft, guiTop, 0);
        drawBackground();
        for (GhostWidget widget : widgets)
            widget.drawBack(mousex - guiLeft, mousey - guiTop, f);
        drawForeground();
        for (GhostWidget widget : widgets)
            widget.drawFront(mousex - guiLeft, mousey - guiTop);
        GL11.glTranslated(-guiLeft, -guiTop, 0);
    }

    public void drawBackground() {
    }

    public void drawForeground() {
    }

    @Override
    protected void mouseClicked(int x, int y, int button) {
        super.mouseClicked(x, y, button);
        for (GhostWidget widget : widgets)
            widget.mouseClicked(x - guiLeft, y - guiTop, button);
    }

    @Override
    protected void mouseMovedOrUp(int x, int y, int button) {
        super.mouseMovedOrUp(x, y, button);
        for (GhostWidget widget : widgets)
            widget.mouseMovedOrUp(x - guiLeft, y - guiTop, button);
    }

    @Override
    protected void mouseClickMove(int x, int y, int button, long time) {
        super.mouseClickMove(x, y, button, time);
        for (GhostWidget widget : widgets)
            widget.mouseDragged(x - guiLeft, y - guiTop, button, time);
    }

    @Override
    public void updateScreen() {
        super.updateScreen();
        if (mc.currentScreen == this)
            for (GhostWidget widget : widgets)
                widget.update();
    }

    @Override
    public void handleMouseInput() {
        super.handleMouseInput();
        int i = Mouse.getEventDWheel();
        if (i != 0) {
            Point p = GuiDraw.getMousePosition();
            int scroll = i > 0 ? 1 : -1;
            for (GhostWidget widget : widgets)
                widget.mouseScrolled(p.x - guiLeft, p.y - guiTop, scroll);
        }
    }

    public void addWidgets() {
    }

    @Override
    public void actionPerformed(String actionCommand, Object... params) {
    }

    @Override
    public GuiScreen getPreviousScreen() {
        return previousGui;
    }

    @Override
    public void prepareReDisplay() {
    }

    @Override
    public boolean doesGuiPauseGame() {
        return false;
    }

}
