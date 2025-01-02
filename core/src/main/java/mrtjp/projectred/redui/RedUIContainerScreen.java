package mrtjp.projectred.redui;

import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.lib.Size;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.AbstractContainerMenu;

import java.util.LinkedList;
import java.util.List;

public class RedUIContainerScreen<T extends AbstractContainerMenu> extends AbstractContainerScreen<T> implements RedUIRootNode {

    private final List<RedUINode> children = new LinkedList<>();

    // Frame of the GUI window
    private Rect frame;
    // Frame of the full Minecraft window (positioned at 0, 0)
    private Rect screenFrame;
    // Absolute z position of this root node
    private double zPos = 0;

    private long lastClickTime = 0;

    public RedUIContainerScreen(int backgroundWidth, int backgroundHeight, T container, Inventory playerInventory, Component title) {
        super(container, playerInventory, title);
        this.imageWidth = backgroundWidth;
        this.imageHeight = backgroundHeight;

        // These frames are fully set during init() call, when the bounds of the entire screen are known
        this.frame = new Rect(Point.ZERO, new Size(backgroundWidth, backgroundHeight));
        this.screenFrame = Rect.ZERO;
    }

    @Override
    protected void init() {
        super.init();

        screenFrame = new Rect(Point.ZERO, new Size(width, height));

        Point framePos = new Point((screenFrame.width() - frame.width()) / 2, (screenFrame.height() - frame.height()) / 2);
        frame = new Rect(framePos, frame.size);
    }

    @Override
    public void render(GuiGraphics graphics, int mouseX, int mouseY, float partialFrame) {
        // Call frame update function on all nodes
        Point mousePoint = new Point(mouseX, mouseY);
        operateOnSubtree(mousePoint, (n, p, c) -> {
            n.frameUpdate(p, partialFrame);
            return false;
        }, false);

        // Render semi-transparent grey background
        renderBackground(graphics);

        // Super render call, which does the following:
        //  - Call renderBg(), which is typically overridden to draw main background
        //  - Fire ContainerScreenEvent.Render.Background client event
        //  - Disable depth test
        //  - Render each widget in renderables list (call Screen#render())
        //  - Render each slot and its highlight (via renderSlot() and renderSlotHighlight())
        //  - Render labels (renderLabels())
        //  - Fire ContainerScreenEvent.Render.Foreground client event
        //  - Render picked up item on cursor (via renderFloatingItem())
        //  - Render snapback items (via renderFloatingItem())
        //  - Enable depth test
        super.render(graphics, mouseX, mouseY, partialFrame);

        // Render tooltips
        renderTooltip(graphics, mouseX, mouseY);

        // Render foreground
        drawFrontForSubtree(graphics, new Point(mouseX, mouseY), partialFrame);
    }

    @Override
    protected void renderBg(GuiGraphics graphics, float partialFrame, int mouseX, int mouseY) {
        // Draw back of all nodes in RedUI tree (this class is expected to provide the main background via RedUI's render call)
        RenderSystem.enableDepthTest(); // Nodes render out of order, so depth test is needed
        drawBackForSubtree(graphics, new Point(mouseX, mouseY), partialFrame);
    }

    @Override
    protected void containerTick() {
        super.containerTick();
        getSubTree(n -> true).forEach(RedUINode::update);
    }

    @Override
    public boolean isPauseScreen() {
        return false;
    }

    @Override
    public boolean mouseClicked(double x, double y, int glfwMouseButton) {
        lastClickTime = System.currentTimeMillis();
//        boolean consumed = super.mouseClicked(x, y, glfwMouseButton); //TODO Check consumption somehow. This always returns true
        super.mouseClicked(x, y, glfwMouseButton);
        boolean consumed = false;
        return operateOnZOrderedSubtree(new Point((int) x, (int) y), (n, p, c) -> n.mouseClicked(p, glfwMouseButton, c), consumed);
    }

    @Override
    public boolean mouseReleased(double x, double y, int button) {
        boolean consumed = super.mouseReleased(x, y, button);
        long timeSinceLastClick = System.currentTimeMillis() - lastClickTime;
        return operateOnZOrderedSubtree(new Point((int) x, (int) y), (n, p, c) -> n.mouseReleased(p, button, timeSinceLastClick, c), consumed);
    }

    @Override
    public boolean mouseDragged(double x, double y, int button, double dragX, double dragY) {
        boolean consumed = super.mouseDragged(x, y, button, dragX, dragY);
        long timeSinceLastClick = System.currentTimeMillis() - lastClickTime;
        return operateOnZOrderedSubtree(new Point((int) x, (int) y), (n, p, c) -> n.mouseDragged(p, button, timeSinceLastClick, c), consumed);
    }

    @Override
    public boolean mouseScrolled(double x, double y, double scroll) {
        boolean consumed = super.mouseScrolled(x, y, scroll);
        return operateOnZOrderedSubtree(new Point((int) x, (int) y), (n, p, c) -> n.mouseScrolled(p, scroll, c), consumed);
    }

    @Override
    public boolean charTyped(char keyPressed, int glfwFlags) {
        boolean consumed = super.charTyped(keyPressed, glfwFlags);
        return operateOnZOrderedSubtree(Point.ZERO, (n, p, c) -> n.onCharTyped(keyPressed, glfwFlags, c), consumed);
    }

    @Override
    public boolean keyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags) {
        boolean consumed = super.keyPressed(glfwKeyCode, glfwScanCode, glfwFlags);
        return operateOnZOrderedSubtree(Point.ZERO, (n, p, c) -> n.onKeyPressed(glfwKeyCode, glfwScanCode, glfwFlags, c), consumed);
    }

    @Override
    public boolean keyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags) {
        boolean consumed = super.keyReleased(glfwKeyCode, glfwScanCode, glfwFlags);
        return operateOnZOrderedSubtree(Point.ZERO, (n, p, c) -> n.onKeyReleased(glfwKeyCode, glfwScanCode, glfwFlags, c), consumed);
    }

    //region RedUINode overrides

    @Override
    public Rect getScreenFrame() {
        return screenFrame;
    }

    @Override
    public List<RedUINode> getOurChildren() {
        return children;
    }

    @Override
    public Rect getFrame() {
        return frame;
    }

    @Override
    public Point getPosition() {
        return frame.origin;
    }

    @Override
    public double getZPosition() {
        return zPos;
    }

    @Override
    public void setZPosition(double zPosition) {
        this.zPos = zPosition;
    }

    @Override
    public boolean isHidden() {
        return false;
    }

    @Override
    public Font getFontRenderer() {
        return font;
    }

    //endregion
}
