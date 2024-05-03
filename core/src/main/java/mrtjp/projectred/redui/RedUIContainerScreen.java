package mrtjp.projectred.redui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.lib.Size;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.entity.ItemRenderer;
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
    public void render(PoseStack stack, int mouseX, int mouseY, float partialFrame) {

        // Call frame update function on all nodes
        Point mousePoint = new Point(mouseX, mouseY);
        operateOnSubtree(mousePoint, (n, p, c) -> {
            n.frameUpdate(p, partialFrame);
            return false;
        }, false);

        RenderSystem.enableDepthTest(); // Nodes render out of order, so depth test is needed

        // Render semi-transparent grey background
        int x = getScreenFrame().x();
        int y = getScreenFrame().y();
        fillGradient(stack, x, y, x + getScreenFrame().width(), y + getScreenFrame().height(), -1072689136, -804253680);

        // Render background
        drawBackForSubtree(stack, new Point(mouseX, mouseY), partialFrame);
        // Sandwich ContainerScreen's default rendering between RedUI's foreground and background rendering
        super.render(stack, mouseX, mouseY, partialFrame);
        renderTooltip(stack, mouseX, mouseY);
        // Render foreground
        drawFrontForSubtree(stack, new Point(mouseX, mouseY), partialFrame);
    }

    @Override
    protected void renderBg(PoseStack stack, float partialFrame, int mouseX, int mouseY) {
        // We render through RedUI's render methods
    }

    @Override
    public void renderTooltipScreenSpace(PoseStack stack, Point screenSpacePoint, List<Component> tooltip) {
        renderComponentTooltip(stack, tooltip, screenSpacePoint.x, screenSpacePoint.y);
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
    public ItemRenderer getItemRenderer() {
        return itemRenderer;
    }

    @Override
    public Font getFontRenderer() {
        return font;
    }

    //endregion
}
