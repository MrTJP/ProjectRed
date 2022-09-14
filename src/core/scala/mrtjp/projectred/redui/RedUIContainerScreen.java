package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.Container;
import net.minecraft.util.text.ITextComponent;

import java.util.LinkedList;
import java.util.List;

public class RedUIContainerScreen<T extends Container> extends ContainerScreen<T> implements RedUIRootNode {

    private final List<RedUINode> children = new LinkedList<>();

    // Frame of the GUI window
    private Rect frame;
    // Frame of the full Minecraft window (positioned at 0, 0)
    private Rect screenFrame;
    // Absolute z position of this root node
    private double zPos = 0;

    private long lastClickTime = 0;

    public RedUIContainerScreen(int backgroundWidth, int backgroundHeight, T container, PlayerInventory playerInventory, ITextComponent title) {
        super(container, playerInventory, title);
        this.imageWidth = backgroundWidth;
        this.imageHeight = backgroundHeight;

        // These frames are fully set during init() call, when the bounds of the entire screen are known
        this.frame = new Rect(Point.zeroPoint(), new Size(backgroundWidth, backgroundHeight));
        this.screenFrame = Rect.zeroRect();
    }

    @Override
    protected void init() {
        super.init();

        screenFrame = new Rect(Point.zeroPoint(), new Size(width, height));

        Point framePos = new Point((screenFrame.width() - frame.width()) / 2, (screenFrame.height() - frame.height()) / 2);
        frame = new Rect(framePos, frame.size());
    }

    @Override
    public void render(MatrixStack stack, int mouseX, int mouseY, float partialFrame) {

        // Call frame update function on all nodes
        Point mousePoint = new Point(mouseX, mouseY);
        operateOnSubtree(mousePoint, (n, p, c) -> {
            n.frameUpdate(p, partialFrame);
            return false;
        }, false);

        RenderSystem.enableDepthTest(); // Nodes render out of order, so depth test is needed

        // Render semi-transparent grey background
        fillGradient(stack, getScreenFrame().x(), getScreenFrame().y(), getScreenFrame().width(), getScreenFrame().height(), -1072689136, -804253680);

        // Render background
        drawBackForSubtree(stack, new Point(mouseX, mouseY), partialFrame);
        // Sandwich ContainerScreen's default rendering between RedUI's foreground and background rendering
        super.render(stack, mouseX, mouseY, partialFrame);
        renderTooltip(stack, mouseX, mouseY);
        // Render foreground
        drawFrontForSubtree(stack, new Point(mouseX, mouseY), partialFrame);
    }

    @Override
    protected void renderBg(MatrixStack stack, float partialFrame, int mouseX, int mouseY) {
        // We render through RedUI's render methods
    }

    @Override
    public void tick() {
        super.tick();
        getSubTree(n -> true).forEach(RedUINode::update);
    }

    @Override
    public boolean isPauseScreen() {
        return false;
    }

    @Override
    public boolean mouseClicked(double x, double y, int glfwMouseButton) {
        lastClickTime = System.currentTimeMillis();
        boolean consumed = super.mouseClicked(x, y, glfwMouseButton);
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
        return operateOnZOrderedSubtree(Point.zeroPoint(), (n, p, c) -> n.onCharTyped(keyPressed, glfwFlags, c), consumed);
    }

    @Override
    public boolean keyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags) {
        boolean consumed = super.keyPressed(glfwKeyCode, glfwScanCode, glfwFlags);
        return operateOnZOrderedSubtree(Point.zeroPoint(), (n, p, c) -> n.onKeyPressed(glfwKeyCode, glfwScanCode, glfwFlags, c), consumed);
    }

    @Override
    public boolean keyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags) {
        boolean consumed = super.keyReleased(glfwKeyCode, glfwScanCode, glfwFlags);
        return operateOnZOrderedSubtree(Point.zeroPoint(), (n, p, c) -> n.onKeyReleased(glfwKeyCode, glfwScanCode, glfwFlags, c), consumed);
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
        return frame.origin();
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
    public FontRenderer getFontRenderer() {
        return font;
    }

    //endregion
}
