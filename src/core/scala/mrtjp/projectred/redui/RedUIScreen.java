package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;
import net.minecraft.client.gui.FontRenderer;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.util.text.ITextComponent;

import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;

/**
 * Base Screen class that implements the UI Node system. This is for UIs that do not have containers.
 */
public class RedUIScreen extends Screen implements RedUIRootNode {

    private final List<RedUINode> children = new LinkedList<>();

    // Frame of the GUI window
    private Rect frame;
    // Frame of the full Minecraft window (positioned at 0, 0)
    private Rect screenFrame;
    // Absolute z position of this root node
    private double zPos = 0;

    private long lastClickTime = 0;

    public RedUIScreen(int backgroundWidth, int backgroundHeight, ITextComponent title) {
        super(title);

        // These frames are fully set during init() call, when the bounds of the entire screen are known
        this.frame = new Rect(Point.zeroPoint(), new Size(backgroundWidth, backgroundHeight));
        this.screenFrame = Rect.zeroRect();

        this.onAddedToParent(); // No actual parent, so this is called once on construction
    }

    @Override
    protected void init() {
        super.init();

        screenFrame = new Rect(Point.zeroPoint(), new Size(width, height));

        Point framePos = new Point((screenFrame.width() - frame.width()) / 2, (screenFrame.height() - frame.height()) / 2);
        frame = new Rect(framePos, frame.size());
    }

    @Override
    public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialFrame) {
        super.render(matrixStack, mouseX, mouseY, partialFrame);

        RenderSystem.disableDepthTest(); // UI nodes are drawn in order, so depth test is not needed

        // Render semi-transparent grey background
        fillGradient(matrixStack, getScreenFrame().x(), getScreenFrame().y(), getScreenFrame().width(), getScreenFrame().height(), -1072689136, -804253680);

        // Call frame update function on all nodes
        Point mousePoint = new Point(mouseX, mouseY);
        operateOnSubtree(mousePoint, (n, p, c) -> {
            n.frameUpdate(p, partialFrame);
            return false;
        }, false);

        // Draw the UI
        drawBackForSubtree(this, matrixStack, mousePoint, partialFrame);
        drawFrontForSubtree(this, matrixStack, mousePoint, partialFrame);

        RenderSystem.enableDepthTest();
    }

    @Override
    public void tick() {
        super.tick();
        getSubTree(n -> true).forEach(RedUINode::update);
    }

    //TODO Move draw methods to RedUIRootNode as default implementation
    private static void drawBackForSubtree(RedUINode node, MatrixStack stack, Point mouse, float partialFrame) {
        drawForSubtree(node, stack, mouse, partialFrame, RedUINode::drawBack);
    }

    private static void drawFrontForSubtree(RedUINode node, MatrixStack stack, Point mouse, float partialFrame) {
        drawForSubtree(node, stack, mouse, partialFrame, RedUINode::drawFront);
    }

    private interface QuadConsumer<A, B, C, D> {
        void accept(A a, B b, C c, D d);
    }

    private static void drawForSubtree(RedUINode node, MatrixStack stack, Point mouse, float partialFrame, QuadConsumer<RedUINode, MatrixStack, Point, Float> drawFunction) {
        List<RedUINode> nodes = node.getZOrderedSubtree(c -> !c.isHidden(), true);
        ListIterator<RedUINode> it = nodes.listIterator(nodes.size());

        // In reverse render order tell each node that nodes below it are about to be drawn
        while (it.hasPrevious()) it.previous().onNodesBelowPreDraw();

        // In render order, tell each node that under are done rendering, then draw the node
        while (it.hasNext()) {
            RedUINode n = it.next();
            n.onNodesBelowPostDraw();

            Point parentScreenPos = n.convertParentPointToScreen(Point.zeroPoint());
            stack.pushPose();
            stack.translate(parentScreenPos.x(), parentScreenPos.y(), 0); // Z pos is for sorting only

            Point relativeMousePos = n.convertScreenPointToParent(mouse);
            drawFunction.accept(n, stack, relativeMousePos, partialFrame);

            stack.popPose();
        }
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

    @Override
    public boolean isPauseScreen() {
        return false;
    }
}
