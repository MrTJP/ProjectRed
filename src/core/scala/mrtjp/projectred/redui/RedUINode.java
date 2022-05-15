package mrtjp.projectred.redui;

import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.core.vec.Rect;
import mrtjp.core.vec.Size;
import net.minecraft.util.text.ITextProperties;
import net.minecraftforge.fml.client.gui.GuiUtils;
import org.lwjgl.glfw.GLFW;

import javax.annotation.Nullable;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.function.Predicate;

/**
 * A Node object that is organized into a tree structure. Each node receives user input events and can contain
 * child nodes. Nodes can be used purely for organizing subtrees, or also as actual rendered UI elements.
 * <p>
 * Nodes have a position and render themselves relative to their parent's position. A node's position is an offset
 * of its parent's position. There are several utility methods to help convert points from one node's coordinate
 * system to another.
 */
public interface RedUINode {

    // Getter and setter methods that must be implemented on the base class. These are used
    // by the various default implementations.

    /**
     * @return The root node for this tree. Can be this, or null if this node is not
     * part of a valid tree.
     */
    RedUIRootNode getRoot();

    void setRoot(@Nullable RedUIRootNode root);

    /**
     * @return The parent for this node, or null if not set
     * @throws RuntimeException if this is the root node
     */
    RedUINode getParent();

    void setParent(@Nullable RedUINode parent);

    /**
     * Returns a list of the children for this node. This list will be modified as
     * nodes are added and removed, so it must not be an immutable wrapper around
     * the actual list.
     */
    List<RedUINode> getOurChildren();

    /**
     * @return A Rect representing the bounds for this node, positioned in
     * the parent's coordinate space
     */
    Rect getFrame();

    /**
     * @return This node's position in the parent's coordinate space
     */
    Point getPosition();

    /**
     * @return The absolute Z position for this node. Higher Z position nodes are rendered
     * on top of other nodes.
     */
    double getZPosition();

    /**
     * @param zPosition The absolute Z position to set
     */
    void setZPosition(double zPosition);

    /**
     * @return Z position of this node relative to the parent (or screen if this is the root node)
     */
    double getRelativeZPosition();

    /**
     * @return True if this node should be ignored for rendering and hit-testing
     */
    boolean isHidden();

    /**
     * @return True if this is the root node
     */
    default boolean isRoot() {
        return getRoot() == this;
    }

    default void assertValidNode() throws RuntimeException {
        if (getRoot() == null || getParent() == null) { throw new RuntimeException("Node is not valid"); }
    }

    /**
     * Builds a list traversing up the tree graph, such that:
     *
     * <ul>
     * <li> The first element is this node
     * <li> The last element is either <code>to</code> or the root node, whichever
     *      one is found first.
     * </ul>
     *
     * @param to The node to stop at if found
     * @return The hierarchy list
     */
    default List<RedUINode> buildParentHierarchy(RedUINode to) {
        List<RedUINode> hierarchy = new LinkedList<>();
        RedUINode next = this;

        hierarchy.add(next);
        while (!next.isRoot() && next != to) {
            next = next.getParent();
            if (next == null) {
                throw new RuntimeException("Found node with null parent while building parent hierarchy");
            }
            hierarchy.add(next);
        }

        return hierarchy;
    }

    /**
     * Builds a list of nodes in subtree rooted at this node, ignoring branches that do not pass
     * the filter. Filtered nodes are completely ignored, including all descendants.
     *
     * @param filter      A predicate to filter branches
     * @param leavesFirst If true, subtree list will be sorted with leaves first rather than root
     * @return A subtree containing only nodes that pass the filter
     */
    default List<RedUINode> getSubTree(Predicate<RedUINode> filter, boolean leavesFirst) {
        LinkedList<RedUINode> subTree = new LinkedList<>();
        Queue<RedUINode> queue = new LinkedList<>();
        queue.add(this);
        while (!queue.isEmpty()) {
            RedUINode next = queue.poll();
            if (filter.test(next)) {
                if (leavesFirst) { subTree.addFirst(next); } else { subTree.add(next); }
                queue.addAll(next.getOurChildren());
            }
        }
        return subTree;
    }

    /**
     * Builds a subtree list with the root at the head
     *
     * @see RedUINode#getSubTree(Predicate, boolean)
     */
    default List<RedUINode> getSubTree(Predicate<RedUINode> filter) {
        return getSubTree(filter, false);
    }

    /**
     * Builds a subtree list with nodes sorted in ascending or descending z order. Nodes at the front
     * of the list will have a larger Z position and should be rendered on top.
     * <p>
     * Nodes with equal Z positions will be ordered based on their location in the tree,
     * with nodes further from the root receiving priority.
     *
     * @param filter   Subtree branch filter
     * @param reversed If true, sort in ascending z order
     * @return Subtree in descending z position order
     */
    default List<RedUINode> getZOrderedSubtree(Predicate<RedUINode> filter, boolean reversed) {
        List<RedUINode> subTree = getSubTree(filter, !reversed);
        subTree.sort(Comparator.comparingDouble(n -> n.getZPosition() * (reversed ? 1 : -1)));
        return subTree;
    }

    /**
     * @return True if this node is part of the given <code>node</code>'s subtree
     */
    default boolean isDescendantOf(RedUINode node) {
        return node != this && buildParentHierarchy(node).contains(node);
    }

    /**
     * @return True if the given <code>node</code> has the same root node
     */
    default boolean isRelativeOf(RedUINode node) {
        return node != this && node.getRoot() == this.getRoot();
    }

    /**
     * Converts a point from one node's coordinate system to another node's coordinate system.
     * This assumes that both this node and the target node are part of the same tree. That is,
     * both nodes have identical root nodes.
     *
     * @param p  The point to convert, in this node's coordinate space (i.e. it is relative to this node's position)
     * @param to The target node coordinate system
     * @return The converted point
     */
    default Point convertPointTo(Point p, RedUINode to) {

        if (this == to) return p;

        if (!this.isRelativeOf(to)) throw new RuntimeException("Unable to convert point between unrelated nodes");

        // This node's origin in screen space
        Point thisOffset = this.getScreenOffset();
        // Target node's origin in screen space
        Point thatOffset = to.getScreenOffset();
        // Calculate vector between the two screen space points
        Point difference = thatOffset.subtract(thisOffset);

        // Offset the point by the vector
        return p.add(difference);
    }

    /**
     * Convert a given point <code>p</code> in this node's coordinate system to screen space
     *
     * @param p The point to convert in this node's coordinate space
     * @return The converted point in screen space
     */
    default Point convertPointToScreen(Point p) {
        List<RedUINode> nodesToRoot = buildParentHierarchy(getRoot());
        Point screenOffset = nodesToRoot.stream()
                .map(RedUINode::getPosition)
                .reduce(Point.zeroPoint(), Point::add);

        return p.add(screenOffset);
    }

    /**
     * Convert a given point in screen space to this node's coordinate system
     *
     * @param p The point in screen space
     * @return The converted point in this node's coordinate space
     */
    default Point convertPointFromScreen(Point p) {
        List<RedUINode> nodesToRoot = buildParentHierarchy(getRoot());
        Point screenOffset = nodesToRoot.stream()
                .map(RedUINode::getPosition)
                .reduce(Point.zeroPoint(), Point::add);

        return p.subtract(screenOffset);
    }

    /**
     * @return This node's position in screen space
     */
    default Point getScreenOffset() {
        return buildParentHierarchy(getRoot()).stream()
                .map(RedUINode::getPosition)
                .reduce(Point.zeroPoint(), Point::add);
    }

    /**
     * Converts a rectangle from this node's coordinate system to the given target node's coordinate system
     *
     * @param r  The rectangle to convert with origin inside this node's coordinate system
     * @param to The node containing the target coordinate system
     * @return The converted rectangle
     */
    default Rect convertRectTo(Rect r, RedUINode to) { return new Rect(convertPointTo(r.origin(), to), r.size()); }

    /**
     * Converts a rectangle from this node's coordinate system to screen space
     *
     * @param r The rectangle to convert with origin inside this node's coordinate system
     * @return The rectangle with origin in screen space
     */
    default Rect convertRectToScreen(Rect r) { return new Rect(convertPointToScreen(r.origin()), r.size()); }

    /**
     * Converts a rectangle from screen space to this node's coordinate system
     *
     * @param r The rectangle to convert with origin in screen space
     * @return The rectangle with origin in this node's coordinate system
     */
    default Rect convertRectFromScreen(Rect r) { return new Rect(convertPointFromScreen(r.origin()), r.size()); }

    /**
     * Converts a point in the coordinate system of this node's parent to screen space
     * <p>
     * This method assists in converting points to screen space in situations where this
     * node may be the root node, in which case getParent() is not available, and therefore,
     * you cannot simply do:
     *
     * <pre>
     *  this.getParent().convertPointToScreen(p)
     * </pre>
     *
     * @param point The relatable point to convert, in this node's parent's coordinate system
     * @return The converted point in screen space
     */
    default Point convertParentPointToScreen(Point point) {
        return isRoot() ? point : getParent().convertPointToScreen(point);
    }

    /**
     * Converts a screen space point to the coordinate space of this node's parent.
     * <p>
     * This method assists in converting points from screen space in situations where this
     * node may be the root node, in which case getParent() is not available, and therefore,
     * you cannot simply do:
     *
     * <pre>
     *  this.getParent().convertPointToScreen(p)
     * </pre>
     *
     * @param point The point to convert, in screen space
     * @return The converted point in this node's parent's coordinate system
     */
    default Point convertScreenPointToParent(Point point) {
        return isRoot() ? point : getParent().convertPointFromScreen(point);
    }

    /**
     * Converts a rectangle in the coordinate system of this node's parent to screen space
     *
     * @param r The rectangle to convert, in parent's coordinate space
     * @return The converted rectangle in screen space
     */
    default Rect convertParentRectToScreen(Rect r) { return new Rect(convertParentPointToScreen(r.origin()), r.size()); }

    /**
     * Converts a screen space rectangle to the coordinate space of this node's parent
     *
     * @param r The rectangle to convert, in screen space
     * @return The converted rectangle, in the parent's coordinate system
     */
    default Rect convertScreenRectToParent(Rect r) { return new Rect(convertScreenPointToParent(r.origin()), r.size()); }

    /**
     * Calculates the GL11 screen-space equivalent this node's frame.
     *
     * @return Bottom-left anchored frame in GL11 window space
     */
    default Rect calculateGL11Frame() {
        // Convert frame to screen space anchored at bottom-left instead of top-left
        Rect screenFrame = getRoot().getScreenFrame();
        Rect frame = convertParentRectToScreen(getFrame());
        Rect bottomLeftFrame = new Rect(
                new Point(frame.x(), screenFrame.height() - frame.y() - frame.height()),
                frame.size());

        // Convert from GUI screen space to GL11 screen space using the Minecraft Gui Scale value
        double glWScale = getRoot().getMinecraft().getWindow().getGuiScale();
        double glHScale = getRoot().getMinecraft().getWindow().getGuiScale();
        Rect gl11Rect = new Rect(new Point((int) Math.round(bottomLeftFrame.x() * glWScale), (int) Math.round(bottomLeftFrame.y() * glHScale)),
                new Size((int) Math.round(bottomLeftFrame.width() * glWScale), (int) Math.round(bottomLeftFrame.height() * glHScale)));

        return gl11Rect;
    }

    /**
     * Calculate a frame encompassing all frames in the entire subtree. Hidden nodes and all its
     * descendants are excluded.
     *
     * @return Rect in the coordinate system of this node's parent
     */
    default Rect calculateAccumulatedFrame() {
        Rect screenSpaceFrame = getSubTree(n -> !n.isHidden()).stream()
                .map(n -> n.convertParentRectToScreen(n.getFrame()))
                .reduce(Rect.zeroRect(), Rect::union);
        return convertScreenRectToParent(screenSpaceFrame);
    }

    /**
     * Returns a list of subtree nodes that intersect with the given point. Hits are ordered
     * by z position. For each node, {@link RedUINode#checkHit(Point)} is called to check if
     * the node intersects the point.
     *
     * <p>
     *
     * @param point The point to test, in the parent's coordinate system
     * @return A list of nodes intersecting the given point
     */
    default List<RedUINode> hitTest(Point point) {

        LinkedList<RedUINode> hits = new LinkedList<>();
        Point absPoint = convertParentPointToScreen(point);

        for (RedUINode child : getSubTree(n -> !n.isHidden())) {
            if (child.checkHit(absPoint)) hits.addFirst(child); // Naturally order child nodes higher than parents
        }

        // Use Z Position for hit ordering, falling back to child-first order for equal z positions
        hits.sort(Comparator.comparingDouble(n -> n.getZPosition() * -1));

        return hits;
    }

    /**
     * Test to see if a given point in screen space intersects this node's frame. By default,
     * this checks if the given point lies in the node's frame.
     *
     * @param absPoint Point to test in screen space
     * @return True if the test point intersects this node's frame
     * @see RedUINode#getFrame()
     * @see RedUINode#hitTest(Point)
     */
    default boolean checkHit(Point absPoint) {
        Point relatablePoint = convertScreenPointToParent(absPoint);
        return getFrame().contains(relatablePoint);
    }

    /**
     * Checks if a node in the tree is the top-most hit at a given point
     *
     * @param p The point to trace hits at, in the coordinate space of this node's parent
     * @return True if <code>node</code> was the first node intersecting <code>point</code>.
     */
    default boolean isFirstHit(Point p) {
        Point screenPoint = convertParentPointToScreen(p);
        List<RedUINode> hits = getRoot().hitTest(screenPoint);
        return !hits.isEmpty() && hits.get(0) == this;
    }

    /**
     * Offset the Z position of this node and its subtree by the given delta
     *
     * @param z The amount to offset z position
     */
    default void pushZBy(double z) {
        for (RedUINode n : getSubTree(n -> true)) {
            n.setZPosition(n.getZPosition() + z);
        }
    }

    /**
     * Sets this node's z position to the given value, and keeps the z position of all
     * other nodes in the subtree the same relative to this node
     *
     * @param z The z position to set
     */
    default void pushZTo(double z) {
        double offset = z - getZPosition();
        pushZBy(offset);
    }

    /**
     * Adds a child node to this node. This will set the given node's parent to this node,
     * and set the root of all nodes in its subtree to this node's root. After,
     * {@link RedUINode#onAddedToParent()} is called on the newly added child.
     *
     * @param child The child to add
     */
    default void addChild(RedUINode child) {
        getOurChildren().add(child);
        child.setParent(this);
        final RedUIRootNode root = getRoot();
        child.getSubTree(n -> true).forEach(n -> n.setRoot(root));
        child.onAddedToParent();
    }

    /**
     * Removes this node and its subtree from the tree graph. This node's parent is set to null,
     * and the root node is set to null of all nodes in the subtree.
     */
    default void removeFromParent() {
        getParent().getOurChildren().remove(this);
        setParent(null);
        getSubTree(n -> true).forEach(n -> n.setRoot(null));
    }

    /**
     * Runs all nodes in the subtree through a given operation in unspecific order.
     *
     * @param p        The starting point, in this node's parent's coordinate space. This is converted to keep
     *                 it relative to the node's parent that is being operated on
     * @param op       The function to operate on each node
     * @param consumed Initial consumed state. This is given to the operation function and will stay false
     *                 until one of the operations returns true, at which point it will stay true for all subsequent nodes.
     * @return True if any operation has consumed this event (or if it was initially true to begin with)
     * @see RedUINode#operateOnZOrderedSubtree(Point, SubtreeOp, boolean)
     * <p>
     * TODO Perhaps more practical to call all functions on subtree with screen-space point, and each node can convert if they care about the event
     */
    default boolean operateOnSubtree(Point p, SubtreeOp op, boolean consumed) {
        consumed |= op.operate(this, p, consumed);
        Point relativePoint = p.subtract(getPosition());

        for (RedUINode child : getOurChildren()) {
            consumed |= child.operateOnSubtree(relativePoint, op, consumed);
        }

        return consumed;
    }

    /**
     * Runs all nodes in the subtree through a given operation in descending Z order, prioritizing
     * nodes further from the root for equal Z positions. If order of operation is not important,
     * use {@link RedUINode#operateOnSubtree(Point, SubtreeOp, boolean)} to avoid unnecessary sorting.
     *
     * @param p        The starting point, in this node's parent's coordinate space. This is converted to keep
     *                 it relative to the node's parent that is being operated on
     * @param op       The function to operate on each node
     * @param consumed Initial consumed state. This is given to the operation function and will stay false
     *                 until one of the operations returns true, at which point it will stay true for all subsequent nodes.
     * @return True if any operation has consumed this event (or if it was initially true to begin with)
     */
    default boolean operateOnZOrderedSubtree(Point p, SubtreeOp op, boolean consumed) {
        Point screenPoint = convertParentPointToScreen(p);
        for (RedUINode n : getZOrderedSubtree(n -> true, false)) {
            Point relativePoint = n.convertScreenPointToParent(screenPoint); // Push point into n's parent's coordinate system
            consumed |= op.operate(n, relativePoint, consumed);
        }

        return consumed;
    }

    /**
     * Called when this node as added to another, after the <code>parent</code> and <code>root</code> nodes
     * are set.
     *
     * @see RedUINode#addChild(RedUINode)
     */
    default void onAddedToParent() { }

    /**
     * Called on every node in the graph once per tick
     *
     * @see RedUIScreen#tick()
     */
    default void update() { }

    /**
     * Called once per render call prior to rendering the background via {@link RedUINode#drawBack(MatrixStack, Point, float)}
     *
     * @param mouse        Mouse position in parent's coordinate space
     * @param partialFrame Partial value representing the progress from one tick to the next
     */
    default void frameUpdate(Point mouse, float partialFrame) { }

    /**
     * Called on mouse click event.
     * <p>
     * Nodes throughout the graph are called in z order.
     *
     * @param p               The mouse position, relative to the parent
     * @param glfwMouseButton The mouse button code
     * @param consumed        True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     * @see GLFW
     */
    default boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) { return false; }

    /**
     * Called on mouse released event.
     * <p>
     * Nodes throughout the graph are called in z order.
     *
     * @param p               The mouse position, relative to the parent
     * @param glfwMouseButton The mouse button code
     * @param timeHeld        Milliseconds since the last mouseClicked event
     * @param consumed        True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     * @see GLFW
     */
    default boolean mouseReleased(Point p, int glfwMouseButton, long timeHeld, boolean consumed) { return false; }

    /**
     * Called on mouse dragged event.
     * <p>
     * Nodes throughout the graph are called in z order.
     *
     * @param p               The mouse position, relative to the parent
     * @param glfwMouseButton The mouse button code
     * @param timeHeld        Milliseconds since the last mouseClicked event
     * @param consumed        True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     */
    default boolean mouseDragged(Point p, int glfwMouseButton, long timeHeld, boolean consumed) { return false; }

    /**
     * Called on mouse scrolled event.
     * <p>
     * Nodes throughout the graph are called in z order.
     *
     * @param p        The mouse position, relative to the parent
     * @param scroll   The scroll amount (positive for scroll up, negative for scroll down)
     * @param consumed True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     */
    default boolean mouseScrolled(Point p, double scroll, boolean consumed) { return false; }

    /**
     * Key press event called when any key is pressed down
     *
     * @param glfwKeyCode  GLFW key code
     * @param glfwScanCode GLFW scan code
     * @param glfwFlags    GLFW modifier flags TODO: Change to glfwModifierMask
     * @param consumed     True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     * @see GLFW
     */
    default boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) { return false; }

    /**
     * Key release event called when a previously held key is released
     *
     * @param glfwKeyCode  GLFW key code
     * @param glfwScanCode GLFW scan code
     * @param glfwFlags    GLFW modifier flags
     * @param consumed     True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     * @see GLFW
     */
    default boolean onKeyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) { return false; }

    /**
     * Special key press event that is called specifically for typed characters. Also fired repeatedly
     * for when the key is continuously held down.
     *
     * @param ch        The character that was typed
     * @param glfwFlags GLFW modifier flags
     * @param consumed  True if another higher-ordered node consumed this event
     * @return True if this event was consumed
     * @see GLFW
     */
    default boolean onCharTyped(char ch, int glfwFlags, boolean consumed) { return false; }

    /**
     * Called before {@link RedUINode#drawBack(MatrixStack, Point, float)} or
     * {@link RedUINode#drawFront(MatrixStack, Point, float)} is called on all children, but after
     * the draw method on this node is called.
     */
    default void onNodesBelowPreDraw() { }

    /**
     * Draw call for the background layer, typically used to render the background. Drawing is done
     * relative to the parent.
     *
     * @param stack        The matrix stack that is translated to the parent
     * @param mouse        Current mouse position, relative to the parent
     * @param partialFrame Partial frames between ticks
     */
    default void drawBack(MatrixStack stack, Point mouse, float partialFrame) { }

    /**
     * Draw call for the foreground layer, typically used to render items, tooltips, etc.
     *
     * @param stack        The matrix stack that is translated to the parent
     * @param mouse        Current mouse position, relative to the parent
     * @param partialFrame Partial frames between ticks
     */
    default void drawFront(MatrixStack stack, Point mouse, float partialFrame) { }

    /**
     * Called after {@link RedUINode#drawBack(MatrixStack, Point, float)} or
     * {@link RedUINode#drawFront(MatrixStack, Point, float)} is called on all nodes below, but after
     * the draw method on this node is called.
     */
    default void onNodesBelowPostDraw() { }

    /**
     * Node operation function that can be run over an entire graph
     *
     * @see RedUINode#operateOnSubtree(Point, SubtreeOp, boolean)
     * @see RedUINode#operateOnZOrderedSubtree(Point, SubtreeOp, boolean)
     */
    @FunctionalInterface
    interface SubtreeOp {

        /**
         * Function that is run over multiple nodes in a graph
         *
         * @param node          Node to operate on
         * @param relativePoint A point relative to the given node's parent
         * @param consumed      True if previous operation has consumed this event
         * @return True to consume this event
         */
        boolean operate(RedUINode node, Point relativePoint, boolean consumed);
    }

    // Utility methods

    default void renderTooltip(MatrixStack stack, Point mouse, List<ITextProperties> tooltip) {

        if (tooltip.isEmpty()) return;

        // Draw tooltip in screen-space to allow it to force-fit on screen

        Point screenOffset = getParent().getScreenOffset();
        Point mouseScreenSpace = screenOffset.add(mouse);

        stack.pushPose();
        stack.translate(-screenOffset.x(), -screenOffset.y(), 0);

        GuiUtils.drawHoveringText(stack, tooltip, mouseScreenSpace.x(), mouseScreenSpace.y(), getRoot().getScreenFrame().width(), getRoot().getScreenFrame().height(), -1, getRoot().getFontRenderer());

        stack.popPose();
    }
}
