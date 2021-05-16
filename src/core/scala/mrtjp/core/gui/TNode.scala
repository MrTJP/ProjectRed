/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem
import mrtjp.core.vec.{Point, Rect, Size}
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.{AbstractGui, FontRenderer}
import net.minecraft.client.renderer.texture.TextureManager

/**
  * Basic building block of a GUI tree.
  *
  * == Overview ==
  * A `TNode` is a fundamental building block of all content shown by a [[NodeGui]]. On its own, a `TNode` object will
  * not draw anything to the screen. Implementations must leverage interface functions to draw to the screen. Every `TNode`
  * has a few basic properties. More can be added during implementation as needed. For example, it may make sense for
  * some implementations to have a `size` property.
  *
  * == Positions ==
  * Every node has a position, defined by its `position` property. It defines a coordinate system that is originated at
  * the top-left corner of the node. Every child of this node has its own `position` property, that lives inside the
  * coordinate system of its parent.
  *
  * == Implementing ==
  * For create a custom implementation, there are several implementation overrides provided. These include events such
  * as mouse and keyboard events, as well as draw calls.
  *
  */
trait TNode extends AbstractGui
{
    /**
      * The parent of this node. This will be null if this node is not added to a
      * tree or if it is the root node.
      */
    var parent:TNode = null

    /**
      * All children of this array. Do not manipulate this directly. Children can be added to this node with
      * [[TNode.addChild()]]. A child can be removed from its parent with [[TNode.removeFromParent()]].
      */
    //Can't be named 'children' due to Screen using the same name.
    var ourChildren = Seq[TNode]()

    /**
      * @return A [[Rect]] object that represents the bounds of this node. This does not account for chlid
      *         nodes that may be outside of this frame. @see [[calculateAccumulatedFrame]]
      */
    def frame = Rect(position, Size.zeroSize)

    /**
      * The position of this node in the parent's coordinate system. This plane is origined at the top-left
      * corner, increasing moving down and to the right.
      */
    var position = Point.zeroPoint

    /**
      * The distance of the nodes in the Z axis. This axis increases moving towards the screen. Nodes with a larger
      * `zPosition` value will be rendered on top of those with a lower `zPosition` value.
      *
      * @note This value is absolute and is not relative to the parent like the [[position]] property.
      */
    var zPosition = 0.0

    /** Hidden nodes will not render themselves or any of their subnodes. */
    var hidden = false

    /**
      * Nodes with user interactions disabled will ignore any user generated events such as mouse clicks or keyboard
      * presses. It will also be excluded from hit testing.
      */
    var userInteractionEnabled = true

    def mcInst:Minecraft = Minecraft.getInstance()
    def soundHandler = mcInst.getSoundManager
    def renderEngine:TextureManager = mcInst.textureManager
    def getFontRenderer:FontRenderer = mcInst.font

    /** For checking if this is the root node in the tree. */
    def isRoot = this.isInstanceOf[NodeGui[_]]

    /**
      * Obtains the root node by moving up the tree.
      *
      * @throws IllegalStateException If there there is no root node of type [[NodeGui]].
      */
    def getRoot:NodeGui[_] =
    {
        def iterate(node:TNode):NodeGui[_] = node match {
            case ng:NodeGui[_] => ng
            case null => throw new IllegalStateException("Incomplete tree")
            case _ => iterate(node.parent)
        }
        iterate(this)
    }

    /**
      * Builds a list of all nodes between and this node and the given `to` node.
      *
      * @param to The node to build the list to.
      * @return A list containing all nodes from this node to `to`, moving upwards in the tree as the list progresses.
      *         This node will be at the head of the tree. The tail of the list will be `to` if found or the root node
      *         of this tree if it was not found on the way up.
      */
    def buildParentHierarchy(to:TNode) =
    {
        var hierarchy = Seq.newBuilder[TNode]
        def iterate(node:TNode) {
            hierarchy += node
            if (node.isRoot || node == to) return
            iterate(node.parent)
        }
        iterate(this)
        hierarchy.result()
    }

    /** Checks if this node is an ancestor of `someAncestor`. */
    def isDecendantOf(someAncestor:TNode) =
        someAncestor != this && buildParentHierarchy(someAncestor).contains(someAncestor)

    /** Checks if this node and `someRelative` are part of the same tree. */
    def isRelativeOf(someRelative:TNode) =
        someRelative != this && someRelative.getRoot == this.getRoot

    /** Converts point `p` from this node's coordinate system to `to`'s coordinate system. */
    def convertPointTo(p:Point, to:TNode):Point =
    {
        def fold(low:TNode, high:TNode, p:Point)(op:(Point, TNode) => Point) =
            low.buildParentHierarchy(high).dropRight(1).foldLeft(p)(op)

        def convertUp(low:TNode, high:TNode, p:Point) = fold(low, high, p){_+_.position}
        def convertDown(high:TNode, low:TNode, p:Point) = fold(low, high, p){_-_.position}

        if (this == to) p
//        else if (this isDecendantOf to) convertUp(this, to, p)
//        else if (to isDecendantOf this) convertDown(this, to, p)
        else if (this isRelativeOf to) { //TODO see if this still works...
            val root = getRoot
            convertDown(root, to, convertUp(this, root, p))
        }
        else throw new Exception("Attempted to convert points between unrelated nodes.")
    }
    /** Converts point `p` from `from`'s coordinate system to this node's coordinate system. */
    def convertPointFrom(p:Point, from:TNode):Point = from.convertPointTo(p, this)
    /** Converts point `p` from this node's coordinate system to screen space. */
    def convertPointToScreen(p:Point) = getRoot.position+convertPointTo(p, getRoot)
    /** Converts point `p` from screen space to this node's coordinate system. */
    def convertPointFromScreen(p:Point) = convertPointFrom(p, getRoot)-getRoot.position

    /** Converts rectangle `r` from this node's coordinate system to `to`'s coordinate system. */
    def convertRectTo(r:Rect, to:TNode):Rect = Rect(convertPointTo(r.origin, to), r.size)
    /** Converts rectangle `r` from `from`'s coordinate system to this node's coordinate system. */
    def convertRectFrom(r:Rect, from:TNode):Rect = Rect(convertPointFrom(r.origin, from), r.size)
    /** Converts rectangle `r` from this node's coordinate system to screen space. */
    def convertRectToScreen(r:Rect):Rect = Rect(convertPointToScreen(r.origin), r.size)
    /** Converts rectangle `r` from screen space to this node's coordinate system. */
    def convertRectFromScreen(r:Rect):Rect = Rect(convertPointFromScreen(r.origin), r.size)

    /**
      * Calculates a bounding box containing all descendants.
      *
      * @return A [[Rect]] object that encapsulates all bounding boxes of decendants. This excludes nodes that are [[hidden]].
      */
    def calculateChildrenFrame:Rect =
    {
        val rect = if (ourChildren.isEmpty) Rect.zeroRect
            else ourChildren.filterNot(_.hidden).map(_.calculateAccumulatedFrame).reduceLeft(_ union _)
        Rect(convertPointTo(rect.origin, parent), rect.size)
    }

    /**
      * Calculates a bounding box containing this node and all decendents.
      *
      * @return A [[Rect]] object that encapsulates all bounding boxes of this node and its decendants. This excludes
      *         nodes that are [[hidden]].
      */
    def calculateAccumulatedFrame:Rect = frame.union(calculateChildrenFrame)

    /**
      * For checking if the given point `absPoint` is inside the bounds of this node.
      *
      * @param absPoint The point to check, given in screenspace.
      * @return True if the point intersects with this node.
      *
      * @todo Change `absPoint` to be in local space to avoid multiple conversions during hit testing
      */
    def traceHit(absPoint:Point) =
    {
        val f = frame
        val af = Rect(parent.convertPointToScreen(f.origin), f.size)
        af.contains(absPoint)
    }

    /**
      * Utility function used for hit testing on the tree. The given point `point` should reside in the same coordinate
      * system that this node resides in.
      *
      * @param point A point in the coordinate system of this node's parent.
      * @return A sequence all intersecting nodes, ordered by their [[zPosition]] property. The head of the list is
      *         the one that was hit first, and the tail was hit last.
      *
      * @todo This can be made more efficient by having it call traceHit with a local point instead of one in screen
      *       space.
      */
    def hitTest(point:Point):Seq[TNode] =
    {
        if (parent == null) throw new Exception("Cannot hittest a node without a parent.")
        if (isRoot) throw new Exception("Cannot hittest a root node.")

        var test = Seq.newBuilder[TNode]
        val ap = parent.convertPointToScreen(point)
        for (c <- getRoot.subTree(true))
            if (c.traceHit(ap)) test += c

        test.result().sortBy(_.zPosition).reverse //todo instead of reversing, order by -zPosition
    }

    /**
      * Utility function used for testing if this node is at the top of a hit test of point `point`.
      *
      * @param point The point in the coordinate system of this node's parent.
      * @return True if this node was hit and no other nodes occluded it.
      */
    def rayTest(point:Point):Boolean =
    {
        val s = hitTest(point)
        s.nonEmpty && s.head == this
    }

    /**
      * Creates a sequence of all of this node's descendants.
      *
      * @param activeOnly If true, will filter out all inactive nodes (i.e. those that are [[hidden]] or do not have
      *                   [[userInteractionEnabled user interaction enabled]].
      * @return A sequence of all descendants.
      */
    def subTree(activeOnly:Boolean = false) =
    {
        val s = Seq.newBuilder[TNode]
        def gather(children:Seq[TNode]) {
            val ac = if (activeOnly) children.filter(c => !c.hidden && c.userInteractionEnabled) else children
            s ++= ac
            for (c <- ac) gather(c.ourChildren)
        }
        if (!activeOnly || (!hidden && userInteractionEnabled)) gather(ourChildren)
        s.result()
    }

    /**
      * Changes the [[zPosition]] of this node to `z` and adjusts all subnodes to have the same relative z-position
      * as before.
      *
      * @param z The new z-position for this node.
      */
    def pushZTo(z:Double)
    {
        pushZBy(z-zPosition)
    }

    /**
      * Adds `z` to this node's [[zPosition]] and adjusts all subnodes to have the same relative z-position
      * as before.
      *
      * @param z The value to add to this node's z-position.
      */
    def pushZBy(z:Double)
    {
        for (c <- subTree():+this)
            c.zPosition += z
    }

    /** Adds node `w` to the tree as this node's child. */
    def addChild(w:TNode) =
    {
        w.parent = this
        ourChildren :+= w
        w.onAddedToParent_Impl()
    }

    /** Removes this node and all descendant nodes from the tree. */
    def removeFromParent()
    {
        parent.ourChildren = parent.ourChildren.filterNot(_ == this)
        parent = null
    }

    /** Creates a sequence of all descendants of this node, sorted by [[zPosition]]. */
    def childrenByZ = ourChildren.sortBy(_.zPosition)

    /** Creates a sequence of this node and all descendants, sorted by [[zPosition]]. */
    def familyByZ = (Seq(this)++ourChildren).sortBy(_.zPosition)

    protected[gui] final def update()
    {
        update_Impl()
        for (c <- childrenByZ) c.update()
    }

    protected[gui] final def frameUpdate(mouse:Point, rframe:Float)
    {
        frameUpdate_Impl(mouse, rframe)
        for (c <- childrenByZ) c.frameUpdate(mouse-position, rframe)
    }

    private final def operate2(consumed:Boolean)(self:(Boolean) => Boolean)(sub:(TNode, Boolean) => Boolean) =
    {
        familyByZ.reverse.foldLeft(consumed)((c, w) => (if (w == this) self(c) else sub(w, c)) || c)
    }

    protected[gui] def mouseClicked(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        if (hidden || !userInteractionEnabled) return false
        val dp = p-position
        operate2(consumed){mouseClicked_Impl(p, button, _)}{_.mouseClicked(dp, button, _)}
    }

    protected[gui] def mouseReleased(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        if (hidden || !userInteractionEnabled) return false
        val dp = p-position
        operate2(consumed){mouseReleased_Impl(p, button, _)}{_.mouseReleased(dp, button, _)}
    }

    protected[gui] def mouseDragged(p:Point, button:Int, time:Long, consumed:Boolean):Boolean =
    {
        if (hidden || !userInteractionEnabled) return false
        val dp = p-position
        operate2(consumed){mouseDragged_Impl(p, button, time, _)}{_.mouseDragged(dp, button, time, _)}
    }

    protected[gui] def mouseScrolled(p:Point, dir:Int, consumed:Boolean):Boolean =
    {
        if (hidden || !userInteractionEnabled) return false
        val dp = p-position
        operate2(consumed){mouseScrolled_Impl(p, dir, _)}{_.mouseScrolled(dp, dir, _)}
    }

    protected[gui] def keyPressed(ch:Char, keycode:Int, consumed:Boolean):Boolean =
    {
        if (hidden || !userInteractionEnabled) return false
        operate2(consumed){keyPressed_Impl(ch, keycode, _)}{_.keyPressed(ch, keycode, _)}
    }

    protected[gui] def drawBack(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawBack_Impl(stack, mouse, rframe)
                else
                {
                    translateTo()
                    n.drawBack(stack, dp, rframe)
                    translateFrom()
                }
            }
        }
    }

    protected[gui] def drawFront(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawFront_Impl(stack, mouse, rframe)
                else
                {
                    translateTo()
                    n.drawFront(stack, dp, rframe)
                    translateFrom()
                }
            }
        }
    }

    //todo move to NodeGui.
    protected[gui] def rootDrawBack(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            translateTo()
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawBack_Impl(stack, mouse, rframe)
                else n.drawBack(stack, dp, rframe)
            }
            translateFrom()
        }
    }

    //todo move to NodeGui.
    protected[gui] def rootDrawFront(stack:MatrixStack, mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawFront_Impl(stack, mouse, rframe)
                else n.drawFront(stack, dp, rframe)
            }
        }
    }

    protected[gui] def translateTo(){RenderSystem.translated(position.x, position.y, 0)}//zPosition-(if (parent == null) 0 else parent.zPosition))}
    protected[gui] def translateFrom(){RenderSystem.translated(-position.x, -position.y, 0)}// -(zPosition-(if (parent == null) 0 else parent.zPosition)))}

    protected[gui] def translateToScreen()
    {
        val Point(sx, sy) = parent.convertPointToScreen(Point.zeroPoint)
        RenderSystem.translated(-sx, -sy, 0)
    }
    protected[gui] def translateFromScreen()
    {
        val Point(sx, sy) = parent.convertPointToScreen(Point.zeroPoint)
        RenderSystem.translated(sx, sy, 0)
    }

    /** IMPLEMENTATION OVERRIDES **/

    /**
      * Called every tick from the main game loop.
      */
    def update_Impl(){}

    /**
      * Called every frame before background draw call.
      *
      * @param mouse The current position of the mouse, relative to the parent.
      * @param rframe The partial frame until the next frame.
      */
    def frameUpdate_Impl(mouse:Point, rframe:Float){}

    /** Called when this node is added to another as a child. */
    def onAddedToParent_Impl(){}

    /**
      * Called when the mouse button is clicked.
      *
      * @param p The current position of the mouse, relative to the parent.
      * @param button The button that was clicked. 0 is left button, 1 is right.
      * @param consumed Indicates if another node has consumed this event.
      * @return Indicates this node has consumed this event. Nodes that receive this event after this one will have the
      *         `consumed` flag set to true.
      */
    def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) = false

    /**
      * Called when the mouse button is released.
      *
      * @param p The current position of the mouse, relative to the parent.
      * @param button The button that was released. 0 is left button, 1 is right.
      * @param consumed Indicates if another node has consumed this event.
      * @return Indicates this node has consumed this event. Nodes that receive this event after this one will have the
      *         `consumed` flag set to true.
      */
    def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) = false

    /**
      * Called constantly while the mouse is held down.
      *
      * @param p The current position of the mouse, relative to the parent.
      * @param button The button that is currently held down. 0 is left, 1 is right
      * @param time Amount of time the button has been held down for.
      * @param consumed Indicates if another node has consumed this event.
      * @return Indicates this node has consumed this event. Nodes that receive this event after this one will have the
      *         `consumed` flag set to true.
      */
    def mouseDragged_Impl(p:Point, button:Int, time:Long, consumed:Boolean) = false

    /**
      * Called when the mouse wheel is scrolled.
      *
      * @param p The current position of the mouse, relative to the parent.
      * @param dir The direction of scroll. Negative for down, positive for up.
      * @param consumed Indicates if another node has consumed this event.
      * @return Indicates this node has consumed this event. Nodes that receive this event after this one will have the
      *         `consumed` flag set to true.
      */
    def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) = false

    /**
      * Called when a key is pressed on the keyboard.
      *
      * @param c The character that was pressed.
      * @param keycode The keycode for the button that was pressed.
      * @param consumed Indicates if another node has consumed this event.
      * @return Indicates this node has consumed this event. Nodes that receive this event after this one will have the
      *         `consumed` flag set to true.
      */
    def keyPressed_Impl(c:Char, keycode:Int, consumed:Boolean) = false

    /**
      * Called to draw the background. All drawing is done relative to the parent, as GL11 is translated to the
      * parents position during this operation.  However, for the root node, drawing is relative to itself.
      *
      * @note Since the root node does not have a parent, drawing is done relative to its own position.
      *
      * @param mouse The current position of the mouse, relative to the parent.
      * @param rframe The partial frame until the next frame.
      */
    def drawBack_Impl(stack:MatrixStack, mouse:Point, rframe:Float){}

    /**
      * Called to draw the background. All drawing is done relative to the parent, as GL11 is translated to the
      * parents position during this operation.  However, for the root node, drawing is relative to itself.
      *
      * @note Since the root node does not have a parent, drawing is done relative to its own position.
      *
      * @param mouse The current position of the mouse, relative to the parent.
      * @param rframe The partial frame until the next frame.
      */
    def drawFront_Impl(stack:MatrixStack, mouse:Point, rframe:Float){}
}
