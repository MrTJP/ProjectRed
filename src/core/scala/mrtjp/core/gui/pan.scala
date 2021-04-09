/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import codechicken.lib.colour.EnumColour
import mrtjp.core.vec.{Point, Rect, Size, Vec2}

/**
  * Represents a rectangular area in which all subnodes can be moved around the coordinate plane by clicking and
  * dragging. Used for creating a window and being able to pan subnodes around in that window. Typically used alongside
  * a [[ClipNode]] so that things outside the rectangular area are not rendered.
  */
class PanNode extends TNode
{
    /** The size of this node. Subnodes can be panned by clicking and dragging inside this node. */
    var size = Size.zeroSize
    override def frame = Rect(position, size)

    var scrollModifier = Vec2(1, 1)
    /** Thickness of the scrollbars. */
    var scrollBarThickness = 4
    /** The background color of the scrollbars in ARGB format. */
    var scrollBarBGColour = EnumColour.LIGHT_GRAY.argb(0x66)
    /** The color of the actual scroll bar/indicator in ARGB format. */
    var scrollBarColour = EnumColour.GRAY.argb(0x99)
    /** Flag for toggling the rendering of the vertical scroll bar on the right edge. */
    var scrollBarVertical = true
    /** Flag for toggling the rendering of the horizontal scroll bar on the bottom edge. */
    var scrollBarHorizontal = true

    /** A function that indicates if panning can happen. Typically this will check for a hotkey. */
    var dragTestFunction = {() => false}
    /** A callback function for when panning occurs. Called every frame when subnodes are translated. */
    var panDelegate = {() => }

    var clampSlack = 0
    /** A debug flag for rendering an outline showing the clamp box. */
    var debugShowClampBox = false

    private var cFrame = Rect.zeroRect
    private var mouseDown = false
    private var mouseDownRight = false
    private var mouseDownBelow = false
    private var lastMousePos = Point.zeroPoint

    private var raytestMode = 0

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        cFrame = calculateChildrenFrame
        val delta = mouse-lastMousePos
        lastMousePos = mouse

        if (mouseDownRight || mouseDownBelow)
        {
            val sf2 = size.vectorize/cFrame.size.vectorize
            val modVec = if (mouseDownRight) Vec2.up else Vec2.left
            panChildren(delta.vectorize*scrollModifier/sf2*modVec)
        }
        else if (mouseDown)
        {
            panChildren(delta.vectorize*scrollModifier)
        }
        else
        {
            //x disp
            val l = 0+clampSlack
            val lc = if (cFrame.size.width > size.width) cFrame.origin.x max l else cFrame.origin.x min l
            val ld = l-lc
            val r = size.width-clampSlack
            val rc = if (cFrame.size.width > size.width) cFrame.maxX min r else cFrame.maxX max r
            val rd = r-rc

            //y disp
            val t = 0+clampSlack
            val tc = if (cFrame.size.height > size.height) cFrame.origin.y max t else cFrame.origin.y min t
            val td = t-tc
            val b = size.height-clampSlack
            val bc = if (cFrame.size.height > size.height) cFrame.maxY min b else cFrame.maxY max b
            val bd = b-bc

            panChildren(Vec2(ld+rd, td+bd)*0.1*scrollModifier)
        }
    }

    def panChildren(d:Vec2)
    {
        val d2 = d
        if (d2 != Vec2.zeroVec)
        {
            for (c <- ourChildren) c.position = Point(c.position.vectorize+d2)
            panDelegate()
        }
    }

    override def drawBack_Impl(mouse:Point, rframe:Float)
    {
        drawScrollBars()
    }

    override def drawFront_Impl(mouse:Point, rframe:Float){}

    private def drawScrollBars()
    {
        if (scrollBarVertical)
        {
            fillGradient(position.x+size.width-scrollBarThickness, position.y, position.x+size.width, position.y+size.height, scrollBarBGColour, scrollBarBGColour)
            val s = getScrollBarRight
            fillGradient(s.x, s.y, s.x+s.width, s.y+s.height, scrollBarColour, scrollBarColour)
        }
        if (scrollBarHorizontal)
        {
            fillGradient(position.x, position.y+size.height-scrollBarThickness, position.x+size.width, position.y+size.height, scrollBarBGColour, scrollBarBGColour)
            val s = getScrollBarBelow
            fillGradient(s.x, s.y, s.x+s.width, s.y+s.height, scrollBarColour, scrollBarColour)
        }
    }

    def getScrollBarRight:Rect =
    {
        if (cFrame.size.height == 0) return Rect.zeroRect
        val sf = size.height/cFrame.height.toDouble
        val s = Size(scrollBarThickness, (size.height*sf).toInt)
        val p = Point(position.x+size.width-scrollBarThickness, ((position.y-cFrame.y)*sf).toInt)
        Rect(p, s)
    }

    def getScrollBarBelow:Rect =
    {
        if (cFrame.size.width == 0) return Rect.zeroRect
        val sf = size.width/cFrame.width.toDouble
        val s = Size((size.width*sf).toInt, scrollBarThickness)
        val p = Point(((position.x-cFrame.x)*sf).toInt, position.y+size.height-scrollBarThickness)
        Rect(p, s)
    }

    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean):Boolean =
    {
        def doRayTest(mode:Int) =
        {
            raytestMode = mode
            val hit = rayTest(p)
            raytestMode = 0
            hit
        }

        if (!consumed)
        {
            if (scrollBarVertical && doRayTest(1)) mouseDownRight = true
            else if (scrollBarHorizontal && doRayTest(2)) mouseDownBelow = true
            else if (doRayTest(3)) mouseDown = true
            else return false

            lastMousePos = p
            true
        }
        else false
    }

    override def mouseReleased_Impl(p:Point, button:Int, consumed:Boolean) =
    {
        mouseDown = false
        mouseDownRight = false
        mouseDownBelow = false
        false
    }

    override def traceHit(absPoint:Point) = raytestMode match
    {
        case 0 => mouseDown || mouseDownRight || mouseDownBelow
        case 1 => getScrollBarRight.contains(parent.convertPointFromScreen(absPoint))
        case 2 => getScrollBarBelow.contains(parent.convertPointFromScreen(absPoint))
        case 3 => dragTestFunction()
    }
}
