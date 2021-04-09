/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import mrtjp.core.vec.{Point, Rect, Size}

/**
  * A simple node that lays out 6 buttons to select a side. Selected side will toggle on or off. You can choose
  * to allow multiple sides selected or only 1. This serves as a good example of how to create reusable
  * GUI layouts that you may need in many different places without having to build it piece by piece every time.
  *
  * @param x The top-left x position.
  * @param y The top-left y position.
  * @param w The width of the button layout of all buttons.
  * @param h The height of the button layout of all buttons.
  */
class SideSelectNode(x:Int, y:Int, w:Int, h:Int) extends TNode
{
    position = Point(x, y)

    /** Size of whole side select node. */
    val size = Size(w, h)
    override def frame = Rect(position, size)

    /**
      * Side mask of selected sides, where bit 1<<\s is set high if that side is selected. `s` represents an
      * `EnumFacing` index from 0 to 5.
      */
    var sides = 0

    /** If true, only 1 side can be selected at a time. */
    var exclusiveSides = false

    private val buttons = new Array[ButtonNode](6)

    {
        addChild(buildButton(0, 0, "u", 1))
        addChild(buildButton((w/5)*2, 0, "n", 2))
        addChild(buildButton(0, (h/5)*2, "w", 4))
        addChild(buildButton((w/5)*4, (h/5)*1*2, "e", 5))
        addChild(buildButton((w/5)*2, (h/5)*2*2, "s", 3))
        addChild(buildButton((w/5)*4, (h/5)*2*2, "d", 0))
    }

    private def buildButton(x:Int, y:Int, text:String, side:Int) =
    {
        val b = new MCButtonNode
        b.position = Point(x, y)
        b.size = size/3
        b.text = text
        b.clickDelegate = {() => onSidePresed(side)}
        buttons(side) = b
        b
    }

    /**
      * Called by child button nodes when they are pressed.
      * @param side The EnumFacing index of the side that the pressed button represents.
      */
    def onSidePresed(side:Int)
    {
        val old = sides
        sides ^= 1<<side
        if (exclusiveSides) sides &= 1<<side
        if (old != sides) onSideChanged(side)

        for (s <- 0 until 6)
            buttons(s).mouseoverLock = (sides&1<<s) != 0
    }

    /**
      * Called when the side mask changes.
      *
      * @param oldside The old side mask.
      * @todo Make this a delegate function instead of override point.
      */
    def onSideChanged(oldside:Int){}
}