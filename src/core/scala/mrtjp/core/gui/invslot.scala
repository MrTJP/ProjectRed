/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import mrtjp.core.vec.{Point, Rect, Size}

class InventorySlotNode extends TNode
{
    var slotIdx = -1

    var size = Size(16, 16)
    override def frame = Rect(position, size)

    override def frameUpdate_Impl(mouse:Point, rframe:Float)
    {
        val root = getRoot
        val slot = root.getContainer.asInstanceOf[NodeContainer].inventorySlots.get(slotIdx)

        if (hidden || buildParentHierarchy(root).exists(_.hidden))
        {
//            slot.xPos = 9999
//            slot.yPos = 9999
        }
        else
        {
            val absPos = parent.convertPointTo(position, root)
//            slot.xPos = absPos.x
//            slot.yPos = absPos.y
        }
    }
}
