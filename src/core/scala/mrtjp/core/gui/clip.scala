/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import mrtjp.core.vec.{Point, Rect, Size}
import org.lwjgl.opengl.GL11

class ClipNode extends TNode
{
    var size = Size.zeroSize
    override def frame = Rect(position, size)

    override protected[gui] def drawBack(mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawBack_Impl(mouse, rframe)
                else
                {
                    onChildPredraw()
                    translateTo()
                    n.drawBack(dp, rframe)
                    translateFrom()
                    onChildPostdraw()
                }
            }
        }
    }

    override protected[gui] def drawFront(mouse:Point, rframe:Float)
    {
        if (!hidden)
        {
            val dp = mouse-position
            for (n <- familyByZ)
            {
                if (n == this) drawFront_Impl(mouse, rframe)
                else
                {
                    onChildPredraw()
                    translateTo()
                    n.drawFront(dp, rframe)
                    translateFrom()
                    onChildPostdraw()
                }
            }
        }
    }

    private def onChildPredraw()
    {
        val wScale = mcInst.getMainWindow.getScaledWidth
        val hScale = mcInst.getMainWindow.getScaledHeight

        val absPos = parent.convertPointToScreen(position)
        val sFrame = new Rect(absPos.x*wScale, mcInst.getMainWindow.getHeight-(absPos.y*hScale)-size.height*hScale,
            size.width*wScale, size.height*hScale)

        GL11.glEnable(GL11.GL_SCISSOR_TEST)
        GL11.glScissor(sFrame.x, sFrame.y, sFrame.width, sFrame.height)
    }

    private def onChildPostdraw()
    {
        GL11.glDisable(GL11.GL_SCISSOR_TEST)
    }

    override def traceHit(absPoint:Point) = !super.traceHit(absPoint)//only let hits within frame pass through

    override def mouseScrolled_Impl(p:Point, dir:Int, consumed:Boolean) = !frame.contains(p)
    override def mouseClicked_Impl(p:Point, button:Int, consumed:Boolean) = !frame.contains(p)
}

object ClipNode
{
    def tempDisableScissoring()
    {
        GL11.glPushAttrib(GL11.GL_SCISSOR_BIT)
        GL11.glDisable(GL11.GL_SCISSOR_TEST)
    }

    def tempEnableScissoring()
    {
        GL11.glPopAttrib()
    }
}
