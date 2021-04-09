/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.Translation
import codechicken.lib.vec.uv.{UVScale, UVTranslation}
import mrtjp.core.vec.Point
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TileEditorOpErase extends TileEditorOp
{
    override def checkOp(editor:ICTileMapEditor, start:Point, end:Point) = true

    override def writeOp(editor:ICTileMapEditor, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(start.x).writeByte(start.y)
        out.writeByte(end.x).writeByte(end.y)
    }

    override def readOp(editor:ICTileMapEditor, in:MCDataInput)
    {
        val start = Point(in.readUByte(), in.readUByte())
        val end = Point(in.readUByte(), in.readUByte())

        for (x <- math.min(start.x, end.x) to math.max(start.x, end.x))
            for (y <- math.min(start.y, end.y) to math.max(start.y, end.y))
                editor.removeTile(Point(x, y))
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(ccrs:CCRenderState, x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)

        prepairRender(ccrs)
        TextureUtils.changeTexture(GuiICWorkbench.background)
        faceModels(dynamicIdx(0, true)).render(ccrs, t, new UVScale(16) `with` new UVTranslation(330, 18) `with` new UVScale(1/512D))
        finishRender(ccrs)
    }

    @SideOnly(Side.CLIENT)
    override def renderHover(ccrs:CCRenderState, editor:ICTileMapEditor, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getTile(point) != null)
            TileEditorOp.renderHolo(x, y, xSize, ySize, editor.size, point, 0x33FF0000)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(ccrs:CCRenderState, editor:ICTileMapEditor, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        for (px <- math.min(start.x, end.x) to math.max(start.x, end.x))
            for (py <- math.min(start.y, end.y) to math.max(start.y, end.y))
            {
                val point = Point(px, py)
                TileEditorOp.renderHolo(x, y, xSize, ySize, editor.size, point,
                    if (editor.getTile(point) != null) 0x44FF0000 else 0x44FFFFFF)
            }
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = "Erase"
}