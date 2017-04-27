/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.{Rotation, Transformation, Translation, Vector3}
import mrtjp.core.vec.{Point, Vec2}
import mrtjp.projectred.fabrication.TileEditorOp._
import mrtjp.projectred.fabrication.ICComponentStore._
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

abstract class OpGateCommons(meta:Int) extends TileEditorOp
{
    def canPlace(editor:ICTileMapEditor, point:Point):Boolean
    def findRot(editor:ICTileMapEditor, start:Point, end:Point):Int

    override def checkOp(editor:ICTileMapEditor, start:Point, end:Point) =
        canPlace(editor, start) && editor.getPart(start) == null

    override def writeOp(editor:ICTileMapEditor, start:Point, end:Point, out:MCDataOutput)
    {
        out.writeByte(start.x).writeByte(start.y)
        out.writeByte(findRot(editor, start, end))
    }

    override def readOp(editor:ICTileMapEditor, in:MCDataInput)
    {
        val point = Point(in.readByte(), in.readByte())
        val r = in.readUByte()

        if (editor.getPart(point) == null && canPlace(editor, point)) {
            val part = ICTile.createTile(ICGateDefinition(meta).gateType).asInstanceOf[GateICTile]
            part.preparePlacement(r, meta)
            editor.setPart(point, part)
        }
    }

    @SideOnly(Side.CLIENT)
    override def renderHover(ccrs:CCRenderState, editor:ICTileMapEditor, point:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getPart(point) != null) return

        val t = orthoPartT(x, y, xSize, ySize, editor.size, point.x, point.y)
        doRender(ccrs, t, findRot(editor, point, point))

        renderHolo(x, y, xSize,  ySize, editor.size, point,
            if (canPlace(editor, point)) 0x33FFFFFF else 0x33FF0000)
    }

    @SideOnly(Side.CLIENT)
    override def renderDrag(ccrs:CCRenderState, editor:ICTileMapEditor, start:Point, end:Point, x:Double, y:Double, xSize:Double, ySize:Double)
    {
        if (editor.getPart(start) != null) return

        val t = orthoPartT(x, y, xSize, ySize, editor.size, start.x, start.y)
        doRender(ccrs, t, findRot(editor, start, end))

        renderHolo(x, y, xSize,  ySize, editor.size, start,
            if (canPlace(editor, start)) 0x44FFFFFF else 0x44FF0000)
    }

    @SideOnly(Side.CLIENT)
    override def renderImage(ccrs:CCRenderState, x:Double, y:Double, width:Double, height:Double)
    {
        val t = orthoGridT(width, height) `with` new Translation(x, y, 0)
        doRender(ccrs, t, 0)
    }

    @SideOnly(Side.CLIENT)
    def doRender(ccrs:CCRenderState, t:Transformation, rot:Int)
    {
        RenderICGate.renderInv(ccrs, Rotation.quarterRotations(rot).at(Vector3.center) `with` t, meta)
    }

    @SideOnly(Side.CLIENT)
    override def getOpName = ICGateDefinition(meta).unlocal
}

class OpGate(meta:Int) extends OpGateCommons(meta)
{
    override def findRot(editor:ICTileMapEditor, start:Point, end:Point) =
    {
        (end-start).vectorize.axialProject.normalize match {
            case Vec2( 0,-1) => 0
            case Vec2( 1, 0) => 1
            case Vec2( 0, 1) => 2
            case Vec2(-1, 0) => 3
            case _ => 0
        }
    }

    override def canPlace(editor:ICTileMapEditor, point:Point) =
        !isOnBorder(editor.size, point)
}

class OpIOGate(meta:Int) extends OpGateCommons(meta)
{
    override def canPlace(editor:ICTileMapEditor, point:Point) =
        isOnBorder(editor.size, point) && !isOnCorner(editor.size, point)

    override def findRot(editor:ICTileMapEditor, start:Point, end:Point) =
    {
        val wm = editor.size.width-1
        val hm = editor.size.height-1
        start match {
            case Point(_, 0)    => 0
            case Point(`wm`, _) => 1
            case Point(_, `hm`) => 2
            case Point(0, _)    => 3
            case _              => 0
        }
    }
}