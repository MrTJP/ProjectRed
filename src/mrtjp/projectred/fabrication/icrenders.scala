/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.render.uv.{IconTransformation, UVScale}
import codechicken.lib.render.{CCModel, CCRenderState, ColourMultiplier}
import codechicken.lib.vec._
import mrtjp.core.color.Colors
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.util.ResourceLocation
import org.lwjgl.opengl.GL11
import GL11._

import scala.collection.JavaConversions._

object RenderCircuit
{
    def registerIcons(reg:IIconRegister)
    {
        ICComponentStore.registerIcons(reg)
    }

    def renderOrtho(circuit:IntegratedCircuit, x:Double, y:Double, xSize:Double, ySize:Double, frame:Float)
    {
        val t = ICComponentStore.orthoGridT(xSize, ySize) `with` new Translation(x, y, 0)
        renderBoard(circuit, t, true)
        renderCircuit(circuit, t, true, frame)
    }

    def renderDynamic(circuit:IntegratedCircuit, t:Transformation, frame:Float)
    {
        glDisable(GL_DEPTH_TEST)
        renderBoard(circuit, t, true)
        renderCircuit(circuit, t, true, frame)
        glEnable(GL_DEPTH_TEST)
    }

    def renderBoard(circuit:IntegratedCircuit, t:Transformation, ortho:Boolean)
    {
        PrefboardRenderer.render(circuit, t, ortho)
    }

    def renderCircuit(circuit:IntegratedCircuit, t:Transformation, ortho:Boolean, frame:Float)
    {
        for (((x, y), part) <- circuit.parts)
        {
            val tlist = new TransformationList(
                new Scale(1.0/circuit.size.width, 1, 1.0/circuit.size.height),
                new Translation(x*1.0/circuit.size.width, 0, y*1.0/circuit.size.height),
                t
            )
            part.renderDynamic(tlist, ortho, frame)
        }
    }
}

import mrtjp.projectred.fabrication.ICComponentStore._
object PrefboardRenderer
{
    private var boardModels = Map[(Int, Int), Seq[CCModel]]()
    private var cornerModels = Map[(Int, Int), Seq[CCModel]]()
    private var edgeModels = Map[(Int, Int), Seq[CCModel]]()

    private def createBoardModel(w:Int, h:Int):Seq[CCModel] =
        faceModels.map(_.copy.apply(new UVScale(w, h)))

    private def createCornerModel(w:Int, h:Int):Seq[CCModel] =
    {
        val corners = Seq((0, 0), (0, h-1), (w-1, h-1), (w-1, 0)).map
        { pair =>
            new TransformationList(
                new Scale(1.0/w, 1, 1.0/h),
                new Translation(pair._1*1.0/w, 0, pair._2*1.0/h)
            )
        }

        faceModels.map
        { m =>
            var models = Seq[CCModel]()
            for (t <- corners)
                models :+= m.copy.apply(t)
            CCModel.combine(models)
        }
    }

    private def createEdgeModel(w:Int, h:Int):Seq[CCModel] =
    {
        val edges = Seq((0, 0, 1, h), (0, 0, w, 1), (w-1, 0, 1, h), (0, h-1, w, 1)).map
        { pair =>
            (new TransformationList(
                new Scale(1.0/w, 1, 1.0/h),
                new Scale(pair._3, 1, pair._4),
                new Translation(pair._1*1.0/w, 0, pair._2*1.0/h)
            ), new UVScale(pair._3, pair._4))
        }

        faceModels.map
        { m =>
            var models = Seq[CCModel]()
            for ((t, uvt) <- edges)
                models :+= m.copy.apply(t).apply(uvt)
            CCModel.combine(models)
        }
    }

    private def getBoardModel(w:Int, h:Int) =
    {
        if (!boardModels.contains((w, h)))
            boardModels += (w, h) -> createBoardModel(w, h)
        boardModels((w, h))
    }

    private def getCornerModel(w:Int, h:Int) =
    {
        if (!cornerModels.contains((w, h)))
            cornerModels+= (w, h) -> createCornerModel(w, h)
        cornerModels((w, h))
    }

    private def getEdgeModel(w:Int, h:Int) =
    {
        if (!edgeModels.contains((w, h)))
            edgeModels+= (w, h) -> createEdgeModel(w, h)
        edgeModels((w, h))
    }

    def render(circuit:IntegratedCircuit, t:Transformation, ortho:Boolean)
    {
        val w = circuit.size.width
        val h = circuit.size.height

        def bind(s:String)
        {
            val r = new ResourceLocation("projectred", "textures/blocks/fabrication/"+s+".png")
            Minecraft.getMinecraft.getTextureManager.bindTexture(r)
        }

        CCRenderState.reset()
        CCRenderState.pullLightmap()
        CCRenderState.setDynamic()

        for ((tex, models) <- Seq(("prefboard", getBoardModel(w, h)),
            ("prefboard_edge", getEdgeModel(w, h)), ("prefboard_corner", getCornerModel(w, h))))
        {
            bind(tex)
            CCRenderState.startDrawing()
            models(if (ortho) 1 else 0).render(t)
            CCRenderState.draw()
        }
    }
}

object RenderICAlloyWire
{
    var connMap:Byte = 0
    var signal:Byte = 0

    def prepairInv()
    {
        connMap = 0xF
        signal = 0xFF.toByte
    }

    def prepairDynamic(part:AlloyWireICPart)
    {
        connMap = part.connMap
        signal = part.signal
    }

    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(redwireIcons(connMap&0xFF)),
            ColourMultiplier.instance((signal&0xFF)/2+60<<24|0xFF))
        finishRender()
    }
}

object RenderICInsulatedWire
{
    var connMap:Byte = 0
    var signal:Byte = 0
    var colour:Byte = 0

    def prepairInv(c:Int)
    {
        connMap = 0xF
        signal = 255.toByte
        colour = c.toByte
    }

    def prepairDynamic(part:InsulatedWireICPart)
    {
        connMap = part.connMap
        signal = part.signal
        colour = part.colour
    }

    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(redwireIcons(connMap&0xFF)),
            ColourMultiplier.instance((signal&0xFF)/2+60<<24|0xFF))
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(insulatedwireIcons(connMap&0xFF)),
            ColourMultiplier.instance(Colors(colour&0xFF).rgba))
        finishRender()
    }
}

object RenderICBundledCable
{
    var connMap:Byte = 0
    var colour:Byte = 0

    def prepairInv(c:Int)
    {
        connMap = 0xF
        colour = c.toByte
    }

    def prepairDynamic(part:BundledCableICPart)
    {
        connMap = part.connMap
        colour = part.colour
    }

    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(bundledwireIcons(connMap&0xFF)))
        if (colour != -1) faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(bundledColourIcon),
            ColourMultiplier.instance(Colors(colour&0xFF).rgba))
        finishRender()
    }
}

object RenderICTorch
{
    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(torchOnIcon))
        finishRender()
    }
}

object RenderICLever
{
    var on = false

    def prepairInv()
    {
        on = false
    }

    def prepairDynamic(part:LeverICPart)
    {
        on = part.on
    }

    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(if (on) leverOnIcon else leverOffIcon))
        finishRender()
    }
}

object RenderICButton
{
    var on = false

    def prepairInv()
    {
        on = false
    }

    def prepairDynamic(part:ButtonICPart)
    {
        on = part.on
    }

    def render(t:Transformation, ortho:Boolean)
    {
        prepairRender()
        faceModels(dynamicIdx(0, ortho)).render(t, new IconTransformation(if (on) buttonOnIcon else buttonOffIcon))
        finishRender()
    }
}