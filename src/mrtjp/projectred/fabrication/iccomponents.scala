/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.math.MathHelper
import codechicken.lib.render.uv.{IconTransformation, UVTransformation}
import codechicken.lib.render.{CCModel, CCRenderState, ColourMultiplier, TextureUtils}
import codechicken.lib.vec._
import mrtjp.core.color.Colors
import mrtjp.core.vec.Size
import mrtjp.projectred.integration.ComponentStore
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.util.IIcon

object ICComponentStore
{
    val faceModels =
    {
        val m = CCModel.quadModel(4)
        m.generateBlock(0, new Cuboid6(0, 0, 0, 1, 0, 1), ~1)
        m.computeNormals
        m.shrinkUVs(0.0005)
        Seq(m, m.backfacedCopy)
    }

    var pref:IIcon = null
    var prefCorner:IIcon = null
    var prefEdge:IIcon = null

    var torchOffIcon:IIcon = null
    var torchOnIcon:IIcon = null

    val redwireIcons = new Array[IIcon](16)
    val insulatedwireIcons = new Array[IIcon](16)
    val bundledwireIcons = new Array[IIcon](16)
    var bundledColourIcon:IIcon = null

    var ioBorder:IIcon = null
    var ioSig:IIcon = null
    var leverOnIcon:IIcon = null
    var leverOffIcon:IIcon = null
    var redChipOnIcon:IIcon = null
    var redChipOffIcon:IIcon = null
    var yellowChipOnIcon:IIcon = null
    var yellowChipOffIcon:IIcon = null
    var pointerIcon:IIcon = null

    def registerIcons(reg:IIconRegister)
    {
        val baseTex = "projectred:circuits/"
        def register(path:String) = reg.registerIcon(baseTex+path)

        pref = register("prefboard")
        prefCorner = register("prefboardcorner")
        prefEdge = register("prefboardedge")

        for (i <- 0 until 16) redwireIcons(i) = register("alloywire/conn"+i)
        for (i <- 0 until 16) insulatedwireIcons(i) = register("insulatedwire/conn"+i)
        for (i <- 0 until 16) bundledwireIcons(i) = register("bundledcable/conn"+i)
        bundledColourIcon = register("bundledcable/col")

        torchOffIcon = register("torchoff")
        torchOnIcon = register("torchon")

        RenderICGate.registerIcons(reg)

        for (m <- WireModel.wireModels) m.icon = register(m.iconPath)
        for (m <- BaseComponentModel.baseModels) m.icon = register(m.iconPath+"/base")

        ioBorder = register("IOSIMP/border")
        ioSig = register("IOSIMP/signal")
        leverOffIcon = register("TOGLATCH/lever_off")
        leverOnIcon = register("TOGLATCH/lever_on")
        redChipOnIcon = register("redchip_on")
        redChipOffIcon = register("redchip_off")
        yellowChipOnIcon = register("yellowchip_on")
        yellowChipOffIcon = register("yellowchip_off")
        pointerIcon = register("pointer")
    }

    def prepairRender()
    {
        TextureUtils.bindAtlas(0)
        CCRenderState.reset()
        CCRenderState.startDrawing()
        CCRenderState.pullLightmap()
        CCRenderState.setDynamic()
    }

    def finishRender()
    {
        CCRenderState.draw()
    }

    def generateWireModels(name:String, count:Int) =
    {
        val xs = Seq.newBuilder[WireModel]
        for (i <- 0 until count)
            xs += new WireModel(name+"/"+name+"-"+i)
        xs.result()
    }

    def orthoGridT(xSize:Double, ySize:Double) =
        new Scale(xSize, 1, -ySize) `with` new Rotation(0.5*MathHelper.pi, 1, 0, 0)

    def orthoPartT(x:Double, y:Double, xSize:Double, ySize:Double, boardSize:Size, xPoint:Int, yPoint:Int) =
        new TransformationList(
            new Scale(1.0/boardSize.width, 1, 1.0/boardSize.height),
            new Translation(new Vector3(xPoint, 0, yPoint).multiply(1.0/boardSize.width, 0, 1.0/boardSize.height)),
            orthoGridT(xSize, ySize),
            new Translation(x, y, 0)
        )

    def dynamicT(orient:Int) =
        if (orient == 0) new RedundantTransformation
        else new Scale(-1, 0, 1).at(Vector3.center)

    def dynamicIdx(orient:Int, ortho:Boolean) = orient^(if (ortho) 1 else 0)

    def signalColour(signal:Byte) = (signal&0xFF)/2+60<<24|0xFF
}

import mrtjp.projectred.fabrication.ICComponentStore._

abstract class ICComponentModel
{
    def renderModel(t:Transformation, orient:Int, ortho:Boolean)
}

abstract class SingleComponentModel(pos:Vector3 = Vector3.zero) extends ICComponentModel
{
    val models = faceModels.map(_.copy.apply(new Translation(-0.5, 0, -0.5)).apply(pos.copy.multiply(1/16D).translation))

    def getUVT:UVTransformation

    override def renderModel(t:Transformation, orient:Int, ortho:Boolean)
    {
        models(dynamicIdx(orient, ortho)).render(dynamicT(orient) `with` t, getUVT)
    }
}

abstract class OnOffModel(pos:Vector3 = Vector3.zero) extends SingleComponentModel(pos)
{
    var on = false

    def getIcons:Seq[IIcon]

    override def getUVT = new IconTransformation(getIcons(if (on) 1 else 0))
}

object BaseComponentModel
{
    var baseModels = Seq[BaseComponentModel]()
}

class BaseComponentModel(val iconPath:String) extends SingleComponentModel(new Vector3(8, 0, 8))
{
    BaseComponentModel.baseModels :+= this

    var icon:IIcon = null
    override def getUVT = new IconTransformation(icon)
}

class RedstoneTorchModel(x:Double, z:Double) extends OnOffModel(new Vector3(x, 0, z))
{
    override def getIcons = Seq(torchOffIcon, torchOnIcon)
}

object WireModel
{
    var wireModels = Seq[WireModel]()
}

class WireModel(val iconPath:String) extends ICComponentModel
{
    WireModel.wireModels :+= this

    var on = false
    var disabled = false

    var icon:IIcon = null

    var onColour = 187<<24|0xFF
    var offColour = 60<<24|0xFF
    var disabledColour = Colors.GREY.rgba

    override def renderModel(t:Transformation, orient:Int, ortho:Boolean)
    {
        faceModels(dynamicIdx(orient, ortho)).render(dynamicT(orient) `with` t, new IconTransformation(icon),
            ColourMultiplier.instance(if (disabled) disabledColour else if (on) onColour else offColour))
    }
}

class IOSigModel extends ICComponentModel
{
    var on = false
    var colour = 0

    override def renderModel(t:Transformation, orient:Int, ortho:Boolean)
    {
        val m = faceModels(dynamicIdx(orient, ortho))
        val t0 = dynamicT(orient) `with` t
        m.render(t0, new IconTransformation(ioBorder), ColourMultiplier.instance(colour))
        m.render(t0, new IconTransformation(ioSig), ColourMultiplier.instance(signalColour(if (on) 255.toByte else 0.toByte)))
    }
}

class LeverModel(x:Double, z:Double) extends OnOffModel(new Vector3(x, 0, z))
{
    override def getIcons = Seq(leverOffIcon, leverOnIcon)
}

class RedChipModel(x:Double, z:Double) extends OnOffModel(new Vector3(x, 0, z))
{
    override def getIcons = Seq(redChipOffIcon, redChipOnIcon)
}

class YellowChipModel(x:Double, z:Double) extends OnOffModel(new Vector3(x, 0, z))
{
    override def getIcons = Seq(yellowChipOffIcon, yellowChipOnIcon)
}

class PointerModel(x:Double, z:Double, scale:Double = 1) extends ICComponentModel
{
    val pos = new Vector3(x, 0, z).multiply(1/16D)
    val models = faceModels.map(_.copy.apply(new Translation(-0.5, 0, -0.5)).apply(new Scale(scale, 1, scale)))

    var angle = 0.0

    override def renderModel(t:Transformation, orient:Int, ortho:Boolean)
    {
        models(dynamicIdx(orient, ortho)).render(new Rotation(-angle, 0, 1, 0) `with` pos.translation
                `with` dynamicT(orient) `with` t, new IconTransformation(pointerIcon))
    }
}