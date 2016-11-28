package mrtjp.projectred.transmission

import codechicken.lib.lighting.LightModel
import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.render._
import codechicken.lib.render.pipeline.{ColourMultiplier, IVertexOperation}
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{IconTransformation, UVScale, UVTranslation}
import codechicken.microblock.{CommonMicroFactory, IMicroHighlightRenderer, MicroMaterialRegistry, MicroblockRender}
import codechicken.multipart.{BlockMultipart, PartRayTraceResult}
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.BlockRenderLayer
import net.minecraft.util.math.RayTraceResult
import org.lwjgl.opengl.GL11

object RenderFramedWire extends IMicroHighlightRenderer
{
    private val frameModels = FWireFrameModelGen.generateModels
    private val wireModels = new Array[CCModel](64*3)
    private val jacketModels = new Array[FWireJacketModel](64*3)

    private def modelKey(thickness:Int, connMap:Int):Int = connMap|thickness<<6
    private def modelKey(w:FramedWirePart):Int = modelKey(w.getThickness, w.connMap)

    private def getOrGenerateWireModel(key:Int) =
    {
        var m = wireModels(key)
        if (m == null) wireModels(key) =
            {m = FWireModelGen.generateWireModel(key); m}
        m
    }

    private def getOrGenerateJacketedModel(key:Int) =
    {
        var m = jacketModels(key)
        if (m == null) jacketModels(key) =
            {m = FWireModelGen.generateJacketedModel(key); m}
        m
    }

    def render(w:FramedWirePart, pos:Vector3, ccrs:CCRenderState)
    {
        val key = modelKey(w)
        val t = pos.translation()
        val uvt = new IconTransformation(w.getIcon)
        val m = ColourMultiplier.instance(w.renderHue)

        if (w.hasMaterial)
        {
            val jm = getOrGenerateJacketedModel(key)
            jm.renderWire(ccrs, t, uvt, m)
            jm.renderMaterial(pos, w.material, ccrs)
        }
        else
        {
            getOrGenerateWireModel(key).render(ccrs, t, uvt, m)
            renderWireFrame(key, ccrs, t, uvt)
        }
    }

    private def renderWireFrame(key:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        frameModels(6).render(ccrs, ops:_*)
        for (s <- 0 until 6) if ((key&1<<s) != 0) frameModels(s).render(ccrs, ops:_*)
    }

    def renderBreakingOverlay(icon:TextureAtlasSprite, wire:FramedWirePart, ccrs: CCRenderState)
    {
        ccrs.setPipeline(new Translation(wire.x, wire.y, wire.z), new IconTransformation(icon))
        import scala.collection.JavaConversions._
        for (box <- wire.getCollisionBoxes) BlockRenderer.renderCuboid(ccrs, box, 0)
    }

    def renderInv(thickness:Int, hue:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        getOrGenerateWireModel(modelKey(thickness, 0x3F)).render(ccrs, ops :+ ColourMultiplier.instance(hue):_*)
        renderWireFrame(modelKey(thickness, 0), ccrs, ops:_*)
    }

    def renderCoverHighlight(part:FramedWirePart, material:Int, ccrs:CCRenderState)
    {
        val pos = part.pos

        import net.minecraft.client.renderer.GlStateManager._

        pushMatrix()
        translate(pos.getX+0.5, pos.getY+0.5, pos.getZ+0.5)
        scale(1.002, 1.002, 1.002)
        translate(-0.5, -0.5, -0.5)

        enableBlend()
        depthMask(false)
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)

        ccrs.reset()
        TextureUtils.bindBlockTexture()
        ccrs.setBrightness(part.world, pos)
        ccrs.alphaOverride = 127
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)

        getOrGenerateJacketedModel(modelKey(part)).renderMaterial(Vector3.zero, material, ccrs)

        ccrs.draw()

        disableBlend()
        depthMask(true)
        popMatrix()
    }

    override def renderHighlight(player:EntityPlayer, hit:RayTraceResult, mcrFactory:CommonMicroFactory, size:Int, material:Int) =
    {
        val tile = BlockMultipart.getTile(player.worldObj, hit.getBlockPos)
        if (tile == null || mcrFactory.getFactoryID != 0 || size != 1 || player.isSneaking ||
                MicroMaterialRegistry.getMaterial(material).isTransparent) false
        else hit match {
            case prt:PartRayTraceResult => tile.partList(prt.partIndex) match {
                case fpart:FramedWirePart if !fpart.hasMaterial || fpart.material != material =>
                    RenderFramedWire.renderCoverHighlight(fpart, material, CCRenderState.instance())
                    true
                case _ => false
            }
            case _ => false
        }
    }
}

private object FWireFrameModelGen
{
    private val w = 2/8D
    private val d = 1/16D-0.002

    private var frameModels:Array[CCModel] = null

    def generateModels =
    {
        frameModels = new Array[CCModel](7)
        generateCenterModel()
        generateSideModels()
        finishModels()
        frameModels
    }

    private def generateCenterModel()
    {
        val model = CCModel.quadModel(48)
        model.verts(0) = new Vertex5(0.5-w, 0.5-w, 0.5-w, 20, 8)
        model.verts(1) = new Vertex5(0.5+w, 0.5-w, 0.5-w, 28, 8)
        model.verts(2) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 28, 0)
        model.verts(3) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0)
        model.verts(4) = new Vertex5(0.5-w, 0.5-w+d, 0.5+w, 20, 8)
        model.verts(5) = new Vertex5(0.5+w, 0.5-w+d, 0.5+w, 28, 8)
        model.verts(6) = new Vertex5(0.5+w, 0.5-w+d, 0.5-w, 28, 0)
        model.verts(7) = new Vertex5(0.5-w, 0.5-w+d, 0.5-w, 20, 0)
        model.generateSidedParts(0, Vector3.center)
        frameModels(6) = model
    }

    private def generateSideModels()
    {
        val model = CCModel.quadModel(36)
        model.verts(0) = new Vertex5(0.5-w, 0, 0.5+w, 16, 0)
        model.verts(1) = new Vertex5(0.5+w, 0, 0.5+w, 16, 8)
        model.verts(2) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 20, 8)
        model.verts(3) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 20, 0)
        model.verts(4) = new Vertex5(0.5+w, 0, 0.5+w-d, 16, 0)
        model.verts(5) = new Vertex5(0.5-w, 0, 0.5+w-d, 16, 8)
        model.verts(6) = new Vertex5(0.5-w, 0.5-w, 0.5+w-d, 20, 8)
        model.verts(7) = new Vertex5(0.5+w, 0.5-w, 0.5+w-d, 20, 0)

        for (r <- 1 until 4) model.apply(Rotation.quarterRotations(r).at(Vector3.center), 0, r*8, 8)

        model.verts(32) = new Vertex5(0.5-w, 0, 0.5-w, 24, 32)
        model.verts(33) = new Vertex5(0.5+w, 0, 0.5-w, 32, 32)
        model.verts(34) = new Vertex5(0.5+w, 0, 0.5+w, 32, 24)
        model.verts(35) = new Vertex5(0.5-w, 0, 0.5+w, 24, 24)
        frameModels(0) = model

        for (s <- 1 until 6)
        {
            frameModels(s) = model.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
            if (s%2 == 1)
            {
                val verts = frameModels(s).verts
                val t = new UVT(Rotation.quarterRotations(2).at(new Vector3(24, 0, 4)))
                for (i <- 0 until 32) verts(i).apply(t)
            }
        }
    }

    private def finishModels()
    {
        for (m <- frameModels)
        {
            m.apply(new UVScale(1/32D))
            m.shrinkUVs(0.0005)
            m.computeNormals
            m.computeLighting(LightModel.standardLightModel)
        }
    }
}

private object FWireModelGen
{
    var connMap = 0
    var tw = 0
    var w = 0.0D
    var connCount = 0
    var i = 0
    var model:CCModel = null

    def countConnections(mask:Int) =
    {
        var n = 0
        for (r <- 0 until 6) if ((mask&1<<r) != 0) n+=1
        n
    }

    private def setup(key:Int)
    {
        connMap = key&0x3F
        connCount = countConnections(connMap)
        val thickness = key>>6
        tw = thickness+1
        w = tw/16D+0.004
        i = 0
    }

    def generateWireModel(key:Int) =
    {
        setup(key)
        model = CCModel.quadModel(connCount*16+24)
        for (s <- 0 until 6) generateSide(s)
        finishModel()
        model
    }

    private def generateSide(s:Int)
    {
        val verts = connCount match
        {
            case 0 => generateStub(s)
            case 1  if (connMap&1<<(s^1)) != 0 => generateStub(s)
            case _ => generateSideFromType(s)
        }

        val t = AxisCycle.cycles(s/2).at(Vector3.center)
        for (vert <- verts) vert.apply(t)
        i = addVerts(model, verts, i)
    }

    private def generateStub(s:Int) =
    {
        val verts = faceVerts(s, 0.5-w)
        val t = new UVTranslation(12, 12)
        for (vert <- verts) vert.apply(t)
        verts
    }

    private def faceVerts(s:Int, d:Double) =
    {
        val verts = Array(
            new Vertex5(0.5-w, d, 0.5-w, 8-tw, 16+tw),
            new Vertex5(0.5+w, d, 0.5-w, 8+tw, 16+tw),
            new Vertex5(0.5+w, d, 0.5+w, 8+tw, 16-tw),
            new Vertex5(0.5-w, d, 0.5+w, 8-tw, 16-tw)
        )

        if (s%2 == 1)
        {
            val t = new Scale(1, -1, 1).at(Vector3.center)
            for (vert <- verts) vert.apply(t)
            reverseOrder(verts)
        }
        verts
    }

    private def generateSideFromType(s:Int) =
    {
        if ((connMap&1<<s) != 0) generateStraight(s)
        else generateFlat(s)
    }

    private val uvReflect = new UVT(new Scale(-1, 1, 1).at(new Vector3(8, 0, 16)))
    private def generateStraight(s:Int):Array[Vertex5] =
    {
        val verts = new Array[Vertex5](20)
        Array.copy(faceVerts(s, 0), 0, verts, 0, 4)

        if (s%2 == 0)
        {
            verts(4) = new Vertex5(0.5-w, 0, 0.5+w, 8-tw, 24)
            verts(5) = new Vertex5(0.5+w, 0, 0.5+w, 8+tw, 24)
            verts(6) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 8+tw, 16+tw)
            verts(7) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 8-tw, 16+tw)
        }
        else
        {
            verts(4) = new Vertex5(0.5-w, 0.5+w, 0.5+w, 8-tw, 16-tw)
            verts(5) = new Vertex5(0.5+w, 0.5+w, 0.5+w, 8+tw, 16-tw)
            verts(6) = new Vertex5(0.5+w, 1, 0.5+w, 8+tw, 8)
            verts(7) = new Vertex5(0.5-w, 1, 0.5+w, 8-tw, 8)
        }
        for (r <- 1 until 4)
        {
            val t = Rotation.quarterRotations(r).at(Vector3.center)
            for (i <- 0 until 4)
            {
                verts(i+r*4+4) = verts(i+4).copy.apply(t)
                if (r >= 2) verts(i+r*4+4).apply(uvReflect)
            }
        }
        val t = new UVTranslation(12, 12)
        for (i <- 0 until 4) verts(i).apply(t)
        verts
    }

    private def generateFlat(s:Int):Array[Vertex5] =
    {
        val verts = faceVerts(s, 0.5-w)
        var fConnMask = 0
        for (i <- 0 until 4)
        {
            val absSide = ((s&6)+i+2)%6
            if ((connMap&1<<absSide) != 0) fConnMask |= 1<<i
        }

        val rot =
            if ((fConnMask&0xC) == 0) 0
            else if ((fConnMask&3) == 0) 1
            else 2

        val uvt = rot match
        {
            case 1 => new UVT(Rotation.quarterRotations(1).at(new Vector3(8, 0, 16)))
            case 2 => new UVT(Rotation.quarterRotations(1).at(new Vector3(8, 0, 16)).`with`(new Translation(16, 0, 0)))
            case _ => null
        }

        if (uvt != null) for (vert <- verts) vert.apply(uvt)
        verts
    }

    def generateJacketedModel(key:Int) =
    {
        setup(key)
        new FWireJacketModel(generateJacketedWireModel, generateJacketedBoxes)
    }

    private def generateJacketedWireModel:CCModel =
    {
        val n = connCount match
        {
            case 0 => 6
            case 1 => 2
            case _ => connCount
        }

        model = CCModel.quadModel(n*4)
        for (s <- 0 until 6) generateJacketedSide(s)
        finishModel()
        model
    }

    private def generateJacketedSide(s:Int)
    {
        val d =
            if ((connMap&1<<s) != 0) 0.00D
            else if (connCount == 0) 0.25D
            else if (connCount == 1 && (connMap&1<<(s^1)) != 0) 0.25D
            else return

        val verts = faceVerts(s, d-0.002)
        val t = AxisCycle.cycles(s/2).at(Vector3.center)
        val uvt = new UVTranslation(12, 12)
        for (vert <- verts)
        {
            vert.apply(t)
            vert.apply(uvt)
        }
        i = addVerts(model, verts, i)
    }

    private def generateJacketedBoxes:Array[IndexedCuboid6] =
    {
        if (connCount == 0) return Array(new IndexedCuboid6(0, WireBoxes.fOBounds(6)))

        var n = 0
        for (a <- 0 until 3) if ((connMap&3<<a*2) != 0) n+=1

        val boxes = new Array[IndexedCuboid6](n)
        i = 0

        var first = true
        for (a <- 0 until 3) first = !generateAxialJacketBoxes(a, first, boxes)

        boxes
    }

    private def generateAxialJacketBoxes(a:Int, first:Boolean, boxes:Array[IndexedCuboid6]):Boolean =
    {
        import WireBoxes.fOBounds

        val mask = connMap>>a*2&3
        if (mask == 0) return false

        val box = mask match
        {
            case 1 => fOBounds(0).copy
            case 2 => fOBounds(1).copy
            case _ =>
                val b = fOBounds(0).copy
                b.max.y = 1
                b
        }

        box.apply(Rotation.sideRotations(a*2).at(Vector3.center))
        if (first) box.enclose(fOBounds(6))

        val fMask =
            if (first || mask == 3) 0
            else if (mask == 1) 1<<2*a+1
            else 1<<2*a

        boxes(i) = new IndexedCuboid6(fMask, box)
        i += 1
        true
    }

    private def reverseOrder(verts:Array[Vertex5])
    {
        var k = 0
        while (k < verts.length)
        {
            val tmp = verts(k+1)
            verts(k+1) = verts(k+3)
            verts(k+3) = tmp
            k += 4
        }
    }

    /**
     * Puts verts into model m starting at index k
     */
    private def addVerts(m:CCModel, verts:Array[Vertex5], k:Int) =
    {
        for (i <- 0 until verts.length) m.verts(k+i) = verts(i)
        k+verts.length
    }

    private def finishModel()
    {
        model.apply(new UVScale(1/32D))
        model.shrinkUVs(0.0005)
        model.computeNormals
        model.computeLighting(LightModel.standardLightModel)
    }
}

class FWireJacketModel(wire:CCModel, boxes:Array[IndexedCuboid6])
{
    def renderWire(ccrs:CCRenderState, ops:IVertexOperation*)
    {
        wire.render(ccrs, ops:_*)
    }

    def renderMaterial(vec:Vector3, mat:Int, ccrs:CCRenderState)
    {
        val material = MicroMaterialRegistry.getMaterial(mat)
        for (b <- boxes) MicroblockRender.renderCuboid(vec, ccrs, material, BlockRenderLayer.SOLID, b, b.data.asInstanceOf[Int])
    }
}
