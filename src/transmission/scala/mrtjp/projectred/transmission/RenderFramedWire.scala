package mrtjp.projectred.transmission


import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.render._
import codechicken.lib.render.lighting.LightModel
import codechicken.lib.render.pipeline.{ColourMultiplier, IVertexOperation}
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{IconTransformation, UV, UVScale, UVTranslation}
import codechicken.microblock.api.MicroMaterial
import codechicken.microblock.{CommonMicroFactory, IMicroHighlightRenderer, MicroblockRender}
import codechicken.multipart.block.BlockMultiPart
import codechicken.multipart.util.PartRayTraceResult
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.core.{PRLib, UVT}
import mrtjp.projectred.transmission.WireBoxes.fOBounds
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderType}
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.util.Hand
import net.minecraft.util.math.BlockRayTraceResult

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
            {m = FWireModelGen.instance.generateWireModel(key); m}
        m
    }

    private def getOrGenerateJacketedModel(key:Int) =
    {
        var m = jacketModels(key)
        if (m == null) jacketModels(key) =
            {m = FWireModelGen.instance.generateJacketedModel(key); m}
        m
    }

    def render(w:FramedWirePart, ccrs:CCRenderState)
    {
        val key = modelKey(w)
        val uvt = new IconTransformation(w.getIcon)
        val m = ColourMultiplier.instance(w.renderHue)

        if (w.material != null)
        {
            val jm = getOrGenerateJacketedModel(key)
            jm.renderWire(ccrs, uvt, m)
            jm.renderMaterial(w.material, ccrs, false)
        }
        else
        {
            getOrGenerateWireModel(key).render(ccrs, uvt, m)
            renderWireFrame(key, ccrs, uvt)
        }
    }

    private def renderWireFrame(key:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        frameModels(6).render(ccrs, ops:_*)
        for (s <- 0 until 6) if ((key&1<<s) != 0) frameModels(s).render(ccrs, ops:_*)
    }

    def renderInv(thickness:Int, hue:Int, ccrs:CCRenderState, ops:IVertexOperation*)
    {
        getOrGenerateWireModel(modelKey(thickness, 0x3C)).render(ccrs, ops :+ ColourMultiplier.instance(hue):_*)
        renderWireFrame(modelKey(thickness, 0), ccrs, ops:_*)
    }

    def renderCoverHighlight(part:FramedWirePart, material:MicroMaterial, ccrs:CCRenderState, mStack: MatrixStack, getter: IRenderTypeBuffer)
    {
        val pos = part.pos

        val mat = new Matrix4(mStack)
        mat.translate(pos)
        mat.apply(new Scale(1.002, 1.002, 1.002).at(Vector3.CENTER))

        ccrs.reset()
        ccrs.bind(MicroblockRender.highlighRenderType, getter, mat)
        ccrs.alphaOverride = 127
        getOrGenerateJacketedModel(modelKey(part)).renderHighlight(material, ccrs, true)
    }

    override def renderHighlight(player: PlayerEntity, hand: Hand, hit: BlockRayTraceResult, mcrFactory: CommonMicroFactory, size: Int, material: MicroMaterial, mStack: MatrixStack, getter: IRenderTypeBuffer, partialTicks: Float) =
    {
        val tile = BlockMultiPart.getTile(player.level, hit.getBlockPos)
        if (tile == null || mcrFactory.getFactoryID != 0 || size != 1 || player.isCrouching ||
            material.isTransparent) false
        else hit match {
            case prt:PartRayTraceResult => prt.part match {
                case fpart:FramedWirePart if fpart.material == null || fpart.material != material =>
                    RenderFramedWire.renderCoverHighlight(fpart, material, CCRenderState.instance(), mStack, getter)
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
        model.generateSidedParts(0, Vector3.CENTER)
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

        for (r <- 1 until 4) model.apply(Rotation.quarterRotations(r).at(Vector3.CENTER), 0, r*8, 8)

        model.verts(32) = new Vertex5(0.5-w, 0, 0.5-w, 24, 32)
        model.verts(33) = new Vertex5(0.5+w, 0, 0.5-w, 32, 32)
        model.verts(34) = new Vertex5(0.5+w, 0, 0.5+w, 32, 24)
        model.verts(35) = new Vertex5(0.5-w, 0, 0.5+w, 24, 24)
        frameModels(0) = model

        for (s <- 1 until 6)
        {
            frameModels(s) = model.copy.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
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

object FWireModelGen
{
    val instances = new ThreadLocal[FWireModelGen] {
        override def initialValue() = new FWireModelGen
    }

    def instance = instances.get()
}

class FWireModelGen
{
    var connMap = 0
    var tw = 0
    var w = 0.0D
    var connCount = 0 // Number of sided connections
    var axisCount = 0 // Number of connection axis used
    var fRotationMasks = new Array[Int](6) // Rotation masks for each face
    var fAxisCounts = new Array[Int](6) // Axis count for each face
    var i = 0
    var model:CCModel = null

    def countConnections(mask:Int) =
    {
        var n = 0
        for (r <- 0 until 6) if ((mask&1<<r) != 0) n+=1
        n
    }

    private def countAxis(mask:Int):Int = {
        var n = 0
        for (a <- 0 until 3) if ((mask&0x3<<(a*2)) != 0) n += 1
        n
    }

    private def calcFaceRotationMask(connMap:Int, s:Int):Int = {
        var rMask = 0
        for (r <- 0 until 4) {
            val absSide = Rotation.rotateSide(s, (r+2)%4)
            if ((connMap&1<<absSide) != 0) rMask |= 1<<r
        }
        rMask
    }

    private def countFaceAxis(fRotMask:Int):Int = {
        var a = 0
        if ((fRotMask&0x5) != 0) a += 1
        if ((fRotMask&0xA) != 0) a += 1
        a
    }

    private def setup(key:Int)
    {
        connMap = key&0x3F
        connCount = countConnections(connMap)
        axisCount = countAxis(connMap)
        for (s <- 0 until 6) fRotationMasks(s) = calcFaceRotationMask(connMap, s)
        for (s <- 0 until 6) fAxisCounts(s) = countFaceAxis(fRotationMasks(s))
        val thickness = key>>6
        tw = thickness+1
        w = tw/16D+0.004
        i = 0
    }

    def generateWireModel(key:Int) =
    {
        setup(key)

        val axisVertCount = axisCount * 16 // Each axis (up/down, north/south, west/east) require 4 faces each
        val stubVertCount = fAxisCounts.count(_ == 0) * 4 // Stubs when no rotational conns on a face
        val circleVertCount = fAxisCounts.count(_ == 2) * 4 // Circular cross texture on faces with both vertical and horizontal axis
        val capVertCount = connCount * 4 // Cap at end of connections reaching out of the block

        model = CCModel.quadModel(axisVertCount + stubVertCount + circleVertCount + capVertCount)
        for (s <- 0 until 6) generateFace(s)
        finishModel()
        model
    }

    private def generateFace(s:Int):Unit = {
        val vb = Array.newBuilder[Vertex5]

        vb ++= generateFaceAxisVerts(s) // Verts for conns perpendicular to 's' axis

        fAxisCounts(s) match {
            case 0 => vb ++= generateFaceStubVerts(s, 0.5-w) // caps at center
            case 2 => vb ++= generateFaceCircleVerts(s) // circular texture showing crossed axis
            case _ =>
        }

        if ((connMap&1<<(s^1)) != 0) // Verts for caps
            vb ++= generateFaceStubVerts(s, 0)

        val verts = vb.result()

        val t = Rotation.sideOrientation(s, 0).at(Vector3.CENTER)
        for (v <- verts) v.apply(t)
        i = addVerts(model, verts, i)
    }

    private def generateFaceAxisVerts(s:Int):Array[Vertex5] = {
        val d = 0.5-w

        val numAxis = fAxisCounts(s)
        val fMask = fRotationMasks(s)

        val verts = new Array[Vertex5](numAxis * 4)
        var vi = 0

        if ((fMask&0x5) != 0) { // Vertical axis looking at the face (i.e. looking down on face 0, north/south conns)
            val aVerts = axisVerts(fMask, d)
            reflectSide(aVerts, s, 0)
            Array.copy(aVerts, 0, verts, vi, 4)
            vi += 4
        }

        if ((fMask&0xA) != 0) { // Horizontal axis looking at the face (i.e. looking down on face 0, east/west conns)
            val aVerts = axisVerts(fMask>>1, d)
            reflectSide(aVerts, s, 1)
            val t = Rotation.quarterRotations(1).at(Vector3.CENTER)
            for (v <- aVerts) v.apply(t)
            Array.copy(aVerts, 0, verts, vi, 4)
            vi += 4
        }
        verts
    }

    private def generateFaceStubVerts(s:Int, d:Double):Array[Vertex5] = {
        val aVerts = axisVerts(0, d)
        val t = new UVTranslation(12, 12)
        for (vert <- aVerts) vert.apply(t)
        if (s%2 == 1) { //Reflect the stub on opposite sides
            val ft = new UVScale(-1, 1).at(new UV(20, 28))
            for (vert <- aVerts) vert.apply(ft)
        }
        aVerts
    }

    private def generateFaceCircleVerts(s:Int):Array[Vertex5] = {
        val d = 0.5-w
        val aVerts = axisVerts(0, d - 0.002) //Offset for zfighting
        rotateSide(aVerts, s)
        val t = new UVTranslation(16, 0)
        for (v <- aVerts) v.apply(t)
        aVerts
    }

    private def axisVerts(mask:Int, d:Double):Array[Vertex5] = {
        val tl = 8-tw
        val l = tl/16D + 0.004
        val zn = if ((mask&0x1) != 0) l else 0
        val vn = if ((mask&0x1) != 0) tl else 0
        val zp = if ((mask&0x4) != 0) l else 0
        val vp = if ((mask&0x4) != 0) tl else 0

        Array[Vertex5](
            new Vertex5(0.5-w, 1-d, 0.5+w+zp, 8-tw, 16+tw+vp),
            new Vertex5(0.5+w, 1-d, 0.5+w+zp, 8+tw, 16+tw+vp),
            new Vertex5(0.5+w, 1-d, 0.5-w-zn, 8+tw, 16-tw-vn),
            new Vertex5(0.5-w, 1-d, 0.5-w-zn, 8-tw, 16-tw-vn)
        )
    }

    private val sideReflect = new UVT(Rotation.quarterRotations(2).at(new Vector3(8, 0, 16)))
    private def reflectSide(verts:Array[Vertex5], s:Int, r:Int):Unit = {
        if ((r+PRLib.bundledCableBaseRotationMap(s))%4 >= 2) for (vert <- verts) vert.apply(sideReflect)
    }

    private def rotateSide(verts:Array[Vertex5], s:Int):Unit = {
        val r = PRLib.bundledCableBaseRotationMap(s)
        val uvt = new UVT(Rotation.quarterRotations(r%4).at(new Vector3(8, 0, 16)))
        for (vert <- verts) vert.apply(uvt)
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
            val t = new Scale(1, -1, 1).at(Vector3.CENTER)
            for (vert <- verts) vert.apply(t)
            reverseOrder(verts)
        }
        verts
    }

    def generateJacketedModel(key:Int) =
    {
        setup(key)
        new FWireJacketModel(generateJacketedWireModel, generateJacketedBoxes, generateJacketHighlightBoxes)
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
        val t = AxisCycle.cycles(s/2).at(Vector3.CENTER)
        val uvt = new UVTranslation(12, 12)
        for (vert <- verts)
        {
            vert.apply(t)
            vert.apply(uvt)
        }
        i = addVerts(model, verts, i)
    }

    private def generateJacketHighlightBoxes:Array[IndexedCuboid6] = {
        if (connCount == 0) return Array(new IndexedCuboid6(0, WireBoxes.fOBounds(6)))

        val boxes = Array.newBuilder[IndexedCuboid6]

        for (s <- 0 until 6) if ((connMap & 1<<s) != 0) {
            val box = fOBounds(0).copy
            box.apply(Rotation.sideRotations(s).at(Vector3.CENTER))
            val fMask = 1<<(s^1) //dont render face touching center
            boxes += new IndexedCuboid6(fMask, box)
        }

        //center box
        boxes += new IndexedCuboid6(connMap, WireBoxes.fOBounds(6)) //dont render faces on sides with conns

        boxes.result()
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

        box.apply(Rotation.sideRotations(a*2).at(Vector3.CENTER))
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
        for (i <- verts.indices) m.verts(k+i) = verts(i)
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

class FWireJacketModel(wire:CCModel, boxes:Array[IndexedCuboid6], highlightBoxes:Array[IndexedCuboid6])
{
    def renderWire(ccrs:CCRenderState, ops:IVertexOperation*)
    {
        wire.render(ccrs, ops:_*)
    }

    def renderMaterial(material:MicroMaterial, ccrs:CCRenderState, inventory:Boolean)
    {
        val layer = if (inventory) null else RenderType.solid()
        for (b <- boxes) MicroblockRender.renderCuboid(ccrs, material, layer, b, b.data.asInstanceOf[Int])
    }

    def renderHighlight(material:MicroMaterial, ccrs:CCRenderState, inventory:Boolean):Unit = {
        val layer = if (inventory) null else RenderType.solid()
        for (b <- highlightBoxes)
            MicroblockRender.renderCuboid(ccrs, material, layer, b, b.data.asInstanceOf[Int])
    }
}
