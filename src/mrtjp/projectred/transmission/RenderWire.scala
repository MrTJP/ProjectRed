package mrtjp.projectred.transmission

import codechicken.lib.render._
import codechicken.lib.vec._
import codechicken.lib.render.uv._
import codechicken.lib.math.MathHelper
import java.util
import codechicken.lib.lighting.{LightMatrix, LightModel}
import net.minecraft.util.IIcon
import codechicken.lib.render.CCRenderState.IVertexOperation

object RenderWire
{
    /**
     * Array of all built models. Generated on demand.
     */
    val wireModels = new Array[CCModel](3*6*256)
    val invModels = new Array[CCModel](3)

    /**
     * Returns a tightly packed unique index for the specific model represented
     * by this wire. The mask is split into 3 sections the combination of
     * corresponding bits from the two lowest nybbles gives the connection type
     * in that direction.
     * 00 = none
     * 01 = corner
     * 10 = straight
     * 11 = internal The
     * second byte contains the thickness*6+side
     *
     * @param side      The side the wire is attached to
     * @param thickness The thickness of the wire -1 in 1/8th blocks. Supported
     *                  values 0, 1, 2
     * @param connMap   The connection mask of the wire
     */
    def modelKey(side:Int, thickness:Int, connMap:Int):Int =
    {
        var key = connMap&0xFF //take the straight and corner connections

        val renderCorner = connMap>>20&0xF
        key |= (renderCorner^key&0xF)<<4 //any corner conns that arent rendered convert to straight
        key &= ~0xF|renderCorner //set corners to renderCorers

        val internal = (connMap&0xF00)>>8 //internal cons
        key |= internal<<4|internal //if internal is set, set both straight and corner to 1

        key |= side+thickness*6<<8 //add side and thickness
        key
    }
    def modelKey(w:WirePart):Int = modelKey(w.side, w.getThickness, w.connMap)

    def getOrGenerateModel(key:Int) =
    {
        var m = wireModels(key)
        if (m == null) wireModels(key) =
            {m = WireModelGen.generateModel(key, false); m}
        m
    }
    def getOrGenerateInvModel(thickness:Int) =
    {
        var m = invModels(thickness)
        if (m == null) invModels(thickness) =
            {m = WireModelGen.generateInvModel(thickness); m}
        m
    }

    def render(w:WirePart, pos:Vector3)
    {
        getOrGenerateModel(modelKey(w)).render(
            pos.translation(), new IconTransformation(w.getIcon),
            ColourMultiplier.instance(w.renderHue), CCRenderState.colourAttrib)
    }

    def renderInv(thickness:Int, hue:Int, ops:IVertexOperation*)
    {
        getOrGenerateInvModel(thickness).render(ops :+ ColourMultiplier.instance(hue):_*)
    }

    def renderBreakingOverlay(icon:IIcon, wire:WirePart)
    {
        val key = modelKey(wire)
        val side = (key>>8)%6
        val w = ((key>>8)/6+1)/16D
        val h = w+1/16D
        val mask = key&0xFF
        val connMask = (mask&0xF0)>>4|mask&0xF
        val connCount = WireModelGen.countConnections(connMask)

        val boxes = Vector.newBuilder[Cuboid6]
        boxes += new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, h, 0.5+w).apply(Rotation.sideRotations(side).at(Vector3.center)) //center

        for (r <- 0 until 4)
        {
            val length =
                if (connCount == 0)
                {
                    if (r%2 == 1) 4
                    else 0
                }
                else if (connCount == 1)
                {
                    if (connMask == (1<<(r+2)%4)) 4 //this side is opposite the one with a connection
                    else if (connMask == (1<<r)) 8
                    else 0
                }
                else if ((connMask&1<<r) != 0) 8 else 0

            if (length > 0)
            {
                val l = length/16D
                boxes += new Cuboid6(0.5-w, 0, 0.5+w, 0.5+w, h, 0.5+l).apply(Rotation.sideOrientation(side, r).at(Vector3.center))
            }
        }

        CCRenderState.setPipeline(new Translation(wire.x, wire.y, wire.z), new IconTransformation(icon))
        for (box <- boxes.result()) BlockRenderer.renderCuboid(box, 0)
    }
}

class UVT(t:Transformation) extends UVTransformation
{
    private val vec = new Vector3

    def transform(uv:UV)
    {
        vec.set(uv.u, 0, uv.v).apply(t)
        uv.set(vec.x, vec.z)
    }

    override def apply(uv:UV) =
    {
        vec.set(uv.u, 0, uv.v).apply(t)
        uv.set(vec.x, vec.z)
    }

    override def inverse() = new UVT(t.inverse())
}

/**
 * All generations are done on side 0 so know that for rotation r
 * 0 = side 3 = +Z = SOUTH
 * 1 = side 4 = -X = WEST
 * 2 = side 2 = -Z = NORTH
 * 3 = side 5 = +X = EAST
 */
private object WireModelGen
{
    var side = 0
    var tw = 0
    var th = 0
    var w = 0.0D
    var h = 0.0D
    var mask = 0
    var connMask = 0
    var connCount = 0
    var model:CCModel = null
    var i = 0
    var inv = false

    def countConnections(mask:Int) =
    {
        var n = 0
        for (r <- 0 until 4) if ((mask&1<<r) != 0) n+=1
        n
    }

    private def numFaces:Int =
    {
        if (inv) return 22
        val conns = if (connCount < 2) 2 else connCount
        var faces = conns*3+5
        for (i <- 0 until 4) if ((mask>>i&0x11) == 1) faces += 1
        faces
    }

    def generateInvModel(thickness:Int) = generateModel(RenderWire.modelKey(0, thickness, 0xF0), true)

    def generateModel(key:Int, inv:Boolean):CCModel =
    {
        this.inv = inv
        side = (key>>8)%6
        tw = (key>>8)/6+1
        w = tw/16D
        th = tw+1
        h = th/16D
        mask = key&0xFF
        connMask = (mask&0xF0)>>4|mask&0xF
        connCount = countConnections(connMask)
        model = CCModel.quadModel(numFaces*4)
        i = 0

        generateCenter()
        for (r <- 0 until 4) generateSide(r)
        model.apply(Rotation.sideOrientation(side, 0).at(Vector3.center))

        finishModel()
        model
    }

    private def generateCenter()
    {
        var tex = connCount match//0 = straight n/s, 1 = straight e/w, 2 = circle
        {
            case 0 => 1
            case 1 => if ((connMask&5) != 0) 0 else 1 //if there is one connection, and it is north/south then north/south, otherwise east/west
            case _ if connMask == 5 => 0
            case _ if connMask == 10 => 1
            case _ => 2
        }
        var verts = Array[Vertex5](
            new Vertex5(0.5-w, h, 0.5+w, 8-tw, 16+tw),
            new Vertex5(0.5+w, h, 0.5+w, 8+tw, 16+tw),
            new Vertex5(0.5+w, h, 0.5-w, 8+tw, 16-tw),
            new Vertex5(0.5-w, h, 0.5-w, 8-tw, 16-tw)
        )

        if (tex == 0 || tex == 1) tex = (tex+reorientSide(side))%2
        var r = reorientSide(side)
        if (tex == 1) r += 3
        if (r != 0)
        {
            val uvt = new UVT(Rotation.quarterRotations(r%4).at(new Vector3(8, 0, 16)))
            for (vert <- verts) vert.apply(uvt)
        }
        if (tex == 2) //circle (translate across to u = 24)
        {
            val uvt = new UVTranslation(16, 0)
            for (vert <- verts) vert.apply(uvt)
        }
        if (inv) verts = withBottom(verts, 0, 4)

        i = addVerts(model, verts, i)
    }

    private def generateSide(r:Int)
    {
        val stype = (mask>>r)&0x11

        val verts = if (inv) generateSideInv(r) else connCount match
        {
            case 0 => if (r%2 == 1) generateStub(r) else generateFlat(r)
            case 1 if connMask == (1<<(r+2)%4) => generateStub(r) //this side is opposite the one with a connection
            case _ => generateSideFromType(stype, r)
        }

        val t = Rotation.quarterRotations(r).at(Vector3.center)
        for (vert <- verts) vert.apply(t)
        i = addVerts(model, verts, i)
    }

    private def generateSideInv(r:Int) = withBottom(generateStraight(r), 4, 4)

    private def generateStraight(r:Int) =
    {
        val verts = generateExtension(8)
        reflectSide(verts, r)
        verts
    }

    private def generateExtension(tl:Int) =
    {
        val l = tl/16D
        Array(
            //cap
            new Vertex5(0.5-w, 0, 0.5+l, 8-tw, 24+2*th),
            new Vertex5(0.5+w, 0, 0.5+l, 8+tw, 24+2*th),
            new Vertex5(0.5+w, h, 0.5+l, 8+tw, 24+th),
            new Vertex5(0.5-w, h, 0.5+l, 8-tw, 24+th),
            //top
            new Vertex5(0.5-w, h, 0.5+l, 8-tw, 16+tl),
            new Vertex5(0.5+w, h, 0.5+l, 8+tw, 16+tl),
            new Vertex5(0.5+w, h, 0.5+w, 8+tw, 16+tw),
            new Vertex5(0.5-w, h, 0.5+w, 8-tw, 16+tw),
            //left
            new Vertex5(0.5-w, 0, 0.5+w, 0, 16+tw),
            new Vertex5(0.5-w, 0, 0.5+l, 0, 16+tl),
            new Vertex5(0.5-w, h, 0.5+l, th, 16+tl),
            new Vertex5(0.5-w, h, 0.5+w, th, 16+tw),
            //right
            new Vertex5(0.5+w, 0, 0.5+l, 16, 16+tl),
            new Vertex5(0.5+w, 0, 0.5+w, 16, 16+tw),
            new Vertex5(0.5+w, h, 0.5+w, 16-th, 16+tw),
            new Vertex5(0.5+w, h, 0.5+l, 16-th, 16+tl)
        )
    }

    private def generateStub(r:Int):Array[Vertex5] =
    {
        val verts = generateExtension(4)
        for (i <- 0 until 4) verts(i).vec.z -= 0.002 //pull the stub in a little so it doesn't z fight with framed cables
        reflectSide(verts, r)
        verts
    }

    private def generateFlat(r:Int):Array[Vertex5] =
    {
        val verts = Array(
            new Vertex5(0.5-w, 0, 0.5+w, 16, 16+tw),
            new Vertex5(0.5+w, 0, 0.5+w, 16, 16-tw),
            new Vertex5(0.5+w, h, 0.5+w, 16-th, 16-tw),
            new Vertex5(0.5-w, h, 0.5+w, 16-th, 16+tw)
        )

        if (Rotation.rotateSide(side, r)%2 == 0) //red is on the negative side
        {
            val uvt = new UVT(Rotation.quarterRotations(2).at(new Vector3(8, 0, 16)))
            for (vert <- verts) vert.apply(uvt)
        }
        verts
    }

    private def generateSideFromType(stype:Int, r:Int) = stype match
    {
        case 0x00 => generateFlat(r)
        case 0x01 => generateCorner(r)
        case 0x10 => generateStraight(r)
        case _    => generateInternal(r)
    }

    private def generateCorner(r:Int):Array[Vertex5] =
    {
        var verts = generateExtension(8+th)
        //retexture cap
        for (i <- 0 until 4) verts(i).apply(new UVTranslation(0, -th))

        //add end face extending around block
        verts = util.Arrays.copyOf(verts, 20)
        verts(16) = new Vertex5(0.5-w, 0, 1, 8-tw, 24+2*th)
        verts(17) = new Vertex5(0.5+w, 0, 1, 8+tw, 24+2*th)
        verts(18) = new Vertex5(0.5+w, 0, 1+h, 8+tw, 24+th)
        verts(19) = new Vertex5(0.5-w, 0, 1+h, 8-tw, 24+th)

        reflectSide(verts, r)
        verts
    }

    private def generateInternal(r:Int):Array[Vertex5] =
    {
        val verts = generateExtension(8)

        // retexture cap
        verts(0).uv.set(8+tw, 24)
        verts(1).uv.set(8-tw, 24)
        verts(2).uv.set(8-tw, 24+tw)
        verts(3).uv.set(8+tw, 24+tw)

        //offset side textures
        reflectSide(verts, r)
        for (i <- 4 until 16) verts(i).apply(new UVTranslation(16, 0))

        verts
    }

    /**
     * Returns a copy of vertices with the bottom face added at the start.
     *
     * @param start The index of the first vertex making up the top face
     * @param count The number of vertices making up the top face
     */
    private def withBottom(verts:Array[Vertex5], start:Int, count:Int):Array[Vertex5] =
    {
        val i_verts = new Array[Vertex5](verts.length+count)
        val r = new Rotation(MathHelper.pi, 0, 0, 1).at(new Vector3(0.5, h/2, 0))

        for (i <- 0 until count) i_verts(i) = verts(i+start).copy.apply(r)
        System.arraycopy(verts, 0, i_verts, count, verts.length)

        i_verts
    }

    val reorientSide = Array(0, 3, 3, 0, 0, 3)
    private val sideReflect = new UVT(Rotation.quarterRotations(2).at(new Vector3(8, 0, 16)))
    private def reflectSide(verts:Array[Vertex5], r:Int)
    {
        if ((r+reorientSide(side))%4 >= 2) for (vert <- verts) vert.apply(sideReflect)
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
