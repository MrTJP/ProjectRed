package mrtjp.projectred.core

import codechicken.lib.colour.EnumColour
import codechicken.lib.render.{BlockRenderer, CCRenderState}
import codechicken.lib.vec._
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.eventbus.api.SubscribeEvent
import org.lwjgl.opengl.GL11._

object RenderHalo
{
    private var renderList = Vector[LightCache]()
    private val renderEntityPos = new Vector3
    private val vec = new Vector3

    private class LightCache(val pos:BlockPos, val color:Int, val cube:Cuboid6) extends Ordered[LightCache]
    {
        def this(x:Int, y:Int, z:Int, c:Int, cube:Cuboid6) = this(new BlockPos(x, y, z), c, cube)

        private def renderDist = vec.set(pos.getX, pos.getY, pos.getZ).subtract(renderEntityPos).magSquared

        override def compare(o:LightCache) =
        {
            val ra = renderDist
            val rb = o.renderDist
            if (ra == rb) 0 else if (ra < rb) 1 else -1
        }
    }

    def addLight(pos:BlockPos, color:Int, box:Cuboid6)
    {
        renderList :+= new LightCache(pos, color, box)
    }

    @SubscribeEvent
    def onRenderWorldLast(event:RenderWorldLastEvent)
    {
//        if (renderList.isEmpty) return
//        val w = Minecraft.getMinecraft.world
//        val entity = Minecraft.getMinecraft.getRenderViewEntity
//        renderEntityPos.set(entity.posX, entity.posY+entity.getEyeHeight, entity.posZ)
//
//        renderList = renderList.sorted
//
//        pushMatrix()
//        // Adjust translation for camera movement between frames (using camra coordinates for numeric stability).
//        translate(
//             entity.posX-(entity.posX-entity.lastTickPosX)*event.getPartialTicks-entity.lastTickPosX,
//             entity.posY-(entity.posY-entity.lastTickPosY)*event.getPartialTicks-entity.lastTickPosY,
//             entity.posZ-(entity.posZ-entity.lastTickPosZ)*event.getPartialTicks-entity.lastTickPosZ
//        )
//        prepareRenderState()
//
//        val it = renderList.iterator
//        val max = if (Configurator.lightHaloMax < 0) renderList.size else Configurator.lightHaloMax
//
//        var i = 0
//        while (i < max && it.hasNext) {
//            val cc = it.next()
//            renderHalo(w, cc)
//            i += 1
//        }
//        renderList = Vector()
//
//        restoreRenderState()
//        popMatrix()
    }

//    def prepareRenderState()
//    {
//        enableBlend()
//        blendFunc(GL_SRC_ALPHA, GL_ONE)
//        disableTexture2D()
//        disableLighting()
//        disableCull()
//        depthMask(false)
//
//        val rs = CCRenderState.instance()
//        rs.reset()
//        rs.startDrawing(GL_QUADS, DefaultVertexFormats.ITEM)
//    }
//
//    def restoreRenderState()
//    {
//        CCRenderState.instance().draw()
//        depthMask(true)
//        color(1, 1, 1, 1)
//        enableCull()
//        enableLighting()
//        enableTexture2D()
//        blendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA)
//        disableBlend()
//    }
//
//    private def renderHalo(world:World, cc:LightCache)
//    {
//        CCRenderState.instance().setBrightness(world, cc.pos)
//        // Make sure to use camera coordinates for the halo transformation.
//        val entity = Minecraft.getMinecraft.getRenderViewEntity
//        renderHalo(cc.cube, cc.color,
//            new Translation(cc.pos.getX-entity.posX, cc.pos.getY-entity.posY, cc.pos.getZ-entity.posZ))
//    }

    def renderHalo(cuboid:Cuboid6, colour:Int, t:Transformation)
    {
        val rs = CCRenderState.instance()
        rs.reset()
        rs.setPipeline(t)
        rs.baseColour = EnumColour.values()(colour).rgba
        rs.alphaOverride = 128
        BlockRenderer.renderCuboid(rs, cuboid, 0)
    }
}
