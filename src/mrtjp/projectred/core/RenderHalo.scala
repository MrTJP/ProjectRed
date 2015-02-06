package mrtjp.projectred.core

import codechicken.lib.render.{BlockRenderer, CCRenderState, RenderUtils}
import codechicken.lib.vec._
import cpw.mods.fml.common.eventhandler.SubscribeEvent
import mrtjp.core.color.Colors_old
import net.minecraft.client.Minecraft
import net.minecraft.world.World
import net.minecraftforge.client.event.RenderWorldLastEvent
import org.lwjgl.opengl.GL11

object RenderHalo
{
    private var renderList = Vector[LightCache]()
    private val renderEntityPos = new Vector3
    private val vec = new Vector3

    private class LightCache(val pos:BlockCoord, val color:Int, val cube:Cuboid6) extends Ordered[LightCache]
    {
        def this(x:Int, y:Int, z:Int, c:Int, cube:Cuboid6) = this(new BlockCoord(x, y, z), c, cube)

        private def renderDist = vec.set(pos.x, pos.y, pos.z).sub(renderEntityPos).magSquared

        override def compare(o:LightCache) =
        {
            val ra = renderDist
            val rb = o.renderDist
            if (ra == rb) 0 else if (ra < rb) 1 else -1
        }
    }

    def addLight(x:Int, y:Int, z:Int, color:Int, box:Cuboid6)
    {
        renderList :+= new LightCache(x, y, z, color, box)
    }

    @SubscribeEvent
    def onRenderWorldLast(event:RenderWorldLastEvent)
    {
        if (renderList.size == 0) return
        val w = Minecraft.getMinecraft.theWorld
        val entity = Minecraft.getMinecraft.renderViewEntity
        renderEntityPos.set(entity.posX, entity.posY+entity.getEyeHeight, entity.posZ)

        renderList = renderList.sorted

        GL11.glPushMatrix()
        
        // Adjust translation for camera movement between frames (using camra coordinates for numeric stability).
        // Note: When porting to MC 1.8, might want to use GlStateManager.translate() here instead.
        GL11.glTranslated(
             entity.posX - (entity.posX - entity.lastTickPosX) * event.partialTicks - entity.lastTickPosX, 
             entity.posY - (entity.posY - entity.lastTickPosY) * event.partialTicks - entity.lastTickPosY,
             entity.posZ - (entity.posZ - entity.lastTickPosZ) * event.partialTicks - entity.lastTickPosZ)        
        
        prepareRenderState()
        val it = renderList.iterator
        val max = if (Configurator.lightHaloMax < 0) renderList.size else Configurator.lightHaloMax

        var i = 0
        while (i < max && it.hasNext)
        {
            val cc = it.next()
            renderHalo(w, cc)
            i += 1
        }

        renderList = Vector()
        restoreRenderState()
        GL11.glPopMatrix()
    }

    def prepareRenderState()
    {
        GL11.glEnable(GL11.GL_BLEND)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE)
        GL11.glDisable(GL11.GL_TEXTURE_2D)
        GL11.glDisable(GL11.GL_LIGHTING)
        GL11.glDisable(GL11.GL_CULL_FACE)
        GL11.glDepthMask(false)
        CCRenderState.reset()
        CCRenderState.setDynamic()
        CCRenderState.startDrawing()
    }

    def restoreRenderState()
    {
        CCRenderState.draw()
        GL11.glDepthMask(true)
        GL11.glColor3f(1, 1, 1)
        GL11.glEnable(GL11.GL_CULL_FACE)
        GL11.glEnable(GL11.GL_LIGHTING)
        GL11.glEnable(GL11.GL_TEXTURE_2D)
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
        GL11.glDisable(GL11.GL_BLEND)
    }

    private def renderHalo(world:World, cc:LightCache)
    {
        CCRenderState.setBrightness(world, cc.pos.x, cc.pos.y, cc.pos.z)
        // Make sure to use camera coordinates for the halo transformation.
        val entity = Minecraft.getMinecraft.renderViewEntity
        renderHalo(cc.cube, cc.color, 
            new Translation(cc.pos.x - entity.posX, cc.pos.y - entity.posY, cc.pos.z - entity.posZ))
    }

    def renderHalo(cuboid:Cuboid6, colour:Int, t:Transformation)
    {
        CCRenderState.reset()
        CCRenderState.setPipeline(t)
        CCRenderState.baseColour = Colors_old.VALID_COLORS(colour).rgba
        CCRenderState.alphaOverride = 128
        BlockRenderer.renderCuboid(cuboid, 0)
    }
}
