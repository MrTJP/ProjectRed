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

        // Removed: To minimize floating point errors we need to minimize calculations (especially 
        // multiplications) involving absolute world coordinates, which have lesser precision the
        // larger they get. The idea here is to do calculations which translate the halo box in
        // local camera coordinates instead of world coordinates: The box is rendered with
        // (entity.posX, entity.posY, entity.posZ) as the origin instead of (0, 0, 0) as before.
        //
        //RenderUtils.translateToWorldCoords(Minecraft.getMinecraft.renderViewEntity, event.partialTicks)
               
        // Adjust translation for camera movement (linear interpolation of movement between frames).
        // This is the same calculation as in RenderUtils.translateToWorldCoords except that the 
        // interpolated value is translated back to local camera coordinates (entity pos as origin).
        // Note:   Although this calculation could be simplified to (1 - partial) * (lastPos - pos) this 
        //         form is numerically more stable than the simplified version (which will cause judder).
        // MC 1.8: CodeChickenLib uses GlStateManager.translate() instead of GL11.glTranslated() in its
        //         1.8 branch (arguments stay the same).
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
        
        // Removed: We have to take into account that the current transformation
        //          matrix transforms to camera coordinates, since we changed it in
        //          onRenderWorldLast(), which is also the only function calling this method.
        //          
        //renderHalo(cc.cube, cc.color, new Translation(cc.pos.x, cc.pos.y, cc.pos.z))
        
        // cc.pos is in world coordinates, but we need camera coordinates instead to render the 
        // halo box. We get those by simply subtracting the camera position from cc.pos.
        val entity = Minecraft.getMinecraft.renderViewEntity;
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
