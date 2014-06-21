package mrtjp.projectred.illumination

import codechicken.lib.vec._
import net.minecraft.client.renderer.Tessellator
import codechicken.lib.render.{TextureUtils, CCRenderState, RenderUtils, BlockRenderer}
import mrtjp.projectred.core.libmc.PRColors
import cpw.mods.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraft.client.Minecraft
import org.lwjgl.opengl.GL11
import net.minecraft.world.World
import mrtjp.projectred.core.Configurator
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRendererHelper, ItemRenderType}
import net.minecraft.item.ItemStack
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.tileentity.TileEntity
import codechicken.lib.render.uv.IconTransformation

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
        RenderUtils.translateToWorldCoords(Minecraft.getMinecraft.renderViewEntity, event.partialTicks)
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
        renderHalo(cc.cube, cc.color, new Translation(cc.pos.x, cc.pos.y, cc.pos.z))
    }

    def renderHalo(cuboid:Cuboid6, colour:Int, t:Transformation)
    {
        CCRenderState.reset()
        CCRenderState.setPipeline(t)
        CCRenderState.baseColour = PRColors.VALID_COLORS(colour).rgba
        CCRenderState.alphaOverride = 128
        BlockRenderer.renderCuboid(cuboid, 0)
    }
}

object LampTESR extends TileEntitySpecialRenderer with IItemRenderer
{
    override def handleRenderType(item:ItemStack, `type`:ItemRenderType) = true
    override def shouldUseRenderHelper(`type`:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        import ItemRenderType._
        t match
        {
            case ENTITY => render(-0.15, 0, -0.15, 1)
            case EQUIPPED => render(0, 0, 0, 1)
            case EQUIPPED_FIRST_PERSON => render(0, 0, 0, 1)
            case INVENTORY => render(0, -0.05, 0, 0.95)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, s:Double)
        {
            val meta = item.getItemDamage
            val icon = new IconTransformation(if (meta > 15) BlockLamp.on(meta%16) else BlockLamp.off(meta))

            GL11.glPushMatrix()
            GL11.glTranslated(x, y, z)
            GL11.glScaled(s, s, s)
            TextureUtils.bindAtlas(0)
            CCRenderState.reset()
            CCRenderState.setDynamic()
            CCRenderState.pullLightmap()
            CCRenderState.startDrawing()

            val t = new Translation(x, y, z)
            CCRenderState.setPipeline(t, icon)
            BlockRenderer.renderCuboid(Cuboid6.full, 0)
            CCRenderState.draw()

            if (meta > 15)
            {
                RenderHalo.prepareRenderState()
                RenderHalo.renderHalo(lBounds, meta%16, t)
                RenderHalo.restoreRenderState()
            }

            GL11.glPopMatrix()
        }
    }

    private val lBounds = Cuboid6.full.copy.expand(0.05D)
    override def renderTileEntityAt(te:TileEntity, x:Double, y:Double, z:Double, partials:Float)
    {
        te match
        {
            case light:ILight if light.isOn =>
                val meta = te.getWorldObj.getBlockMetadata(te.xCoord, te.yCoord, te.zCoord)
                RenderHalo.addLight(te.xCoord, te.yCoord, te.zCoord, meta, lBounds)
            case _ =>
        }
    }
}

object RenderButton extends IItemRenderer
{
    private val invRenderBox = new Cuboid6(0.0, 0.375, 0.5-0.1875, 0.25, 0.625, 0.5+0.1875)
    private val invLightBox = invRenderBox.copy.expand(0.025D)

    override def handleRenderType(item:ItemStack, `type`:ItemRenderType) = true
    override def shouldUseRenderHelper(`type`:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        import ItemRenderType._
        t match
        {
            case ENTITY => render(-0.05, 0, -0.1, 0.5)
            case EQUIPPED => render(0.2, 0, 0, 1)
            case EQUIPPED_FIRST_PERSON => render(0.2, 0, 0, 1)
            case INVENTORY => render(0.25, 0, 0, 1)
            case _ =>
        }

        def render(x:Double, y:Double, z:Double, s:Double)
        {
            val color = item.getItemDamage
            if (0 until 16 contains color)
            {
                val icon = new IconTransformation(ItemPartButton.icons(color))
                val t = new Translation(x, y, z)

                GL11.glPushMatrix()
                GL11.glTranslated(x, y, z)
                GL11.glScaled(s, s, s)

                TextureUtils.bindAtlas(0)
                CCRenderState.reset()
                CCRenderState.setDynamic()
                CCRenderState.pullLightmap()
                CCRenderState.startDrawing()

                CCRenderState.setPipeline(t, icon)
                BlockRenderer.renderCuboid(invRenderBox, 0)

                CCRenderState.draw()
                RenderHalo.prepareRenderState()
                RenderHalo.renderHalo(invLightBox, color, t)
                RenderHalo.restoreRenderState()
                GL11.glPopMatrix()
            }
        }
    }
}


