package mrtjp.projectred.illumination

import codechicken.lib.render._
import codechicken.lib.render.uv.IconTransformation
import codechicken.lib.vec._
import cpw.mods.fml.common.eventhandler.SubscribeEvent
import mrtjp.projectred.core.Configurator
import mrtjp.projectred.core.libmc.PRColors
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraft.tileentity.TileEntity
import net.minecraft.world.World
import net.minecraftforge.client.IItemRenderer
import net.minecraftforge.client.IItemRenderer.{ItemRenderType, ItemRendererHelper}
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
        import net.minecraftforge.client.IItemRenderer.ItemRenderType._
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

trait ButtonRenderCommons extends IItemRenderer
{
    val invRenderBox = new Cuboid6(0.0, 0.375, 0.5-0.1875, 0.25, 0.625, 0.5+0.1875)
    val invLightBox = invRenderBox.copy.expand(0.025D)

    override def handleRenderType(item:ItemStack, t:ItemRenderType) = true
    override def shouldUseRenderHelper(t:ItemRenderType, item:ItemStack, helper:ItemRendererHelper) = true

    override def renderItem(t:ItemRenderType, item:ItemStack, data:AnyRef*)
    {
        import net.minecraftforge.client.IItemRenderer.ItemRenderType._
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
                drawExtras(t)

                CCRenderState.draw()
                RenderHalo.prepareRenderState()
                RenderHalo.renderHalo(invLightBox, color, t)
                RenderHalo.restoreRenderState()
                GL11.glPopMatrix()
            }
        }
    }

    def drawExtras(t:Transformation){}
}

object RenderButton extends ButtonRenderCommons

object RenderFButton extends ButtonRenderCommons
{
    val model = genModel(16, 2, 8)
    override def drawExtras(t:Transformation)
    {
        model.render(t, new IconTransformation(Blocks.redstone_torch.getIcon(0, 0)))
    }

    private def genModel(height:Int, x:Double, z:Double):CCModel =
    {
        val m = CCModel.quadModel(20)
        m.verts(0) = new Vertex5(7/16D, 10/16D, 9/16D, 7/16D, 8/16D)
        m.verts(1) = new Vertex5(9/16D, 10/16D, 9/16D, 9/16D, 8/16D)
        m.verts(2) = new Vertex5(9/16D, 10/16D, 7/16D, 9/16D, 6/16D)
        m.verts(3) = new Vertex5(7/16D, 10/16D, 7/16D, 7/16D, 6/16D)
        m.generateBlock(4, 6/16D, (10-height)/16D, 7/16D, 10/16D, 11/16D, 9/16D, 0x33)
        m.generateBlock(12, 7/16D, (10-height)/16D, 6/16D, 9/16D, 11/16D, 10/16D, 0xF)
        m.apply(new Translation(-0.5+x/16, (height-10)/16D, -0.5+z/16))
        m.computeNormals
        m.shrinkUVs(0.0005)
        m.apply(new Scale(1.0005))
        m
    }
}


