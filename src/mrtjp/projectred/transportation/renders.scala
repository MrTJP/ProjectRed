package mrtjp.projectred.transportation

import codechicken.lib.colour.EnumColour
import codechicken.lib.lighting.LightModel
import codechicken.lib.render._
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.render.pipeline.{ColourMultiplier, IVertexOperation}
import codechicken.lib.texture.TextureUtils
import codechicken.lib.texture.TextureUtils.IIconRegister
import codechicken.lib.util.TransformUtils
import codechicken.lib.vec._
import codechicken.lib.vec.uv.{IconTransformation, UV, UVScale, UVTransformation}
import codechicken.microblock.{BlockMicroMaterial, CommonMicroFactory, IMicroHighlightRenderer, MicroMaterialRegistry}
import codechicken.multipart.{BlockMultipart, PartRayTraceResult}
import com.google.common.collect.ImmutableList
import net.minecraft.block.state.IBlockState
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.GlStateManager._
import net.minecraft.client.renderer.block.model.ItemCameraTransforms.TransformType
import net.minecraft.client.renderer.block.model.{ItemCameraTransforms, ItemOverrideList}
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.item.ItemStack
import net.minecraft.util.EnumFacing
import net.minecraft.util.math.RayTraceResult
import net.minecraftforge.client.model.IPerspectiveAwareModel
import net.minecraftforge.client.model.IPerspectiveAwareModel.MapWrapper
import org.lwjgl.opengl.GL11

object RenderPipe extends IIconRegister
{
    private final val customRenderItem = Minecraft.getMinecraft.getRenderItem

    var sideModels:Array[CCModel] = null
    var centerModels:Array[CCModel] = null

    var sideModelsRS:Array[CCModel] = null
    var centerModelsRS:Array[CCModel] = null

    var sideModelsWool:Array[CCModel] = null
    var centerModelsWool:Array[CCModel] = null

    {
        val gen = new PipeModelGenerator
        sideModels = gen.sideModels
        centerModels = gen.centerModels

        val rsgen = new PipeModelGenerator
        rsgen.applyScale(0.90)
        sideModelsRS = rsgen.sideModels
        centerModelsRS = rsgen.centerModels

        val woolgen = new PipeModelGenerator
        woolgen.applyScale(0.80)
        sideModelsWool = woolgen.sideModels
        centerModelsWool = woolgen.centerModels
    }

    override def registerIcons(textureMap:TextureMap)
    {
        for (p <- PipeDefs.values) p.registerIcon(textureMap)
        for (c <- RoutingChipDefs.values) c.registerIcons(textureMap)
    }

    def renderPipe(p:SubcorePipePart, pos:Vector3, ccrs:CCRenderState)
    {
        val t = pos.translation()
        var uvt = new IconTransformation(p.getPipeType.sprites(0))
        val connMap = p.connMap&0x3F

        if (connMap == 0x3 || connMap == 0xC || connMap == 0x30) for (a <- 0 until 3)
        {
            if ((connMap>>a*2) == 3) centerModels(a).render(ccrs, t, uvt)
        }
        else centerModels(3).render(ccrs, t, uvt)

        for (s <- 0 until 6) if ((connMap&1<<s) != 0)
        {
            uvt = new IconTransformation(p.getIcon(s))
            sideModels(s).render(ccrs, t, uvt)
        }

        val gen = new PipeModelGenerator
        sideModels = gen.sideModels
        centerModels = gen.centerModels

        val rsgen = new PipeModelGenerator
        rsgen.applyScale(0.90)
        sideModelsRS = rsgen.sideModels
        centerModelsRS = rsgen.centerModels

        val woolgen = new PipeModelGenerator
        woolgen.applyScale(0.85)
        sideModelsWool = woolgen.sideModels
        centerModelsWool = woolgen.centerModels

    }

    def renderRSWiring(p:TRedstonePipe, pos:Vector3, signal:Byte, ccrs:CCRenderState)
    {
        val t = pos.translation()
        val colour = ColourMultiplier.instance((signal&0xFF)/2+60<<24|0xFF)
        val uvt2 = new IconTransformation(PipeDefs.BASIC.sprites(1))
        val connMap = p.connMap&0x3F

        if (connMap == 0x3 || connMap == 0xC || connMap == 0x30) for (a <- 0 until 3)
        {
            if ((connMap>>a*2) == 3) centerModelsRS(a).render(ccrs, t, uvt2, colour)
        }
        else centerModelsRS(3).render(ccrs, t, uvt2, colour)

        for (s <- 0 until 6) if ((connMap&1<<s) != 0)
            sideModelsRS(s).render(ccrs, t, uvt2, colour)
    }

    def renderColourWool(p:TColourFilterPipe, pos:Vector3, colour:Byte, ccrs:CCRenderState)
    {
        val t = pos.translation()
        val uvt2 = new IconTransformation(PipeDefs.PRESSURETUBE.sprites(1+colour))
        val connMap = p.connMap&0x3F

        if (connMap == 0x3 || connMap == 0xC || connMap == 0x30) for (a <- 0 until 3)
        {
            if ((connMap>>a*2) == 3) centerModelsWool(a).render(ccrs, t, uvt2)
        }
        else centerModelsWool(3).render(ccrs, t, uvt2)

        for (s <- 0 until 6) if ((connMap&1<<s) != 0)
            sideModelsWool(s).render(ccrs, t, uvt2)
    }

    def renderBreakingOverlay(icon:TextureAtlasSprite, pipe:SubcorePipePart, ccrs:CCRenderState)
    {
        ccrs.setPipeline(new Translation(pipe.x, pipe.y, pipe.z), new IconTransformation(icon))
        import scala.collection.JavaConversions._
        for (box <- pipe.getCollisionBoxes)
            BlockRenderer.renderCuboid(ccrs, box, 0)
    }

    def renderInv(ccrs:CCRenderState, ops:IVertexOperation*)
    {
        centerModels(3).render(ccrs, ops:_*)
        for (s <- 0 to 1) sideModels(s).render(ccrs, ops:_*)
    }

    def renderItemFlow[T <: AbstractPipePayload](p:PayloadPipePart[T], pos:Vector3, frame:Float, ccrs:CCRenderState)
    {
        pushMatrix()
        disableLighting()
        for (r <- p.itemFlow.delegate) if (!p.itemFlow.outputQueue.contains(r))
        {
            val dir = if (r.isEntering) r.input else r.output
            val prog = r.progress+(r.speed*frame)

            var frameX = pos.x+0.5D
            var frameY = pos.y+0.25D
            var frameZ = pos.z+0.5D
            dir match
            {
                case 0 => frameY = (pos.y-0.25D)+(1.0D-prog)
                case 1 => frameY = (pos.y-0.25D)+prog
                case 2 => frameZ = pos.z+(1.0D-prog)
                case 3 => frameZ = pos.z+prog
                case 4 => frameX = pos.x+(1.0D-prog)
                case 5 => frameX = pos.x+prog
                case _ =>
            }
            doRenderItem(r, frameX, frameY, frameZ)
            r match
            {
                case net:NetworkPayload =>
                    renderPayloadColour(net.netPriority.color, frameX, frameY, frameZ, ccrs)
                case pa:PressurePayload if pa.colour > -1 =>
                    renderPayloadColour(pa.colour, frameX, frameY, frameZ, ccrs)
                case _ =>
            }
        }
        enableLighting()
        popMatrix()
    }

    private def doRenderItem(r:AbstractPipePayload, x:Double, y:Double, z:Double)
    {
        if (r == null || r.getItemStack == null) return
        val renderScale = 0.7f
        val itemstack = r.getItemStack

        pushMatrix()
        translate(x, y, z)
        translate(0, 0.125f, 0)
        scale(renderScale, renderScale, renderScale)

        customRenderItem.renderItem(itemstack, TransformType.FIXED)

        popMatrix()
    }

    private def renderPayloadColour(colour:Int, x:Double, y:Double, z:Double, ccrs:CCRenderState)
    {
        pushMatrix()
        prepareRenderState(ccrs)
        enableLighting()

        val t = new Vector3(x, y, z).add(-4/16D, 0, -4/16D)

        ccrs.setPipeline(new Translation(t))
        ccrs.alphaOverride = 32
        ccrs.baseColour = EnumColour.values()(colour).rgba
        BlockRenderer.renderCuboid(ccrs, new Cuboid6(1/16D, 1/16D, 1/16D, 7/16D, 7/16D, 7/16D), 0)

        restoreRenderState(ccrs)
        popMatrix()
    }

    private def prepareRenderState(ccrs:CCRenderState)
    {
        enableBlend()
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE)
        disableTexture2D()
        disableLighting()
        disableCull()
        depthMask(false)
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)
        ccrs.pullLightmap()
    }

    private def restoreRenderState(ccrs:CCRenderState)
    {
        ccrs.draw()
        depthMask(true)
        color(1, 1, 1)
        enableCull()
        enableLighting()
        enableTexture2D()
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
        disableBlend()
    }

    private def renderMicroHighlight(part:SubcorePipePart, ccrs:CCRenderState, tFunc:() => _)
    {
        val pos = part.pos
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

        tFunc()

        ccrs.draw()
        disableBlend()
        depthMask(true)
        popMatrix()
    }

    def renderRSMicroHighlight(part:TRedstonePipe, ccrs:CCRenderState)
    {
        renderMicroHighlight(part, ccrs, {() => renderRSWiring(part, Vector3.zero, 255.toByte, ccrs)})
    }

    def renderWoolMicroHighlight(part:TColourFilterPipe, colour:Byte, ccrs:CCRenderState)
    {
        renderMicroHighlight(part, ccrs, {() => renderColourWool(part, Vector3.zero, colour, ccrs)})
    }
}

private class PipeModelGenerator(val w:Double = 2/8D, val d:Double = 1/16D-0.002)
{
    var sideModels = new Array[CCModel](6)
    var centerModels = new Array[CCModel](4)
    generateModels()

    def generateModels()
    {
        generateCenterModel()
        generateCrossExclusiveModels()
        generateSideModels()
        finishModels()
    }

    def generateCenterModel()
    {
        val model = CCModel.quadModel(48)
        model.verts(0) = new Vertex5(0.5-w, 0.5-w, 0.5-w, 4, 8)
        model.verts(1) = new Vertex5(0.5+w, 0.5-w, 0.5-w, 12, 8)
        model.verts(2) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 12, 0)
        model.verts(3) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 4, 0)
        model.verts(4) = new Vertex5(0.5-w, 0.5-w+d, 0.5+w, 4, 8)
        model.verts(5) = new Vertex5(0.5+w, 0.5-w+d, 0.5+w, 12, 8)
        model.verts(6) = new Vertex5(0.5+w, 0.5-w+d, 0.5-w, 12, 0)
        model.verts(7) = new Vertex5(0.5-w, 0.5-w+d, 0.5-w, 4, 0)
        model.generateSidedParts(0, Vector3.center)
        centerModels(3) = model
    }

    def generateCrossExclusiveModels()
    {
        val model = CCModel.quadModel(32)
        model.verts(0) = new Vertex5(0.5-w, 0.5-w, 0.5-w, 0, 16)
        model.verts(1) = new Vertex5(0.5+w, 0.5-w, 0.5-w, 8, 16)
        model.verts(2) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 8, 8)
        model.verts(3) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 0, 8)
        model.verts(4) = new Vertex5(0.5-w, 0.5-w+d, 0.5+w, 0, 16)
        model.verts(5) = new Vertex5(0.5+w, 0.5-w+d, 0.5+w, 8, 16)
        model.verts(6) = new Vertex5(0.5+w, 0.5-w+d, 0.5-w, 8, 8)
        model.verts(7) = new Vertex5(0.5-w, 0.5-w+d, 0.5-w, 0, 8)

        for (s <- 1 until 4) model.generateSidedPart(0, s, Vector3.center, 0, 8*s, 8)

        centerModels(0) = model.copy.apply(Rotation.sideOrientation(2, 1).at(Vector3.center))
        centerModels(1) = model.copy.apply(Rotation.sideOrientation(0, 1).at(Vector3.center))
        centerModels(2) = model
    }

    def generateSideModels()
    {
        val model = CCModel.quadModel(36)
        model.verts(0) = new Vertex5(0.5-w, 0, 0.5+w, 0, 0)
        model.verts(1) = new Vertex5(0.5+w, 0, 0.5+w, 0, 8)
        model.verts(2) = new Vertex5(0.5+w, 0.5-w, 0.5+w, 4, 8)
        model.verts(3) = new Vertex5(0.5-w, 0.5-w, 0.5+w, 4, 0)
        model.verts(4) = new Vertex5(0.5+w, 0, 0.5+w-d, 0, 0)
        model.verts(5) = new Vertex5(0.5-w, 0, 0.5+w-d, 0, 8)
        model.verts(6) = new Vertex5(0.5-w, 0.5-w, 0.5+w-d, 4, 8)
        model.verts(7) = new Vertex5(0.5+w, 0.5-w, 0.5+w-d, 4, 0)
        for (r <- 1 until 4) model.apply(Rotation.quarterRotations(r).at(Vector3.center), 0, r*8, 8)

        model.verts(32) = new Vertex5(0.5-w, 0, 0.5-w, 8, 16)
        model.verts(33) = new Vertex5(0.5+w, 0, 0.5-w, 16, 16)
        model.verts(34) = new Vertex5(0.5+w, 0, 0.5+w, 16, 8)
        model.verts(35) = new Vertex5(0.5-w, 0, 0.5+w, 8, 8)
        sideModels(0) = model

        for (s <- 1 until 6)
        {
            sideModels(s) = model.copy.apply(Rotation.sideRotations(s).at(Vector3.center))
            if (s%2 == 1)
            {
                val verts = sideModels(s).verts
                val t = new UVT(Rotation.quarterRotations(2).at(new Vector3(8, 0, 4)))
                for (i <- 0 until 32) verts(i).apply(t)
            }
        }
    }

    def finishModels()
    {
        for (m <- centerModels++sideModels)
        {
            m.apply(new UVScale(1/16D))
            m.shrinkUVs(0.0005)
            m.computeNormals
            m.computeLighting(LightModel.standardLightModel)
        }
    }

    def applyScale(scale:Double)
    {
        val nscale = 2.0-scale-(1.0-scale)*0.001
        val tscale = (1-scale)/2D
        val trans = Seq(
            new Translation(0, tscale, 0),
            new Translation(0, -tscale, 0),
            new Translation(0, 0, tscale),
            new Translation(0, 0, -tscale),
            new Translation(tscale, 0, 0),
            new Translation(-tscale, 0, 0)
        )

        for (s <- Seq(0, 1))
            sideModels(s).apply(new Scale(scale, nscale, scale).at(Vector3.center)).apply(trans(s))

        for (s <- Seq(2, 3))
            sideModels(s).apply(new Scale(scale, scale, nscale).at(Vector3.center)).apply(trans(s))

        for (s <- Seq(4, 5))
            sideModels(s).apply(new Scale(nscale, scale, scale).at(Vector3.center)).apply(trans(s))

        val cscale = scale
        for (f <- 0 until 4) centerModels(f).apply(new Scale(cscale, cscale, cscale).at(Vector3.center))
    }

}

private class UVT(t:Transformation) extends UVTransformation
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

object PipeRSHighlightRenderer extends IMicroHighlightRenderer
{
    override def renderHighlight(player:EntityPlayer, hit:RayTraceResult, mcrFactory:CommonMicroFactory, size:Int, material:Int):Boolean =
    {
        if (mcrFactory.getFactoryID != 3 || size != 1 || player.isSneaking) return false
        val tile = BlockMultipart.getTile(player.worldObj, hit.blockPos)
        if (tile == null) return false

        MicroMaterialRegistry.getMaterial(material) match
        {
            case b:BlockMicroMaterial if b.state.getBlock == Blocks.REDSTONE_BLOCK =>
                tile.partList(hit.asInstanceOf[PartRayTraceResult].partIndex) match {
                    case p:TRedstonePipe if !p.hasRedstone =>
                        RenderPipe.renderRSMicroHighlight(p, CCRenderState.instance())
                        true
                    case _ => false
                }
            case _ => false
        }
    }
}

object PipeColourHighlightRenderer extends IMicroHighlightRenderer
{
    override def renderHighlight(player:EntityPlayer, hit:RayTraceResult, mcrFactory:CommonMicroFactory, size:Int, material:Int):Boolean =
    {
        if (mcrFactory.getFactoryID != 3 || size != 1 || player.isSneaking) return false
        val tile = BlockMultipart.getTile(player.worldObj, hit.getBlockPos)
        if (tile == null) return false

        MicroMaterialRegistry.getMaterial(material) match
        {
            case b:BlockMicroMaterial if b.state.getBlock == Blocks.WOOL =>
                tile.partList(hit.asInstanceOf[PartRayTraceResult].partIndex) match
                {
                    case p:TColourFilterPipe if p.colour != b.state.getBlock.getMetaFromState(b.state) =>
                        RenderPipe.renderWoolMicroHighlight(p, b.state.getBlock.getMetaFromState(b.state).toByte, CCRenderState.instance())
                        true
                    case _ => false
                }
            case _ => false
        }
    }
}

object PipeItemRenderer extends IItemRenderer with IPerspectiveAwareModel
{

    override def getParticleTexture = null
    override def isBuiltInRenderer = true
    override def getItemCameraTransforms = ItemCameraTransforms.DEFAULT
    override def isAmbientOcclusion = true
    override def isGui3d = true
    override def getOverrides = ItemOverrideList.NONE
    override def getQuads(state:IBlockState, side:EnumFacing, rand:Long) = ImmutableList.of()

    override def handlePerspective(t:TransformType) =
        MapWrapper.handlePerspective(this, TransformUtils.DEFAULT_BLOCK.getTransforms, t)

    override def renderItem(item:ItemStack) =
    {
        val damage = item.getItemDamage
        renderWireInventory(damage, 0, 0, 0, 1, CCRenderState.instance())
    }

    def renderWireInventory(meta:Int, x:Float, y:Float, z:Float, scale:Float, ccrs:CCRenderState)
    {
        val pdef = PipeDefs.fromMeta(meta)
        if (pdef == null) return
        TextureUtils.bindBlockTexture()
        ccrs.reset()
        ccrs.pullLightmap()
        ccrs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.ITEM)

        RenderPipe.renderInv(ccrs, new Scale(scale).`with`(new Translation(x, y, z)), new IconTransformation(pdef.sprites(0)))

        ccrs.draw()
    }
}