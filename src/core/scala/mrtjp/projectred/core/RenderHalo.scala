package mrtjp.projectred.core

import codechicken.lib.colour.EnumColour
import codechicken.lib.render.{BlockRenderer, CCRenderState}
import codechicken.lib.util.SneakyUtils
import codechicken.lib.vec._
import com.mojang.blaze3d.matrix.MatrixStack
import com.mojang.blaze3d.systems.RenderSystem
import net.minecraft.client.Minecraft
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.client.renderer.{IRenderTypeBuffer, RenderState, RenderType}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.math.vector.{Matrix4f, Vector3d}
import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.eventbus.api.SubscribeEvent

object RenderHalo
{
    val RenderTypeLamp:RenderType = RenderType.create("pr:lamp",
        DefaultVertexFormats.POSITION_COLOR, 7, 8192, false, true,
        RenderType.State.builder().setTransparencyState(RenderState.LIGHTNING_TRANSPARENCY)
                .setTextureState(RenderState.NO_TEXTURE)
                .setTexturingState(new RenderState.TexturingState("disable_lighting", () => RenderSystem.disableLighting(), SneakyUtils.none))
                .setCullState(RenderState.NO_CULL)
//                .writeMask(RenderState.DEPTH_WRITE)
                .createCompositeState(false))

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
    def onRenderWorldLast(event:RenderWorldLastEvent):Unit = {
        if (renderList.nonEmpty) {
            val w = Minecraft.getInstance.level
            val mStack = event.getMatrixStack
            val buffers = Minecraft.getInstance.renderBuffers().bufferSource()
            val projectedView = Minecraft.getInstance.gameRenderer.getMainCamera.getPosition

            renderAndFlushLights(mStack, buffers, projectedView, event.getProjectionMatrix)
        }
    }

    def renderAndFlushLights(mStack:MatrixStack, buffers:IRenderTypeBuffer, projectedView:Vector3d, viewMat:Matrix4f):Unit = {
        mStack.pushPose()
        mStack.translate(-projectedView.x, -projectedView.y, -projectedView.z)

        val ccrs = CCRenderState.instance()
        prepareRenderState(ccrs, mStack, buffers)

        val it = renderList.iterator
        val max = if (Configurator.lightHaloMax < 0) renderList.size else Configurator.lightHaloMax

        var i = 0
        while (i < max && it.hasNext) {
            val cc = it.next()
            renderToCCRS(ccrs, cc.cube, cc.color, new Translation(cc.pos.getX, cc.pos.getY, cc.pos.getZ))
            i += 1
        }
        renderList = Vector()
        mStack.popPose()
    }


    def prepareRenderState(ccrs:CCRenderState, mStack:MatrixStack, buffers:IRenderTypeBuffer):Unit = {
        ccrs.reset()
        ccrs.bind(RenderTypeLamp, buffers, mStack)
    }

    def renderToCCRS(ccrs:CCRenderState, cuboid:Cuboid6, colour:Int, t:Transformation):Unit = {
        ccrs.setPipeline(t)
        ccrs.baseColour = EnumColour.values()(colour).rgba
        ccrs.alphaOverride = 160
        BlockRenderer.renderCuboid(ccrs, cuboid, 0)
    }

    def renderHalo(ccrs:CCRenderState, mStack:MatrixStack, buffers:IRenderTypeBuffer, cuboid:Cuboid6, colour:Int, pos:Vector3):Unit = {
        prepareRenderState(ccrs, mStack, buffers)
        renderToCCRS(ccrs, cuboid, colour, pos.translation())
    }
}
