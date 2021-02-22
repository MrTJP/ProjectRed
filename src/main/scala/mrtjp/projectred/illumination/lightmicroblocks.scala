package mrtjp.projectred.illumination

import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Rotation._
import codechicken.lib.vec.Vector3._
import codechicken.lib.vec.{Cuboid6, RedundantTransformation}
import codechicken.microblock._
import codechicken.microblock.api.{BlockMicroMaterial, MicroBlockTrait}
import codechicken.mixin.api.MixinFactory
import com.mojang.blaze3d.matrix.MatrixStack
import mrtjp.projectred.core.RenderHalo
import net.minecraft.block.Block
import net.minecraft.client.renderer.IRenderTypeBuffer
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.function.Supplier
import scala.jdk.CollectionConverters._

trait ILightMicroblockMixinMarker {}

class LightMicroMaterial(val block:Supplier[Block]) extends BlockMicroMaterial(block.get().getDefaultState) with ILightMicroblockMixinMarker
{
    def getLightColor:Int = state.getBlock.asInstanceOf[IllumarLampBlock].colour
}

object LightMicroMaterial
{
    var traitKey:MixinFactory.TraitKey = _

    def register():Unit = {
        traitKey = MicroBlockGenerator.registerTrait(classOf[LightMicroblock])
    }
}

@MicroBlockTrait(value = classOf[ILightMicroblockMixinMarker])
trait LightMicroblock extends MicroblockClient
{
    @OnlyIn(Dist.CLIENT)
    override def renderDynamic(mStack:MatrixStack, buffers:IRenderTypeBuffer, packedLight:Int, packedOverlay:Int, partialTicks:Float):Unit = {

        val boxes = this match {
            case h: HollowMicroblock =>
                val size = h.getHollowSize
                val d1 = 0.5 - size / 32D
                val d2 = 0.5 + size / 32D
                val t = (shape >> 4) / 8D
                val ex = 0.025

                val tr = sideRotations(shape & 0xF).at(CENTER)

                Seq(new Cuboid6(0 - ex, 0 - ex, 0 - ex, 1 + ex, t + ex, d1 + ex),
                    new Cuboid6(0 - ex, 0 - ex, d2 - ex, 1 + ex, t + ex, 1 + ex),
                    new Cuboid6(0 - ex, 0 - ex, d1 + ex, d1 + ex, t + ex, d2 - ex),
                    new Cuboid6(d2 - ex, 0 - ex, d1 + ex, 1 + ex, t + ex, d2 - ex))
                    .map(c => c.apply(tr))
            case _ =>
                Seq(getBounds.copy.expand(0.025))
        }

        val colour = getMaterial.asInstanceOf[LightMicroMaterial].getLightColor

        val ccrs = CCRenderState.instance
        RenderHalo.prepareRenderState(ccrs, mStack, buffers)

        for (box <- boxes)
            RenderHalo.renderToCCRS(ccrs, box, colour, new RedundantTransformation)
    }

    override def getLightValue:Int = {
        val lightVolume = tile.getPartList.asScala.collect { case p:LightMicroblock => p }.map { light =>
            val b = light.getBounds
            math.abs(b.max.x-b.min.x)*math.abs(b.max.y-b.min.y)*math.abs(b.max.z-b.min.z)
        }.sum

        math.min(15, 10+5*lightVolume*8).toInt
    }
}
