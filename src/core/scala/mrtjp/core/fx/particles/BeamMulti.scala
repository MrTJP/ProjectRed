/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx.particles

import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.Vector3
import mrtjp.core.fx.{TAlphaParticle, TColourParticle, TTextureParticle}
import net.minecraft.client.particle.Particle
import net.minecraft.client.renderer.BufferBuilder
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.Entity
import net.minecraft.world.World
import org.lwjgl.opengl.GL11

class BeamMulti(w:World) extends CoreParticle(w) with TAlphaParticle with TColourParticle with TTextureParticle
{
    texture = "projectred:textures/particles/beam1.png"
    setSize(0.02F, 0.02F)

    var points = Seq.empty[Vector3]

/*    override def renderParticle(buffer:BufferBuilder, entity:Entity, frame:Float, cosyaw:Float, cospitch:Float, sinyaw:Float, sinsinpitch:Float, cossinpitch:Float)
    {
        super.renderParticle(buffer, entity, frame, cosyaw, cospitch, sinyaw, sinsinpitch, cossinpitch)
        if (points.size > 1)
        {
            TextureUtils.changeTexture(texture)
            for (i <- 1 until points.size)
                drawBeam(buffer, points(i-1), points(i), frame)
        }
    }*/

    def drawBeam(buffer:BufferBuilder, p1:Vector3, p2:Vector3, f:Float)
    {
/*        val var9 = 1.0F
        val slide = getAge
        val size = 0.7F
        val dp = p1.copy.subtract(p2)
        val dptx = dp.x
        val dpty = dp.y
        val dptz = dp.z
        val length = math.sqrt(dptx*dptx+dpty*dpty+dptz*dptz)
        val rotationYaw = (math.atan2(dptx, dptz)*180.0D/math.Pi).toFloat
        val rotationPitch = (math.atan2(dpty, math.sqrt(dptx*dptx+dptz*dptz))*180.0D/math.Pi).toFloat

        val r = red.toFloat
        val g = green.toFloat
        val b = blue.toFloat
        val a = alpha.toFloat

        pushMatrix()
        glTexParameterf(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_S, 10497.0F)
        glTexParameterf(GL11.GL_TEXTURE_2D, GL11.GL_TEXTURE_WRAP_T, 10497.0F)
        disableCull()
        disableLighting()
        enableBlend()
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE)
        alphaFunc(516, 0.003921569F)
        depthMask(false)
        color(1.0F, 1.0F, 1.0F, 1.0F)

        val xx = p1.x-Particle.interpPosX
        val yy = p1.y-Particle.interpPosY
        val zz = p1.z-Particle.interpPosZ
        translate(xx, yy, zz)

        rotate(90.0F, 1.0F, 0.0F, 0.0F)
        rotate(180.0F+rotationYaw, 0.0F, 0.0F, -1.0F)
        rotate(rotationPitch, 1.0F, 0.0F, 0.0F)

        val var11 = slide+f
        val var12 = -var11*0.2F-math.floor(-var11*0.1F)
        val var44 = -0.15D*size
        val var17 = 0.15D*size

        for (t <- 0 until 2)
        {
            val var29 = length*var9
            val var31 = 0.0D
            val var33 = 1.0D
            val var35 = -1.0F+var12+t/3.0F
            val var37 = length*var9+var35

            rotate(90.0F, 0.0F, 1.0F, 0.0F)

            val rs = CCRenderState.instance()

            rs.reset()
            rs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX_COLOR, buffer)

            buffer.pos(var44, var29, 0.0D).tex(var33, var37).color(r, g, b, a).endVertex()
            buffer.pos(var44, 0.0D, 0.0D).tex(var33, var35).color(r, g, b, a).endVertex()
            buffer.pos(var17, 0.0D, 0.0D).tex(var31, var35).color(r, g, b, a).endVertex()
            buffer.pos(var17, var29, 0.0D).tex(var31, var37).color(r, g, b, a).endVertex()

            rs.draw()
        }

        depthMask(true)
        alphaFunc(516, 0.1F)
        disableBlend()
        blendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA)
        enableLighting()
        enableCull()
        popMatrix()*/
    }

//    override def getFXLayer = 3
}
