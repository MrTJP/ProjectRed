/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx.particles

import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import mrtjp.core.fx._
import net.minecraft.client.particle.Particle
import net.minecraft.client.renderer.BufferBuilder
//import net.minecraft.client.renderer.GlStateManager._
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.Entity
import net.minecraft.world.World
import org.lwjgl.opengl.GL11

class BeamPulse2(w:World) extends CoreParticle(w) with TAlphaParticle with TColourParticle with TPositionedParticle with TTargetParticle with TTextureParticle
{
    texture = "projectred:textures/particles/beam1.png"
    setSize(0.02F, 0.02F)

    var flareTexture = "projectred:textures/particles/beamflare.png"

    override def x = posX
    override def y = posY
    override def z = posZ

    override def px = prevPosX
    override def py = prevPosY
    override def pz = prevPosZ

    override def px_=(x:Double){prevPosX = x}
    override def py_=(y:Double){prevPosY = y}
    override def pz_=(z:Double){prevPosZ = z}

    private var s:ParticleAction = null
    def doPulse(r:Double, g:Double, b:Double)
    {
        import ParticleAction._
        removeAction(s)
        s = sequence(
            changeRGBATo(r, g, b, 0.8, 10),
            changeRGBATo(0.5, 0.5, 0.5, 0.3, 32)
        )
        runAction(s)
    }

    /*override def renderParticle(buffer:BufferBuilder, entity:Entity, frame:Float, cosyaw:Float, cospitch:Float, sinyaw:Float, sinsinpitch:Float, cossinpitch:Float)
    {
        super.renderParticle(buffer, entity, frame, cosyaw, cospitch, sinyaw, sinsinpitch, cossinpitch)

        TextureUtils.changeTexture(texture)

        val var9 = 1.0F
        val slide = getAge
        val size = 0.7F
        val dptx = x-tx
        val dpty = y-ty
        val dptz = z-tz
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
        depthMask(false)
        color(1.0F, 1.0F, 1.0F, 1.0F)

        val xx = px+dx*frame-Particle.interpPosX
        val yy = py+dy*frame-Particle.interpPosY
        val zz = pz+dz*frame-Particle.interpPosZ
        translate(xx, yy, zz)

        rotate(90.0F, 1.0F, 0.0F, 0.0F)
        rotate(180.0F+rotationYaw, 0.0F, 0.0F, -1.0F)
        rotate(rotationPitch, 1.0F, 0.0F, 0.0F)

        val var11 = slide+frame
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
        popMatrix()

        renderFlare(buffer, entity, frame, cosyaw, cospitch, sinyaw, sinsinpitch, cossinpitch)
    }

    def renderFlare(buffer:BufferBuilder, entity:Entity, frame:Float, cosyaw:Float, cospitch:Float, sinyaw:Float, sinsinpitch:Float, cossinpitch:Float)
    {
        TextureUtils.changeTexture(flareTexture)

        val part = particleAge%16

        val var8 = part/16.0
        val var9 = var8+0.0624375
        val var10 = 0.0
        val var11 = var10+0.0624375
        val var12 = 0.66*alpha

        val var13 = ptx+dtx*frame-Particle.interpPosX
        val var14 = pty+dty*frame-Particle.interpPosY
        val var15 = ptz+dtz*frame-Particle.interpPosZ

        val r = red.toFloat
        val g = green.toFloat
        val b = blue.toFloat
        val a = alpha.toFloat

        pushMatrix()
        disableLighting()
        enableBlend()
        blendFunc(SourceFactor.SRC_ALPHA, DestFactor.ONE)
        alphaFunc(516, 0.003921569F)
        depthMask(false)
        color(1.0F, 1.0F, 1.0F, 1.0F)

        val rs = CCRenderState.instance()
        rs.reset()
        rs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX_COLOR, buffer)
        buffer.pos(var13-cosyaw*var12-sinsinpitch*var12, var14-cospitch*var12, var15-sinyaw*var12-cossinpitch*var12).tex(var9, var11).color(r, g, b, a).endVertex()
        buffer.pos(var13-cosyaw*var12+sinsinpitch*var12, var14+cospitch*var12, var15-sinyaw*var12+cossinpitch*var12).tex(var9, var10).color(r, g, b, a).endVertex()
        buffer.pos(var13+cosyaw*var12+sinsinpitch*var12, var14+cospitch*var12, var15+sinyaw*var12+cossinpitch*var12).tex(var8, var10).color(r, g, b, a).endVertex()
        buffer.pos(var13+cosyaw*var12-sinsinpitch*var12, var14-cospitch*var12, var15+sinyaw*var12-cossinpitch*var12).tex(var8, var11).color(r, g, b, a).endVertex()
        rs.draw()

        depthMask(true)
        alphaFunc(516, 0.1F)
        disableBlend()
        enableLighting()
        popMatrix()
    }

    override def getFXLayer = 3*/
}
