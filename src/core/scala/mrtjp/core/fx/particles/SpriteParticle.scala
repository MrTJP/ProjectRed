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
import net.minecraft.client.renderer.vertex.DefaultVertexFormats
import net.minecraft.entity.Entity
import net.minecraft.world.World
import org.lwjgl.opengl.GL11

class SpriteParticle(w:World) extends CoreParticle(w) with TColourParticle with TAlphaParticle with TPositionedParticle with TTextureParticle with TScalableParticle
{
    override def x = posX
    override def y = posY
    override def z = posZ

    override def px = prevPosX
    override def py = prevPosY
    override def pz = prevPosZ

    override def px_=(x:Double){prevPosX = x}
    override def py_=(y:Double){prevPosY = y}
    override def pz_=(z:Double){prevPosZ = z}

    /*override def renderParticle(buffer:BufferBuilder, entity:Entity, frame:Float, cosyaw:Float, cospitch:Float, sinyaw:Float, sinsinpitch:Float, cossinpitch:Float)
    {
        super.renderParticle(buffer, entity, frame, cosyaw, cospitch, sinyaw, sinsinpitch, cossinpitch)

        TextureUtils.changeTexture(texture)

        val f11 = (prevPosX+(posX-prevPosX)*frame-Particle.interpPosX).toFloat
        val f12 = (prevPosY+(posY-prevPosY)*frame-Particle.interpPosY).toFloat
        val f13 = (prevPosZ+(posZ-prevPosZ)*frame-Particle.interpPosZ).toFloat

        val min_u = 0
        val min_v = 0
        val max_u = 1
        val max_v = 1

        val r = red.toFloat
        val g = green.toFloat
        val b = blue.toFloat
        val a = alpha.toFloat

        disableLighting()
        enableBlend()
        blendFunc(SourceFactor.SRC_ALPHA, DestFactor.ONE)
        alphaFunc(516, 0.003921569F)
        depthMask(false)
        color(1.0F, 1.0F, 1.0F, 1.0F)

        val rs = CCRenderState.instance()
        rs.reset()
        rs.startDrawing(GL11.GL_QUADS, DefaultVertexFormats.POSITION_TEX_COLOR, buffer)
        buffer.pos(f11-cosyaw*scaleX-sinsinpitch*scaleX, f12-cospitch*scaleY, f13-sinyaw*scaleZ-cossinpitch*scaleZ).tex(max_u, max_v).color(r, g, b, a).endVertex()
        buffer.pos(f11-cosyaw*scaleX+sinsinpitch*scaleX, f12+cospitch*scaleY, f13-sinyaw*scaleZ+cossinpitch*scaleZ).tex(max_u, min_v).color(r, g, b, a).endVertex()
        buffer.pos(f11+cosyaw*scaleX+sinsinpitch*scaleX, f12+cospitch*scaleY, f13+sinyaw*scaleZ+cossinpitch*scaleZ).tex(min_u, min_v).color(r, g, b, a).endVertex()
        buffer.pos(f11+cosyaw*scaleX-sinsinpitch*scaleX, f12-cospitch*scaleY, f13+sinyaw*scaleZ-cossinpitch*scaleZ).tex(min_u, max_v).color(r, g, b, a).endVertex()
        rs.draw()

        depthMask(true)
        alphaFunc(516, 0.1F)
        disableBlend()
        enableLighting()
    }

    override def getFXLayer = 3*/
}
