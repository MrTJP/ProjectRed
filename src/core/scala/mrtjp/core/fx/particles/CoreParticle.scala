/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx.particles

import com.mojang.blaze3d.vertex.IVertexBuilder
import mrtjp.core.fx.ParticleAction
import net.minecraft.client.particle.{IParticleRenderType, Particle}
import net.minecraft.client.renderer.{ActiveRenderInfo, BufferBuilder}
import net.minecraft.entity.Entity
import net.minecraft.world.World

import scala.collection.mutable.ListBuffer

class CoreParticle(w: World) extends Particle(w, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D) {
    motionX = 0.0D
    motionY = 0.0D
    motionZ = 0.0D

    canCollide = false
    var hasVelocity = false
    var isImmortal = false

    private var actions = ListBuffer[ParticleAction]()

    def setAge(age: Int) {
        this.age = age
    }

    def getAge = age

    def runAction(action: ParticleAction) {
        if (!action.canOperate(this)) {
            throw new RuntimeException("Particle action was run on an incompatible particle class.")
        }
        val a1 = action.copy
        a1.compile(this)
        actions += a1
    }

    def removeAction(action: ParticleAction) {
        val idx = actions.indexOf(action)
        if (idx > -1) {
            actions.remove(idx)
        }
    }

    override def tick() {
        if (hasVelocity) move(motionX, motionY, motionZ)

        actions.foreach(_.tickLife())

        age += 1
        if (age > maxAge && !isImmortal) setExpired()
    }

    override def renderParticle(buffer: IVertexBuilder, renderInfo: ActiveRenderInfo, partialTicks: Float):Unit = {
        actions.foreach(_.runOn(this, partialTicks))
        actions = actions.filterNot(_.isFinished)
    }

//    override def renderParticle(buffer: BufferBuilder, entity: Entity, frame: Float, cosyaw: Float, cospitch: Float, sinyaw: Float, sinsinpitch: Float, cossinpitch: Float) {
//        actions.foreach(_.runOn(this, frame))
//        actions = actions.filterNot(_.isFinished)
//    }

    /**
     * 0 - Particle Texture
     * 1 - Block Texture
     * 2 - ?
     * 3 - Bind texture and draw yourself
     */
//    override def getFXLayer = 0
    override def getRenderType: IParticleRenderType = IParticleRenderType.PARTICLE_SHEET_TRANSLUCENT
}
