/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import codechicken.lib.vec.Vector3
import mrtjp.core.fx.particles.CoreParticle

trait TScalableParticle extends CoreParticle
{
    var scale = Vector3.ONE.copy

    def scaleX = scale.x
    def scaleY = scale.y
    def scaleZ = scale.z

    def scaleX_=(x:Double){scale.x = x}
    def scaleY_=(y:Double){scale.y = y}
    def scaleZ_=(z:Double){scale.z = z}
}

class ScaleToAction extends ParticleAction
{
    var target = Vector3.ZERO
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TScalableParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val s = p.asInstanceOf[TScalableParticle]

        if (time < duration)
        {
            val dscale = target.copy.subtract(s.scale)
            val speed = dscale.copy.multiply(1/(duration-time)).multiply(deltaTime(time))
            s.scale.add(speed)

            //Check for resoulution errors - if any of the values have surpassed taret, then we are close enough
            val dscale2 = target.copy.subtract(s.scale)
            if (dscale2.x.signum == 0 || dscale2.x.signum != dscale.x.signum ||
                    dscale2.y.signum == 0 || dscale2.y.signum != dscale.y.signum ||
                    dscale2.z.signum == 0 || dscale2.z.signum != dscale.z.signum)
                isFinished = true
        }
        else isFinished = true

        if (isFinished)
            s.scale.set(target)
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.scaleTo(target.x, target.y, target.z, duration)
}

class ScaleForAction extends ParticleAction
{
    var delta = Vector3.ZERO
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TScalableParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val s = p.asInstanceOf[TScalableParticle]
        if (time < duration) s.scale.add(delta.copy.multiply(deltaTime(time)))
        else isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.scaleFor(delta.x, delta.y, delta.z, duration)
}
