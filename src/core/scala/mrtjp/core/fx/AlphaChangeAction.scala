/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import mrtjp.core.fx.particles.CoreParticle

trait TAlphaParticle extends CoreParticle
{
    var alpha = 1.0
}

class AlphaChangeToAction extends ParticleAction
{
    var target = 0.0
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TAlphaParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val p2 = p.asInstanceOf[TAlphaParticle]

        if (time < duration)
        {
            val da = target-p2.alpha
            val speed = da*(1/(duration-time))*deltaTime(time)
            p2.alpha = p2.alpha+speed
            if (p2.alpha > 1.0) p2.alpha = 1.0
            if (p2.alpha < 0.0) p2.alpha = 0
        }
        else isFinished = true
    }

    override def compile(p:CoreParticle)
    {
    }

    override def copy = ParticleAction.changeAlphaTo(target, duration)
}

class AlphaChangeForAction extends ParticleAction
{
    var delta = 0.0
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TAlphaParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val p2 = p.asInstanceOf[TAlphaParticle]

        if (time < duration) {
            p2.alpha = p2.alpha+delta*deltaTime(time)
            if (p2.alpha > 1.0) p2.alpha = 1.0
            if (p2.alpha < 0.0) p2.alpha = 0
        }
        else isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.changeAlphaFor(delta, duration)
}