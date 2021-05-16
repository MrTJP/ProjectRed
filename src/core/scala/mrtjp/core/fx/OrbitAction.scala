/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import codechicken.lib.vec.Vector3
import mrtjp.core.fx.particles.CoreParticle

class OrbitAction extends ParticleAction
{
    var target = Vector3.ZERO
    var speed = 0.0
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TPositionedParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val p2 = p.asInstanceOf[TPositionedParticle]

        val dp = new Vector3(p2.x, 0, p2.z).subtract(target)
        val dist = dp.mag
        val ang = math.atan2(dp.z, dp.x)+speed*deltaTime(time)
        p2.setPos(
            target.x+math.cos(ang)*dist,
            p2.y,
            target.z+math.sin(ang)*dist
        )

        if (time > duration) isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.orbitAround(target.x, target.z, speed, duration)
}
