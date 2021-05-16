/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import codechicken.lib.vec.Vector3
import mrtjp.core.fx.particles.CoreParticle
import net.minecraft.util.math.BlockPos

trait TPositionedParticle extends CoreParticle
{
//    def px:Double = xo
//    def py:Double = yo
//    def pz:Double = zo
//
//    def px_=(x:Double):Unit = { xo = x }
//    def py_=(y:Double):Unit = { yo = y }
//    def pz_=(z:Double):Unit = { zo = z }

    def dx:Double = x-xo
    def dy:Double = y-yo
    def dz:Double = z-zo

    def position = new Vector3(x, y, z)
    def prevPosition = new Vector3(xo, yo, zo)

    def setPos(pos:Vector3)
    {
        setPos(pos.x, pos.y, pos.z)
    }

    def setPrevPos(pos:Vector3)
    {
        xo = pos.x
        yo = pos.y
        zo = pos.z
    }

    def blockPosition = new BlockPos(math.floor(x).toInt, math.floor(y).toInt, math.floor(z).toInt)

    abstract override def tick()
    {
        super.tick()
        xo = x
        yo = y
        zo = z
    }
}

class PositionChangeToAction extends ParticleAction
{
    var target = Vector3.ZERO
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TPositionedParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val pp = p.asInstanceOf[TPositionedParticle]

        val pos = pp.position
        if (time < duration)
        {
            val dpos = target.copy.subtract(pos)
            val speed = dpos.copy.multiply(1/(duration-time)).multiply(deltaTime(time))
            pp.setPos(pos.add(speed))
        }
        else isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.moveTo(target.x, target.y, target.z, duration)
}

class PositionChangeForAction extends ParticleAction
{
    var delta = Vector3.ZERO
    var duration = 0.0

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TPositionedParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        val pp = p.asInstanceOf[TPositionedParticle]
        if (time < duration) pp.setPos(pp.position.add(delta.copy.multiply(deltaTime(time))))
        else isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.moveFor(delta.x, delta.y, delta.z, duration)
}
