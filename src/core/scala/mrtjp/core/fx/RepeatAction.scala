/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import mrtjp.core.fx.particles.CoreParticle

class RepeatAction extends ParticleAction
{
    var repeatTimes = -1
    var action:ParticleAction = null

    private var iter = 0

    override def canOperate(p:CoreParticle) =
        super.canOperate(p) && action.canOperate(p)

    override def tickLife()
    {
        super.tickLife()
        action.tickLife()
    }

    override def runOn(p:CoreParticle, frame:Float)
    {
        super.runOn(p, frame)

        if (iter < repeatTimes)
        {
            action.runOn(p, frame)
            if (action.isFinished)
            {
                iter += 1
                action.reset()
            }
        }

        if (iter >= repeatTimes)
            isFinished = true
    }

    override def operate(p:CoreParticle, time:Double){}

    override def compile(p:CoreParticle)
    {
        super.compile(p)
        action.compile(p)
    }

    override def reset()
    {
        super.reset()
        iter = 0
        action.reset()
    }

    override def copy = ParticleAction.repeat(action.copy, repeatTimes)
}

class RepeatForeverAction extends ParticleAction
{
    var action:ParticleAction = null

    override def tickLife()
    {
        super.tickLife()
        action.tickLife()
    }

    override def runOn(p:CoreParticle, frame:Float)
    {
        super.runOn(p, frame)

        action.runOn(p, frame)
        if (action.isFinished)
            action.reset()
    }

    override def operate(p:CoreParticle, time:Double){}

    override def compile(p:CoreParticle)
    {
        super.compile(p)
        action.compile(p)
    }

    override def reset()
    {
        super.reset()
        action.reset()
    }

    override def copy = ParticleAction.repeatForever(action.copy)
}