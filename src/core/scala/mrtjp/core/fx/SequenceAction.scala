/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import mrtjp.core.fx.particles.CoreParticle

import scala.collection.mutable.{Seq => MSeq}

class SequenceAction extends ParticleAction
{
    var actions = MSeq[ParticleAction]()

    override def tickLife()
    {
        super.tickLife()
        actions.find(!_.isFinished) match
        {
            case Some(action) => action.tickLife()
            case None =>
        }
    }

    override def runOn(p:CoreParticle, frame:Float)
    {
        super.runOn(p, frame)

        actions.find(!_.isFinished) match
        {
            case Some(action) => action.runOn(p, frame)
            case None =>
                isFinished = true
                return
        }

        if (actions.forall(_.isFinished))
            isFinished = true
    }

    override def operate(p:CoreParticle, time:Double){}

    override def compile(p:CoreParticle)
    {
        super.compile(p)
        actions.foreach(_.compile(p))
    }

    override def reset()
    {
        super.reset()
        actions.foreach(_.reset())
    }

    override def copy = ParticleAction.sequence(actions.map(_.copy).toList:_*)
}
