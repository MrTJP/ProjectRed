/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import codechicken.lib.vec.Vector3
import mrtjp.core.fx.particles.CoreParticle

abstract class ParticleAction
{
    var isFinished = false
    var life = 0
    var lastTime = 0.0

    def tickLife() {life += 1}

    def canOperate(p:CoreParticle) = true

    def runOn(p:CoreParticle, frame:Float)
    {
        if (!isFinished)
        {
            val t = life+frame
            operate(p, t)
            lastTime = t
        }
    }

    def operate(p:CoreParticle, time:Double)

    def compile(p:CoreParticle){}

    def reset()
    {
        isFinished = false
        life = 0
        lastTime = 0
    }

    def deltaTime(t:Double) = t-lastTime

    def copy:ParticleAction
}

object ParticleAction
{
    def delay(ticks:Double):ParticleAction =
    {
        val a = new DelayAction
        a.delay = ticks
        a
    }

    def kill():ParticleAction = new KillAction

    def group(actions:ParticleAction*):ParticleAction =
    {
        val a = new GroupAction
        a.actions ++= actions
        a
    }

    def sequence(actions:ParticleAction*):ParticleAction =
    {
        val a = new SequenceAction
        a.actions ++= actions
        a
    }

    def repeat(action:ParticleAction, times:Int):ParticleAction =
    {
        val a = new RepeatAction
        a.action = action
        a.repeatTimes = times
        a
    }

    def repeatForever(action:ParticleAction):ParticleAction =
    {
        val a = new RepeatForeverAction
        a.action = action
        a
    }

    def moveTo(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new PositionChangeToAction
        a.target = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def moveFor(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new PositionChangeForAction
        a.delta = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def scaleTo(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new ScaleToAction
        a.target = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def scaleFor(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new ScaleForAction
        a.delta = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def changeRGBATo(r:Double, g:Double, b:Double, a:Double, duration:Double):ParticleAction =
    {
        val a1 = new ColourChangeToAction
        a1.target = new Vector3(r, g, b)
        a1.duration = duration
        val a2 = new AlphaChangeToAction
        a2.target = a
        a2.duration = duration
        group(a1, a2)
    }

    def changeRGBAFor(r:Double, g:Double, b:Double, a:Double, duration:Double):ParticleAction =
    {
        val a1 = new ColourChangeForAction
        a1.delta = new Vector3(r, g, b)
        a1.duration = duration
        val a2 = new AlphaChangeForAction
        a2.delta = a
        a2.duration = duration
        group(a1, a2)
    }

    def changeColourTo(r:Double, g:Double, b:Double, duration:Double):ParticleAction =
    {
        val a = new ColourChangeToAction
        a.target = new Vector3(r, g, b)
        a.duration = duration
        a
    }

    def changeColourFor(r:Double, g:Double, b:Double, duration:Double):ParticleAction =
    {
        val a = new ColourChangeForAction
        a.delta = new Vector3(r, g, b)
        a.duration = duration
        a
    }

    def changeAlphaTo(alpha:Double, duration:Double):ParticleAction =
    {
        val a = new AlphaChangeToAction
        a.target = alpha
        a.duration = duration
        a
    }

    def changeAlphaFor(alpha:Double, duration:Double):ParticleAction =
    {
        val a = new AlphaChangeForAction
        a.delta = alpha
        a.duration = duration
        a
    }

    def fadeIn(duration:Double):ParticleAction = changeAlphaTo(1, duration)

    def fadeOut(duration:Double):ParticleAction = changeAlphaTo(0, duration)

    def changeTexture(texture:String):ParticleAction =
    {
        val a = new TextureChangeAction
        a.tex = texture
        a
    }

    def changeTargetTo(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new TargetChangeToAction
        a.target = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def changeTargetFor(x:Double, y:Double, z:Double, duration:Double):ParticleAction =
    {
        val a = new TargetChangeForAction
        a.delta = new Vector3(x, y, z)
        a.duration = duration
        a
    }

    def orbitAround(x:Double, z:Double, speed:Double, duration:Double):ParticleAction =
    {
        val a = new OrbitAction
        a.target = new Vector3(x, 0, z)
        a.duration = duration
        a.speed = speed
        a
    }
}