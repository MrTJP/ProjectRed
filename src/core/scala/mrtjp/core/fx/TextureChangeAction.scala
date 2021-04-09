/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.fx

import mrtjp.core.fx.particles.CoreParticle

trait TTextureParticle extends CoreParticle
{
    var texture = ""
}

class TextureChangeAction extends ParticleAction
{
    var tex = ""

    override def canOperate(p:CoreParticle) = p.isInstanceOf[TTextureParticle]

    override def operate(p:CoreParticle, time:Double)
    {
        p.asInstanceOf[TTextureParticle].texture = tex
        isFinished = true
    }

    override def compile(p:CoreParticle){}

    override def copy = ParticleAction.changeTexture(tex)
}