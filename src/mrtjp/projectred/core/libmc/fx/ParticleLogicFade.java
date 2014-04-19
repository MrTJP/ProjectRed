package mrtjp.projectred.core.libmc.fx;

public final class ParticleLogicFade extends ParticleLogic
{
    private float fadeSpeed;
    private float fadeAccel;

    public ParticleLogicFade()
    {
        fadeSpeed = 0.01F;
        fadeAccel = 0;
    }

    public ParticleLogicFade setFadeSpeed(float fadeSpeed, float fadeAccel)
    {
        this.fadeSpeed = fadeSpeed;
        this.fadeAccel = fadeAccel;
        return this;
    }

    @Override
    public void doUpdate()
    {
        float alpha = particle.a();
        if (alpha <= 0.0F)
        {
            finishLogic();
            return;
        }
        particle.a_$eq(alpha - fadeSpeed);

        fadeSpeed += fadeAccel;
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicFade().setFadeSpeed(fadeSpeed, fadeAccel).setFinal(finalLogic).setPriority(priority);
    }
}