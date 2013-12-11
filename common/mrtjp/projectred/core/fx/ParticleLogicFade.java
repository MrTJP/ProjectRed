package mrtjp.projectred.core.fx;

public final class ParticleLogicFade extends ParticleLogic
{
    private float fadeSpeed;
    private float fadeAccel;

    public ParticleLogicFade(int priority, boolean finalLogic)
    {
        super(priority, finalLogic);
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
        float alpha = particle.getAlpha();
        if (alpha <= 0.0F)
        {
            finishLogic();
            return;
        }
        particle.setAlpha(alpha - fadeSpeed);

        fadeSpeed += fadeAccel;
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicFade(priority, finalLogic).setFadeSpeed(fadeSpeed, fadeAccel);
    }
}