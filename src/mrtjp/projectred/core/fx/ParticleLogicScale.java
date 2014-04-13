package mrtjp.projectred.core.fx;

public class ParticleLogicScale extends ParticleLogic
{
    private float scaleRate = 0.01F;
    private float scaleAccel = 0F;

    public ParticleLogicScale setRate(float scaleRate, float scaleAccel)
    {
        this.scaleRate = scaleRate;
        this.scaleAccel = scaleAccel;
        return this;
    }

    @Override
    public void doUpdate()
    {
        float newScale = particle.getScaleX() + scaleRate;
        if (newScale <= 0.0F)
        {
            finishLogic();
            return;
        }

        particle.setScale(newScale);

        scaleRate += scaleAccel;
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicScale().setRate(scaleRate, scaleAccel).setFinal(finalLogic).setPriority(priority);
    }
}