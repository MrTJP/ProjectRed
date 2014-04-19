package mrtjp.projectred.core.libmc.fx;


public class ParticleLogicTrail extends ParticleLogic
{
    private int ticksBetweenSpawns;
    private int updateTicks;
    private final String particleName;
    private IParticleBuilder builder;

    public ParticleLogicTrail(String particleName, IParticleBuilder builder)
    {
        ticksBetweenSpawns = 2;
        this.particleName = particleName;
        this.builder = builder;
        updateTicks = 0;
    }

    public ParticleLogicTrail setTicksBetweenSpawns(int ticks)
    {
        ticksBetweenSpawns = ticks;
        return this;
    }

    @Override
    public void doUpdate()
    {
        updateTicks += 1;
        if (updateTicks == ticksBetweenSpawns)
        {
            updateTicks = 0;
            CoreParticle c = ParticleManagement.instance.spawn(particle.worldObj, particleName, particle.posX, particle.posY, particle.posZ);
            if (c != null)
                builder.build(c);
        }
    }

    @Override
    public ParticleLogic clone()
    {
        ParticleLogicTrail clone = new ParticleLogicTrail(particleName, builder).setTicksBetweenSpawns(ticksBetweenSpawns);
        return clone;
    }

    public static interface IParticleBuilder
    {
        public void build(CoreParticle c);
    }
}