package mrtjp.projectred.core.libmc.fx;

import net.minecraft.world.World;

import java.util.Random;

public abstract class ParticleLogic
{
    protected CoreParticle particle;
    protected Random rand = new Random();
    protected int priority = 1;
    protected boolean finalLogic = false;
    private boolean finished = false;
    private boolean terminateWhenFinished = false;
    protected boolean firstTick = true;

    public ParticleLogic setTerminate(boolean kill)
    {
        terminateWhenFinished = kill;
        return this;
    }

    public ParticleLogic setFinal(boolean flag)
    {
        finalLogic = flag;
        return this;
    }

    public ParticleLogic setPriority(int pri)
    {
        priority = pri;
        return this;
    }

    public boolean isTerminating()
    {
        return terminateWhenFinished;
    }

    public abstract void doUpdate();

    @Override
    public abstract ParticleLogic clone();

    public void onUpdate(World world, CoreParticle particle)
    {
        this.particle = particle;

        if (!world.isRemote)
        {
            if (particle != null)
                particle.setDead();
            return;
        }

        if (particle != null)
            doUpdate();

        if (firstTick)
            firstTick = false;
    }

    public int getPriority()
    {
        return priority;
    }

    protected void finishLogic()
    {
        finished = true;

        if (particle != null && terminateWhenFinished)
            particle.setDead();
    }

    public boolean isFinalLogic()
    {
        return finalLogic;
    }

    public boolean getFinished()
    {
        return finished;
    }
}