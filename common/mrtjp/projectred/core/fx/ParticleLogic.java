package mrtjp.projectred.core.fx;

import java.util.Random;

import net.minecraft.world.World;

public abstract class ParticleLogic
{
    protected CoreParticle particle;
    protected Random rand;
    protected int priority;
    protected boolean finalLogic;
    private boolean finished;
    private boolean terminateWhenFinished;
    private boolean removeLogicWhenFinished;
    protected boolean firstTick = true;

    public ParticleLogic(int priority, boolean finalLogic)
    {
        rand = new Random();
        this.priority = priority;
        this.finalLogic = finalLogic;
        terminateWhenFinished = false;
        removeLogicWhenFinished = false;
    }

    public ParticleLogic setTerminate(boolean kill)
    {
        terminateWhenFinished = kill;
        return this;
    }

    public ParticleLogic setRemoveLogic(boolean remove)
    {
        removeLogicWhenFinished = remove;
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
        else if (removeLogicWhenFinished)
            particle.removeLogic(this);
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