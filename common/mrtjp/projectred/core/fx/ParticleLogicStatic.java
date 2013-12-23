package mrtjp.projectred.core.fx;

public class ParticleLogicStatic extends ParticleLogic
{
    private int ticksRun = 0;
    private int ticksToStay = 0;
    private boolean indefinate = true;
    
    public ParticleLogicStatic setTime(int time)
    {
        indefinate = time <= 0;
        ticksToStay = time;
        return this;
    }
    
    @Override
    public void doUpdate()
    {
        if (indefinate)
            return;
        
        ticksRun += 1;
        if (ticksRun == ticksToStay)
            finishLogic();
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicStatic().setTime(ticksToStay).setFinal(finalLogic).setPriority(priority);
    }
}