package mrtjp.projectred.core.fx;

public class ParticleLogicStatic extends ParticleLogic
{
    private int ticksRun;
    private int ticksToStay;

    public ParticleLogicStatic(int delay, int priority, boolean finalLogic) {
        super(priority, finalLogic);
        ticksRun = 0;
        this.ticksToStay = delay;
    }

    @Override
    public void doUpdate() {
        ticksRun += 1;
        if (ticksRun == ticksToStay)
            finishLogic();
    }

    @Override
    public ParticleLogic clone() {
        return new ParticleLogicStatic(ticksToStay, priority, finalLogic);
    }
}