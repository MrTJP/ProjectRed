package mrtjp.projectred.core.client.particle;

import net.minecraft.client.Minecraft;

import static mrtjp.projectred.core.ProjectRedCore.LOGGER;

public abstract class ParticleAction {

    protected boolean finished = false;
    private int runTime = 0;
    private double lastTime = 0;

    public void tick() {
        runTime++;
    }

    public void operate(BaseActionParticle particle, float partialTick) {
        if (!finished) {
            double t = Minecraft.getInstance().isPaused() ? lastTime : runTime + partialTick;
            if (t < lastTime) {
                LOGGER.warn("ParticleAction time went backwards! {} -> {}", lastTime, t);
                return;
            }
            operateAction(particle, t);
            lastTime = t;
        }
    }

    public boolean isFinished() {
        return finished;
    }

    public void reset() {
        finished = false;
        runTime = 0;
        lastTime = 0;
    }

    protected static boolean equalsT(double a, double b) {
        return Math.abs(a - b) < 1e-6;
    }

    protected double deltaTime(double time) {
        return time - lastTime;
    }

    public abstract ParticleAction copy();

    public abstract void beginAction(BaseActionParticle particle);

    public abstract void operateAction(BaseActionParticle particle, double time);

    //region Action factories
    public static ParticleAction group(ParticleAction... actions) {
        return new GroupAction(actions);
    }

    public static ParticleAction sequence(ParticleAction... actions) {
        return new SequenceAction(actions);
    }

    public static ParticleAction changeAlphaTo(double targetAlpha, double duration) {
        return new ChangeAlphaToAction(targetAlpha, duration);
    }

    public static ParticleAction remove() {
        return new RemoveAction();
    }
    //endregion
}
