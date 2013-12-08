package mrtjp.projectred.core.fx;

import java.util.ArrayList;

public class ParticleLogicTrail extends ParticleLogic
{
    private int ticksBetweenSpawns;
    private int updateTicks;
    private final String particleName;
    private final boolean ignoreMaxAge;
    private final int maxAge;
    private float r;
    private float g;
    private float b;
    private final ArrayList<ParticleLogic> logics;
    private float offsetX;
    private float offsetY;
    private float offsetZ;

    public ParticleLogicTrail(String particleName, boolean ignoreMaxAge, int maxAge, int priority, boolean finalLogic) {
        super(priority, finalLogic);
        logics = new ArrayList();
        ticksBetweenSpawns = 2;
        this.particleName = particleName;
        this.ignoreMaxAge = ignoreMaxAge;
        this.maxAge = maxAge;
        updateTicks = 0;
    }

    public ParticleLogicTrail setTicksBetweenSpawns(int ticks) {
        ticksBetweenSpawns = ticks;
        return this;
    }

    public ParticleLogicTrail addLogicForTrail(ParticleLogic logic) {
        logics.add(logic);
        return this;
    }

    public ParticleLogicTrail setParticleRGB(float red, float green, float blue) {
        r = red;
        g = green;
        b = blue;
        return this;
    }

    public ParticleLogicTrail addRandomOffset(float x, float y, float z) {
        offsetX = x;
        offsetY = y;
        offsetZ = z;

        return this;
    }

    @Override
    public void doUpdate() {
        updateTicks += 1;
        if (updateTicks == ticksBetweenSpawns) {
            updateTicks = 0;
            CoreParticle c = ParticleManagement.instance.spawn(particle.worldObj, particleName, particle.posX, particle.posY, particle.posZ);
            if (c != null) {
                c.setMaxAge(maxAge);
                c.setIgnoreMaxAge(ignoreMaxAge);
                c.setRGBColorF(r, g, b);
                c.addRandomOffset(offsetX, offsetY, offsetZ);
                for (ParticleLogic logic : logics)
                    c.addLogic(logic.clone().setTerminate(logic.isTerminating()));
            }
        }
    }

    @Override
    public ParticleLogic clone() {
        ParticleLogicTrail clone = new ParticleLogicTrail(particleName, ignoreMaxAge, maxAge, priority, finalLogic).setParticleRGB(r, g, b).setTicksBetweenSpawns(ticksBetweenSpawns).addRandomOffset(offsetX, offsetY, offsetZ);

        for (ParticleLogic logic : logics)
            clone.addLogicForTrail(logic);

        return clone;
    }
}