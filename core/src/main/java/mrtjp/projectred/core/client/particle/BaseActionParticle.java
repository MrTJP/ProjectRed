package mrtjp.projectred.core.client.particle;

import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.Particle;

import javax.annotation.Nullable;
import java.util.LinkedList;

public abstract class BaseActionParticle extends Particle {

    private final LinkedList<ParticleAction> actionQueue = new LinkedList<>();
    private @Nullable ParticleAction currentAction = null;

    public BaseActionParticle(ClientLevel pLevel, double pX, double pY, double pZ) {
        super(pLevel, pX, pY, pZ);
    }

    public void addAction(ParticleAction action) {
        actionQueue.add(action);
    }

    protected void tickActions() {
        if (currentAction != null) {
            currentAction.tick();
        }
    }

    protected void runActions(float partialTick) {
        if (currentAction == null) {
            if (!actionQueue.isEmpty()) {
                currentAction = actionQueue.poll();
                currentAction.reset();
                currentAction.beginAction(this);
            }
        }

        if (currentAction != null) {
            currentAction.operate(this, partialTick);
            if (currentAction.isFinished()) {
                currentAction = null;
            }
        }
    }

    @Override
    public void tick() {
        super.tick();
        tickActions();
    }

    //region Action accessors
    public float getAlpha() {
        return alpha;
    }

    @Override
    public void setAlpha(float alpha) {
        this.alpha = alpha;
    }
    //endregion
}
