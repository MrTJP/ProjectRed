package mrtjp.projectred.core.client.particle;

public class GroupAction extends ParticleAction {

    private final ParticleAction[] actions;

    public GroupAction(ParticleAction... actions) {
        this.actions = actions;
    }

    @Override
    public void tick() {
        super.tick();
        for (ParticleAction action : actions) {
            action.tick();
        }
    }

    @Override
    public void operate(BaseActionParticle particle, float partialTick) {
        super.operate(particle, partialTick);
        boolean allFinished = true;
        for (ParticleAction action : actions) {
            action.operate(particle, partialTick);
            if (!action.isFinished()) {
                allFinished = false;
            }
        }
        finished = allFinished;
    }

    @Override
    public ParticleAction copy() {
        ParticleAction[] newActions = new ParticleAction[actions.length];
        for (int i = 0; i < actions.length; i++) {
            newActions[i] = actions[i].copy();
        }
        return new GroupAction(newActions);
    }

    @Override
    public void beginAction(BaseActionParticle particle) {
        for (ParticleAction action : actions) {
            action.beginAction(particle);
        }
    }

    @Override
    public void operateAction(BaseActionParticle particle, double time) {
        // Action does nothing
    }
}
