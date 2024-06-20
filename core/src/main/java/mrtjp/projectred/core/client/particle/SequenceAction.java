package mrtjp.projectred.core.client.particle;

public class SequenceAction extends ParticleAction {

    private final ParticleAction[] actions;
    private int index = 0;

    public SequenceAction(ParticleAction... actions) {
        this.actions = actions;
    }

    @Override
    public void tick() {
        super.tick();
        if (index < actions.length) {
            actions[index].tick();
        }
    }

    @Override
    public void operate(BaseActionParticle particle, float partialTick) {
        super.operate(particle, partialTick);

        // Shift to next action if needed
        if (actions[index].isFinished()) {
            index++;
            if (index >= actions.length) {
                finished = true;
                return;
            }
            actions[index].beginAction(particle);
        }

        if (index < actions.length) {
            actions[index].operate(particle, partialTick);
        }
    }

    @Override
    public ParticleAction copy() {
        ParticleAction[] newActions = new ParticleAction[actions.length];
        for (int i = 0; i < actions.length; i++) {
            newActions[i] = actions[i].copy();
        }
        return new SequenceAction(newActions);
    }

    @Override
    public void beginAction(BaseActionParticle particle) {
        index = 0;
        actions[index].beginAction(particle);
    }

    @Override
    public void operateAction(BaseActionParticle particle, double time) {
        // Action does nothing
    }
}
