package mrtjp.projectred.core.client.particle;

public class RemoveAction extends ParticleAction {

    public RemoveAction() {
    }

    @Override
    public ParticleAction copy() {
        return new RemoveAction();
    }

    @Override
    public void beginAction(BaseActionParticle particle) {
    }

    @Override
    public void operateAction(BaseActionParticle particle, double time) {
        particle.remove();
        finished = true;
    }
}
