package mrtjp.projectred.core.client.particle;

public class ChangeAlphaToAction extends ParticleAction {

    private final double targetAlpha;
    private final double duration;

    private double deltaAlpha;

    public ChangeAlphaToAction(double targetAlpha, double duration) {
        this.targetAlpha = targetAlpha;
        this.duration = duration;
    }

    @Override
    public ParticleAction copy() {
        return new ChangeAlphaToAction(targetAlpha, duration);
    }

    @Override
    public void beginAction(BaseActionParticle particle) {
        double startAlpha = particle.getAlpha();
        deltaAlpha = (targetAlpha - startAlpha) / duration;
    }

    @Override
    public void operateAction(BaseActionParticle particle, double time) {
        double newAlpha = particle.getAlpha() + deltaAlpha * deltaTime(time);
        newAlpha = Math.max(0.0, Math.min(1.0, newAlpha));

        particle.setAlpha((float) newAlpha);

        if (time >= duration || newAlpha <= 0.0 || newAlpha >= 1.0 || equalsT(newAlpha, targetAlpha)) {
            finished = true;
        }
    }
}
