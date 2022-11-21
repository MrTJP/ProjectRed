package mrtjp.projectred.fabrication.gui;

import codechicken.lib.vec.Vector3;

public class LinearVectorAnimation {

    public final Vector3 vector;
    private final Vector3 step = new Vector3();

    private long lastTime = -1;
    private long stepsRemaining = 0;

    public LinearVectorAnimation() {
        this(0, 0, 0);
    }

    public LinearVectorAnimation(double x, double y, double z) {
        vector = new Vector3(x, y, z);
    }

    public void addDeltaWithNewDuration(Vector3 delta, long duration) {
        if (delta.equals(Vector3.ZERO) || duration == 0) {
            return;
        }

        // Calculate the new step size to satisfy new delta plus previous delta within new duration
        step.multiply(stepsRemaining);
        step.add(delta);
        stepsRemaining = duration;
        step.divide(stepsRemaining);
    }

    public void tick(long time) {

        long dt = lastTime == -1 ? 0 : time - lastTime;
        lastTime = time;

        if (stepsRemaining > 0) {
            long stepsToTake = Math.min(stepsRemaining, dt);
            vector.add(step.x * stepsToTake, step.y * stepsToTake, step.z * stepsToTake);
            stepsRemaining -= stepsToTake;
        }
    }

    public void apply(Vector3 target) {
        target.add(vector);
    }
}
