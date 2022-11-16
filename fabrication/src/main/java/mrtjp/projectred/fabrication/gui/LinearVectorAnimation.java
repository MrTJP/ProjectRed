package mrtjp.projectred.fabrication.gui;

import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;

public class LinearVectorAnimation {

    public final Vector3 vector;

    private final Cuboid6 bounds = new Cuboid6(Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY, Double.NEGATIVE_INFINITY, Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY, Double.POSITIVE_INFINITY);
    private final Vector3 step = new Vector3();

    private long lastTime = -1;
    private long stepsRemaining = 0;

    public LinearVectorAnimation() {
        this(0, 0, 0);
    }

    public LinearVectorAnimation(double x, double y, double z) {
        vector = new Vector3(x, y, z);
    }

    public void setBounds(double xmin, double ymin, double zmin, double xmax, double ymax, double zmax) {
        bounds.min.set(xmin, ymin, zmin);
        bounds.max.set(xmax, ymax, zmax);
        // If now outside, jump into bounds
        if (!bounds.contains(vector)) {
            vector.x = Math.max(bounds.min.x, Math.min(bounds.max.x, vector.x));
            vector.y = Math.max(bounds.min.y, Math.min(bounds.max.y, vector.y));
            vector.z = Math.max(bounds.min.z, Math.min(bounds.max.z, vector.z));
        }
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

            // Contain within bounds
            vector.x = Math.max(bounds.min.x, Math.min(bounds.max.x, vector.x));
            vector.y = Math.max(bounds.min.y, Math.min(bounds.max.y, vector.y));
            vector.z = Math.max(bounds.min.z, Math.min(bounds.max.z, vector.z));
        }
    }

    public void apply(Vector3 target) {
        target.add(vector);
    }
}
