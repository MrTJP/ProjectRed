package mrtjp.projectred.core.fx;

import java.util.Random;

import codechicken.lib.vec.Vector3;

public final class ParticleLogicOrbitPoint extends ParticleLogic
{
    private Vector3 target;
    private double distance;
    private boolean rotateClockwise;
    private double targetY;
    private double targetDistance;
    private double orbitSpeed;
    private double orbitAngle;
    private double orbitY = -512.0D;
    private boolean useCurrentDistance;
    private boolean ignoreYCoord;
    private boolean shrinkingOrbit;
    private double shrinkSpeed = 0.0D;
    private double shrinkTargetDistance = 0.0D;

    public ParticleLogicOrbitPoint(Vector3 point)
    {
        target = point.copy();
        orbitAngle = rand.nextInt(360);
        rotateClockwise = rand.nextInt(10) < 5;
        generateNewTargetY();
        targetDistance = 1.0D + rand.nextDouble() * 0.5D;
    }

    public ParticleLogicOrbitPoint setOrbitY(double orbitY)
    {
        this.orbitY = orbitY;
        return this;
    }

    public ParticleLogicOrbitPoint setIgnoreYCoordinate(boolean ignore)
    {
        ignoreYCoord = ignore;
        return this;
    }

    public ParticleLogicOrbitPoint setTargetDistance(double targetDistance)
    {
        this.targetDistance = targetDistance;
        useCurrentDistance = false;
        return this;
    }

    public ParticleLogicOrbitPoint setUseCurrentDistance()
    {
        useCurrentDistance = true;
        return this;
    }

    public ParticleLogicOrbitPoint setOrbitSpeed(double speed)
    {
        orbitSpeed = speed;
        return this;
    }

    public ParticleLogicOrbitPoint setShrinkingOrbit(double shrinkSpeed, double newTargetDistance)
    {
        shrinkingOrbit = true;
        this.shrinkSpeed = shrinkSpeed;
        shrinkTargetDistance = newTargetDistance;
        return this;
    }

    private void generateNewTargetY()
    {
        if (target != null)
            targetY = new Random().nextDouble() * 2.0D;
        else
            targetY = 0.0D;
    }

    private void generateNewDistance()
    {
        if (target != null)
            targetDistance = new Random().nextDouble() * 2.0D;
        else
            targetDistance = 0.0D;
    }

    public ParticleLogicOrbitPoint setRotateDirection(boolean clockwise)
    {
        rotateClockwise = clockwise;
        return this;
    }

    public ParticleLogicOrbitPoint setStartAngle(float angle)
    {
        orbitAngle = angle;
        return this;
    }

    @Override
    public void doUpdate()
    {
        double posY = particle.posY;

        double relativeTargetY = target.y + targetY;

        if (!ignoreYCoord && Math.abs(particle.posY - relativeTargetY) < 0.1D)
            generateNewTargetY();

        double posX;
        double posZ;
        if (useCurrentDistance)
        {
            double deltaz = target.z - particle.posZ;
            double deltax = target.x - particle.posX;
            double currentDistance = Math.sqrt(deltaz * deltaz + deltax * deltax);
            posX = target.x + Math.cos(orbitAngle) * currentDistance;
            posZ = target.z + Math.sin(orbitAngle) * currentDistance;
        }
        else
        {
            if (shrinkingOrbit)
                if (targetDistance <= shrinkTargetDistance)
                    shrinkingOrbit = false;
                else if (targetDistance < shrinkTargetDistance + shrinkSpeed * 10.0D)
                {
                    double delta = targetDistance - shrinkTargetDistance;
                    targetDistance -= delta * shrinkSpeed;
                }
                else
                    targetDistance -= shrinkSpeed;

            posX = target.x + Math.cos(orbitAngle) * targetDistance;
            posZ = target.z + Math.sin(orbitAngle) * targetDistance;
        }

        if (!ignoreYCoord)
            if (particle.posY < relativeTargetY)
                particle.posY += 0.1D;
            else if (particle.posY > relativeTargetY)
                particle.posY -= 0.1D;

        if (rotateClockwise)
            orbitAngle += orbitSpeed;
        else
            orbitAngle -= orbitSpeed;
        if (orbitAngle > 360.0D)
            orbitAngle -= 360.0D;
        else if (orbitAngle < 0.0D)
            orbitAngle += 360.0D;

        if (orbitY != -512.0D)
            posY = target.y + orbitY;

        particle.setPosition(posX, posY, posZ);
    }

    @Override
    public ParticleLogic clone()
    {
        ParticleLogicOrbitPoint clone = new ParticleLogicOrbitPoint(target);
        if (useCurrentDistance)
            clone.setUseCurrentDistance();
        else
            clone.setTargetDistance(targetDistance);

        if (orbitY != -512.0D)
            clone.setOrbitY(orbitY);

        clone.setOrbitSpeed(orbitSpeed);
        clone.setIgnoreYCoordinate(ignoreYCoord);
        clone.setRotateDirection(rotateClockwise);
        clone.setStartAngle((float) orbitAngle);

        if (shrinkingOrbit)
            clone.setShrinkingOrbit(shrinkSpeed, shrinkTargetDistance);

        return clone.setFinal(finalLogic).setPriority(priority);
    }
}