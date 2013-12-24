package mrtjp.projectred.core.fx;

import net.minecraft.util.MathHelper;
import codechicken.lib.vec.Vector3;

public class ParticleLogicApproachPoint extends ParticleLogic
{
    protected double targetX;
    protected double targetY;
    protected double targetZ;
    protected double approachSpeed;
    protected double targetDistance;
    protected boolean ignoreY;

    public ParticleLogicApproachPoint(Vector3 point, double approachSpeed, double targetDistance)
    {
        this.targetX = point.x;
        this.targetY = point.y;
        this.targetZ = point.z;
        this.approachSpeed = approachSpeed;
        this.targetDistance = targetDistance;
    }

    private double getDistanceSqToPoint(double x, double y, double z)
    {
        double var2 = particle.posX - x;
        double var4 = particle.posY - y;
        double var6 = particle.posZ - z;
        return var2 * var2 + var4 * var4 + var6 * var6;
    }

    public ParticleLogicApproachPoint setIgnoreY(boolean ignore)
    {
        ignoreY = ignore;
        return this;
    }

    @Override
    public void doUpdate()
    {
        double posX = particle.posX;
        double posZ = particle.posZ;
        double posY = particle.posY;

        double distanceToTarget = getDistanceSqToPoint(targetX, targetY, targetZ);
        double deltaZ = targetZ - particle.posZ;
        double deltaX = targetX - particle.posX;
        if (Math.abs(deltaX) > targetDistance || Math.abs(deltaZ) > targetDistance)
        {
            double angle = Math.atan2(deltaZ, deltaX);

            double radians = angle;

            posX = particle.posX + approachSpeed * Math.cos(radians);
            posZ = particle.posZ + approachSpeed * Math.sin(radians);
        }

        if (!ignoreY)
        {
            double deltaY = posY - targetY;

            double horizontalDistance = MathHelper.sqrt_double(deltaX * deltaX + deltaZ * deltaZ);
            float pitchRotation = (float) -Math.atan2(deltaY, horizontalDistance);
            double pitchRadians = pitchRotation;

            posY = particle.posY + approachSpeed * Math.sin(pitchRadians);
        }

        if (distanceToTarget <= targetDistance * targetDistance)
            onDestinationReached();
        else
            particle.setPosition(posX, posY, posZ);
    }
    
    public void onDestinationReached()
    {
        finishLogic();
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicApproachPoint(new Vector3(targetX, targetY, targetZ), approachSpeed, targetDistance).setIgnoreY(ignoreY).setFinal(finalLogic).setPriority(priority);
    }
}