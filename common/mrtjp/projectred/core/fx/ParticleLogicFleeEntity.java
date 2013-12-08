package mrtjp.projectred.core.fx;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.util.MathHelper;

public class ParticleLogicFleeEntity extends ParticleLogic
{
    private Entity target;
    private double fleeSpeed;
    private double targetDistance;

    public ParticleLogicFleeEntity(Entity fleeEntity, double fleeSpeed, double targetDistance, int priority, boolean finalLogic) {
        super(priority, finalLogic);
        target = fleeEntity;
        this.fleeSpeed = fleeSpeed;
        this.targetDistance = targetDistance;
    }

    @Override
    public void doUpdate() {

        double distanceToTarget = particle.getDistanceToEntity(target);

        double deltaZ = particle.posZ - target.posZ;
        double deltaX = particle.posX - target.posX;
        double deltaY;
        double angle = Math.atan2(deltaZ, deltaX);
        double radians = angle;

        double posX = particle.posX + fleeSpeed * Math.cos(radians);
        double posY = particle.posY;
        double posZ = particle.posZ + fleeSpeed * Math.sin(radians);

        if (target instanceof EntityLiving) {
            EntityLiving entityliving = (EntityLiving) target;
            deltaY = posY - (entityliving.posY + entityliving.getEyeHeight());
        } else
            deltaY = (target.boundingBox.minY + target.boundingBox.maxY) / 2.0D - posY;
        double horizontalDistance = MathHelper.sqrt_double(deltaX * deltaX + deltaZ * deltaZ);
        float pitchRotation = (float) -Math.atan2(deltaY, horizontalDistance);
        double pitchRadians = pitchRotation;

        posY = particle.posY + fleeSpeed * Math.sin(pitchRadians);

        if (distanceToTarget > targetDistance)
            finishLogic();
        else
            particle.setPosition(posX, posY, posZ);
    }

    @Override
    public ParticleLogic clone() {
        return new ParticleLogicFleeEntity(target, fleeSpeed, targetDistance, priority, finalLogic);
    }
}