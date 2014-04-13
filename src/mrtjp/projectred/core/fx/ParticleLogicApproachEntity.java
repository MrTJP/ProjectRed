package mrtjp.projectred.core.fx;

import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityLiving;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.util.MathHelper;

public class ParticleLogicApproachEntity extends ParticleLogic
{
    private final Entity target;
    private final double approachSpeed;
    private final double targetDistance;

    public ParticleLogicApproachEntity(Entity approachEntity, double approachSpeed, double targetDistance)
    {
        target = approachEntity;
        this.approachSpeed = approachSpeed;
        this.targetDistance = targetDistance;
    }

    @Override
    public void doUpdate()
    {
        if (target == null)
        {
            finishLogic();
            return;
        }

        double distanceToTarget = particle.getDistanceSqToEntity(target);
        double deltaX = target.posX - particle.posX;
        double deltaY;
        double deltaZ = target.posZ - particle.posZ;
        double angle = Math.atan2(deltaZ, deltaX);

        double radians = angle;

        double posX = particle.posX + approachSpeed * Math.cos(radians);
        double posY = particle.posY;
        double posZ = particle.posZ + approachSpeed * Math.sin(radians);
        if (target instanceof EntityLiving)
        {
            EntityLiving entityliving = (EntityLiving) target;
            deltaY = posY - (entityliving.posY + entityliving.getEyeHeight());
        }
        else if (target instanceof EntityItem)
            deltaY = posY - target.posY;
        else
            deltaY = (target.boundingBox.minY + target.boundingBox.maxY) / 2.0D - posY;
        double horizontalDistance = MathHelper.sqrt_double(deltaX * deltaX + deltaZ * deltaZ);
        float pitchRotation = (float) -Math.atan2(deltaY, horizontalDistance);
        double pitchRadians = pitchRotation;

        posY = particle.posY - approachSpeed * Math.sin(pitchRadians);

        if (distanceToTarget <= targetDistance * targetDistance)
            finishLogic();
        else
            particle.setPosition(posX, posY, posZ);
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicApproachEntity(target, approachSpeed, targetDistance).setFinal(finalLogic).setPriority(priority);
    }
}