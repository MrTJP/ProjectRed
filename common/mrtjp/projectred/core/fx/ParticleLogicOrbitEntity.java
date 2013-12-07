package mrtjp.projectred.core.fx;

import java.util.Random;

import net.minecraft.client.entity.EntityClientPlayerMP;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;

public final class ParticleLogicOrbitEntity extends ParticleLogic
{
    private final Entity target;
    private double distance;
    private final boolean rotateClockwise;
    private double targetY;
    private double curYOffset;
    private double targetDistance;
    private final double orbitSpeed;
    private double orbitAngle;
    private double orbitY = -512.0D;
    private boolean ignoreYCoordinate = false;

    public ParticleLogicOrbitEntity(Entity orbitTarget, double orbitSpeed, int priority, boolean finalLogic) {
        super(priority, finalLogic);
        target = orbitTarget;
        orbitAngle = rand.nextInt(360);
        rotateClockwise = rand.nextBoolean();
        generateNewTargetY();
        targetDistance = 1.0D + rand.nextDouble() * 0.5D;
        this.orbitSpeed = orbitSpeed;
    }

    public ParticleLogicOrbitEntity setOrbitY(double orbitY) {
        this.orbitY = orbitY;
        return this;
    }

    public ParticleLogicOrbitEntity setTargetDistance(double targetDistance) {
        this.targetDistance = targetDistance;
        return this;
    }

    private void generateNewTargetY() {
        if (target != null)
            targetY = new Random().nextDouble() * target.height;
        else
            targetY = 0.0D;
    }

    private void generateNewDistance() {
        if (target != null)
            targetDistance = new Random().nextDouble() * 2.0D;
        else
            targetDistance = 0.0D;
    }

    @Override
    public void doUpdate() {
        if (firstTick)
            curYOffset = particle.posY - (target.posY + target.getEyeHeight());

        if (target == null || target.isDead) {
            finishLogic();
            return;
        }

        double posY = particle.posY;

        if (Math.abs(targetY - curYOffset) < 0.1D)
            generateNewTargetY();

        double posX = target.posX + Math.cos(orbitAngle) * targetDistance;
        double posZ = target.posZ + Math.sin(orbitAngle) * targetDistance;

        if (targetY < curYOffset)
            curYOffset -= orbitSpeed / 4.0D;
        else if (targetY > curYOffset)
            curYOffset += orbitSpeed / 4.0D;

        if (rotateClockwise)
            orbitAngle += orbitSpeed;
        else
            orbitAngle -= orbitSpeed;
        if (orbitAngle > 360.0D)
            orbitAngle -= 360.0D;
        else if (orbitAngle < 0.0D)
            orbitAngle += 360.0D;

        if (!ignoreYCoordinate)
            if (orbitY != -512.0D)
                posY = target.posY + target.getEyeHeight() + orbitY;
            else {
                int offset = 0;
                if (target instanceof EntityPlayer && !(target instanceof EntityClientPlayerMP))
                    offset = (int) (offset + 2.0F * target.height);
                posY = target.posY - target.getEyeHeight() + curYOffset + offset;
            }

        particle.setPosition(posX, posY, posZ);
    }

    @Override
    public ParticleLogic clone() {
        ParticleLogicOrbitEntity clone = new ParticleLogicOrbitEntity(target, orbitSpeed, priority, rotateClockwise).setTargetDistance(targetDistance);
        if (orbitY != -512.0D)
            clone.setOrbitY(orbitY);
        clone.setIgnoreYCoordinate(ignoreYCoordinate);
        return clone;
    }

    public ParticleLogicOrbitEntity setIgnoreYCoordinate(boolean b) {
        ignoreYCoordinate = b;
        return this;
    }
}