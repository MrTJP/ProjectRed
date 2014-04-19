package mrtjp.projectred.core.libmc.fx;

import mrtjp.projectred.core.libmc.MathLib;
import net.minecraft.entity.Entity;
import codechicken.lib.vec.Vector3;

public class ParticleLogicArcToEntity extends ParticleLogic
{
    private Vector3 start;
    private Entity target;
    private Vector3 firstControl;
    private Vector3 secondControl;
    private float percent;
    private float speed;
    private float offsetFactor;
    private float halfOffsetFactor;

    public ParticleLogicArcToEntity(Vector3 start, Entity target)
    {
        this.start = start.copy();
        percent = 0.0F;
        speed = 0.03F;
        offsetFactor = 10.0F;
        halfOffsetFactor = offsetFactor / 2.0F;
        this.target = target;

        generateControlPoints();
    }

    public ParticleLogicArcToEntity generateControlPoints()
    {
        firstControl = new Vector3(start.x + (target.posX - start.x) / 3.0D, start.y + (target.posY - start.y) / 3.0D, start.z + (target.posZ - start.z) / 3.0D);

        secondControl = new Vector3(start.x + (target.posX - start.x) / 3.0D * 2.0D, start.y + (target.posY - start.y) / 3.0D * 2.0D, start.z + (target.posZ - start.z) / 3.0D * 2.0D);

        double offsetX = rand.nextFloat() * offsetFactor - halfOffsetFactor;
        double offsetZ = rand.nextFloat() * offsetFactor - halfOffsetFactor;

        Vector3 offset = new Vector3(offsetX, 0.0D, offsetZ);

        firstControl = firstControl.add(offset);
        secondControl = secondControl.add(offset);

        return this;
    }

    public ParticleLogicArcToEntity setControlPoints(Vector3 first, Vector3 second)
    {
        firstControl = first;
        secondControl = second;
        return this;
    }

    public ParticleLogicArcToEntity setSpeed(float speed)
    {
        this.speed = speed;
        return this;
    }

    @Override
    public void doUpdate()
    {
        percent += speed;
        if (percent >= 1.0F)
        {
            finishLogic();
            return;
        }
        Vector3 bez = MathLib.bezier(start, firstControl, secondControl, new Vector3(target.posX, target.posY, target.posZ).add(new Vector3(0.0D, target.getEyeHeight(), 0.0D)), percent);
        particle.setPosition(bez.x, bez.y, bez.z);
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicArcToEntity(particle.position(), target).setSpeed(speed).setControlPoints(firstControl, secondControl).setFinal(finalLogic).setPriority(priority);
    }
}