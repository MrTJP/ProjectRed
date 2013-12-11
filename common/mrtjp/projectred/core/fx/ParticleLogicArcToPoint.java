package mrtjp.projectred.core.fx;

import mrtjp.projectred.core.utils.MathLib;
import codechicken.lib.vec.Vector3;

public class ParticleLogicArcToPoint extends ParticleLogic
{
    private final Vector3 start;
    private final Vector3 target;
    private Vector3 firstControl;
    private Vector3 secondControl;
    private float percent;
    private float speed;
    private final float offsetFactor;
    private final float halfOffsetFactor;

    public ParticleLogicArcToPoint(Vector3 start, Vector3 end, int priority, boolean finalLogic)
    {
        super(priority, finalLogic);
        this.start = start.copy();
        this.target = end.copy();
        percent = 0.0F;
        speed = 0.03F;
        offsetFactor = 10.0F;
        halfOffsetFactor = offsetFactor / 2.0F;
        generateControlPoints();
    }

    public ParticleLogicArcToPoint generateControlPoints()
    {
        firstControl = new Vector3(start.x + (target.x - start.x) / 3.0F, start.y + (target.y - start.y) / 3.0F, start.z + (target.z - start.z) / 3.0F);

        secondControl = new Vector3(start.x + (target.x - start.x) / 3.0F * 2.0F, start.y + (target.y - start.y) / 3.0F * 2.0F, start.z + (target.z - start.z) / 3.0F * 2.0F);

        double offsetX = rand.nextFloat() * offsetFactor - halfOffsetFactor;
        double offsetZ = rand.nextFloat() * offsetFactor - halfOffsetFactor;
        double offsetY = rand.nextFloat() * offsetFactor - halfOffsetFactor;

        Vector3 offset = new Vector3(offsetX, offsetY, offsetZ);

        firstControl = firstControl.add(offset);
        secondControl = secondControl.add(offset);

        return this;
    }

    public ParticleLogicArcToPoint setControlPoints(Vector3 first, Vector3 second)
    {
        firstControl = first;
        secondControl = second;
        return this;
    }

    public ParticleLogicArcToPoint setSpeed(float speed)
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
        Vector3 bez = MathLib.bezier(start, firstControl, secondControl, target, percent);
        particle.setPosition(bez.x, bez.y, bez.z);
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicArcToPoint(particle.position(), target, priority, finalLogic).setSpeed(speed).setControlPoints(firstControl, secondControl);
    }
}