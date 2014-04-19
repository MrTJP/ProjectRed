package mrtjp.projectred.core.libmc.fx;

import net.minecraft.entity.EntityLiving;

public class ParticleLogicBuff extends ParticleLogic
{
    private int updateTicks;
    private int buffID;
    private EntityLiving entity;
    private int ticksWithoutBuff;

    public ParticleLogicBuff(EntityLiving entity, int buffID)
    {
        this.entity = entity;
        this.buffID = buffID;
        ticksWithoutBuff = 0;
    }

    @Override
    public void doUpdate()
    {
        updateTicks += 1;
        if (updateTicks % 10 == 0)
        {
            if (!entity.isPotionActive(buffID))
            {
                ticksWithoutBuff += 1;
                if (ticksWithoutBuff > 3)
                    particle.setDead();
            }
            else
                ticksWithoutBuff = 0;
            updateTicks = 0;
        }
    }

    @Override
    public ParticleLogic clone()
    {
        return new ParticleLogicBuff(entity, buffID).setFinal(finalLogic).setPriority(priority);
    }
}