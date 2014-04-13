package mrtjp.projectred.core.fx;

import java.util.ArrayList;
import java.util.List;

import net.minecraft.util.Icon;
import net.minecraft.util.MathHelper;

public class ParticleLogicIconShift extends ParticleLogic
{
    private List<Icon> icons = new ArrayList<Icon>();
    private int tickDelay = 0;
    private int ticksRemaining = 0;

    private int iconIndex = 0;

    public static ParticleLogicIconShift fluttering()
    {
        ParticleLogicIconShift logic = new ParticleLogicIconShift().addIcon("flutter1").addIcon("flutter2").addIcon("flutter3").addIcon("flutter4");
        logic.setTicksBetweenChange(3);
        return logic;
    }

    public ParticleLogicIconShift addIcon(String icon)
    {
        icons.add(ParticleIconRegistry.instance.getIcon(icon));
        return this;
    }

    public ParticleLogicIconShift setTicksBetweenChange(int ticks)
    {
        tickDelay = ticksRemaining = ticks;
        return this;
    }

    @Override
    public void doUpdate()
    {
        if (--ticksRemaining > 0)
            return;
        ticksRemaining = tickDelay;

        if (icons.isEmpty())
            return;

        shiftIconIndex();
        shiftDelay();

        particle.setParticleIcon(icons.get(iconIndex));
    }

    private void shiftIconIndex()
    {
        iconIndex = (iconIndex + 1) % icons.size();
    }

    private void shiftDelay()
    {
        int newDelay = tickDelay + MathHelper.getRandomIntegerInRange(rand, -1, 1);
        setTicksBetweenChange(newDelay);
    }

    @Override
    public ParticleLogic clone()
    {
        ParticleLogicIconShift clone = new ParticleLogicIconShift();
        clone.setTicksBetweenChange(tickDelay);
        for (Icon icon : icons)
            clone.icons.add(icon);

        return clone.setFinal(finalLogic).setPriority(priority);
    }
}
