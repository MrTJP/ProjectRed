package mrtjp.projectred.transportation;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public abstract class PipeLogic
{
    public static PipeLogic createPipeLogic(FlowingPipePart p, int meta)
    {
        if (meta == 0)
            return new NullPipeLogic(p);

        return new NullPipeLogic(p);
    }

    FlowingPipePart p;

    public PipeLogic(FlowingPipePart p)
    {
        this.p = p;
    }

    public void save(NBTTagCompound tag)
    {
    }

    public void load(NBTTagCompound tag)
    {
    }

    public void readDesc(MCDataInput packet)
    {
    }

    public void writeDesc(MCDataOutput packet)
    {
    }

    /**
     *
     * @param packet
     * @param key allocated >= 10
     */
    public void read(MCDataInput packet, int key)
    {
    }

    public void tick()
    {
    }

    public abstract boolean endReached(RoutedPayload r);

    public abstract boolean centerReached(RoutedPayload r);

    public abstract boolean handleDrop(RoutedPayload r);

    public abstract boolean resolveDestination(RoutedPayload r);

    public abstract Icon getIcon(int i);

    public static class NullPipeLogic extends PipeLogic
    {
        public NullPipeLogic(FlowingPipePart p)
        {
            super(p);
        }

        @Override
        public boolean endReached(RoutedPayload r)
        {
            return false;
        }

        @Override
        public boolean centerReached(RoutedPayload r)
        {
            return false;
        }

        @Override
        public boolean resolveDestination(RoutedPayload r)
        {
            return false;
        }

        @Override
        public Icon getIcon(int i)
        {
            return null;
        }

        @Override
        public boolean handleDrop(RoutedPayload r)
        {
            return false;
        }
    }
}
