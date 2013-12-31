package mrtjp.projectred.transportation;

import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public abstract class PipeLogic
{
    public static PipeLogic createPipeLogic(BasicPipePart p, int meta)
    {
        if (meta == 0)
            return new NullPipeLogic(p);
                
        return new NullPipeLogic(p);
    }

    BasicPipePart p;

    public PipeLogic(BasicPipePart p)
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

    public void read(MCDataInput packet, int switch_key)
    {
    }

    public void tick()
    {
    }

    public abstract boolean endReached(RoutedPayload r);

    public abstract boolean centerReached(RoutedPayload r);

    public abstract boolean handleDrop(RoutedPayload r);

    public abstract boolean resolveDestination(RoutedPayload r);

    public abstract Icon getIcon(BasicPipePart p, int i);

    public static class NullPipeLogic extends PipeLogic
    {
        public NullPipeLogic(BasicPipePart p)
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
        public Icon getIcon(BasicPipePart p, int i)
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
