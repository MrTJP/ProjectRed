package mrtjp.projectred.integration;

import codechicken.lib.raytracer.IndexedCuboid6;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.MovingObjectPosition;

import java.util.List;

public abstract class GateLogic<PartType extends GatePart>
{
    public static interface ITimerGuiLogic
    {
        public int getTimerMax();

        public void setTimerMax(GatePart gate, int t);
    }

    public static interface ICounterGuiLogic
    {
        public int getCounterMax();

        public void setCounterMax(GatePart gate, int i);

        public int getCounterIncr();

        public void setCounterIncr(GatePart gate, int i);

        public int getCounterDecr();

        public void setCounterDecr(GatePart gate, int i);

        public int getCounterValue();

        public void setCounterValue(GatePart gate, int i);
    }

    public abstract boolean canConnectTo(PartType gate, IConnectable part, int r);

    public boolean cycleShape(PartType gate)
    {
        return false;
    }

    public abstract void onChange(PartType gate);

    public abstract void scheduledTick(PartType gate);

    public void onTick(PartType gate)
    {
    }

    public void setup(PartType part)
    {
    }

    /**
     * Beware you don't actually do anything client side.
     *
     * @return true if the event is consumed by this part
     */
    public boolean activate(PartType part, EntityPlayer player, ItemStack held, MovingObjectPosition hit)
    {
        return false;
    }

    public void addSubParts(PartType part, List<IndexedCuboid6> list)
    {
    }

    public int lightLevel()
    {
        return 7;
    }
}
