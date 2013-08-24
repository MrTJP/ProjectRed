package mrtjp.projectred.integration2;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import mrtjp.projectred.transmission.IConnectable;

public abstract class GateLogic<PartType extends GatePart>
{
    public abstract boolean canConnectTo(PartType gate, IConnectable part, int r);
    
    public abstract int cycleShape(int shape);
    
    public abstract void onChange(PartType gate);
    
    public abstract void scheduledTick(PartType gate);
    
    public void onTick(PartType gate) {
    }
    
    public void setup(PartType part) {
    }
    
    /**
     * Beware you don't actually do anything client side.
     * @return true if the event is consumed by this part
     */
    public boolean activate(PartType part, EntityPlayer player, ItemStack held) {
        return false;
    }
}
