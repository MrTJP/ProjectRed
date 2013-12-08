package mrtjp.projectred.transportation;

import java.util.List;

import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;
import net.minecraft.nbt.NBTTagCompound;

public class RoutingChipset_ItemTerminator extends RoutingChipset_ItemResponder
{
    @Override
    protected SendPriority getSendPriority() {
        return SendPriority.TERMINATED;
    }

    @Override
    public EnumRoutingChip getChipType() {
        return EnumRoutingChip.ITEMTERMINATOR;
    }
}
