package mrtjp.projectred.transportation;

import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import mrtjp.projectred.transportation.RoutedPayload.SendPriority;

public class RoutingChipset_ItemTerminator extends RoutingChipset_ItemResponder
{
    @Override
    protected SendPriority getSendPriority()
    {
        return SendPriority.TERMINATED;
    }

    @Override
    public EnumRoutingChip getChipType()
    {
        return EnumRoutingChip.ITEMTERMINATOR;
    }
}
