package mrtjp.projectred.integration.item;

import mrtjp.projectred.integration.GateType;
import net.minecraft.world.item.Item;

public class GatePartItem extends BaseGatePartItem {

    public GatePartItem(GateType gateType) {
        super(new Item.Properties(), gateType);
    }

    public GateType getGateType() {
        return gateType;
    }

}
