package mrtjp.projectred.integration.item;

import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.integration.ProjectRedIntegration;
import net.minecraft.world.item.Item;

public class GatePartItem extends BaseGatePartItem {

    public GatePartItem(GateType gateType) {
        super(new Item.Properties().tab(ProjectRedIntegration.CREATIVE_TAB), gateType);
    }

}
