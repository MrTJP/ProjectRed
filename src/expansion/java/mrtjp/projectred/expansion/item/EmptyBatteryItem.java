package mrtjp.projectred.expansion.item;

import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.expansion.init.ExpansionReferences;
import net.minecraft.item.Item;

public class EmptyBatteryItem extends Item implements IRechargableBattery {

    public EmptyBatteryItem() {
        super(new Item.Properties().tab(ProjectRedExpansion.EXPANSION_GROUP));
    }

    @Override
    public Item getChargedVariant() {
        return ExpansionReferences.BATTERY_ITEM;
    }

    @Override
    public Item getEmptyVariant() {
        return this;
    }
}
