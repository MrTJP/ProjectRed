package mrtjp.projectred.expansion.item;

import mrtjp.projectred.expansion.ProjectRedExpansion;
import mrtjp.projectred.expansion.init.ExpansionItems;
import net.minecraft.world.item.Item;

public class BatteryItem extends Item implements IRechargableBattery {

    public BatteryItem() {
        super(new Item.Properties()
                .tab(ProjectRedExpansion.EXPANSION_GROUP)
                .durability(1600)
                .setNoRepair());
    }

    @Override
    public Item getChargedVariant() {
        return this;
    }

    @Override
    public Item getEmptyVariant() {
        return ExpansionItems.EMPTY_BATTERY_ITEM.get();
    }
}
