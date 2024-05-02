package mrtjp.projectred.expansion.item;

import mrtjp.projectred.expansion.init.ExpansionItems;
import net.minecraft.world.item.Item;

public class EmptyBatteryItem extends Item implements IRechargableBattery {

    public EmptyBatteryItem() {
        super(new Item.Properties());
    }

    @Override
    public Item getChargedVariant() {
        return ExpansionItems.BATTERY_ITEM.get();
    }

    @Override
    public Item getEmptyVariant() {
        return this;
    }
}
