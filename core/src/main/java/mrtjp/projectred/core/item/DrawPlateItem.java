package mrtjp.projectred.core.item;

import net.minecraft.item.Item;

import static mrtjp.projectred.core.ProjectRedCore.CORE_GROUP;

public class DrawPlateItem extends CraftingDamageItem {

    public DrawPlateItem() {
        super(new Item.Properties().durability(512).tab(CORE_GROUP));
    }
}
