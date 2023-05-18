package mrtjp.projectred.core.item;

import net.minecraft.world.item.Item;

import static mrtjp.projectred.core.ProjectRedCore.CORE_CREATIVE_TAB;

public class DrawPlateItem extends CraftingDamageItem {

    public DrawPlateItem() {
        super(new Item.Properties().durability(512).tab(CORE_CREATIVE_TAB));
    }
}
