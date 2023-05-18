package mrtjp.projectred.exploration.item;

import net.minecraft.item.Item;

import static mrtjp.projectred.exploration.ProjectRedExploration.EXPLORATION_GROUP;

public class WoolGinItem extends DamageableCraftingContainerItem {

    public WoolGinItem() {
        super(new Item.Properties()
                .durability(128)
                .tab(EXPLORATION_GROUP));
    }
}
