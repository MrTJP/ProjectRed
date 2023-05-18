package mrtjp.projectred.exploration.item;

import net.minecraft.world.item.Item;

import static mrtjp.projectred.exploration.ProjectRedExploration.EXPLORATION_CREATIVE_TAB;

public class WoolGinItem extends DamageableCraftingContainerItem {

    public WoolGinItem() {
        super(new Item.Properties()
                .durability(128)
                .tab(EXPLORATION_CREATIVE_TAB));
    }
}
