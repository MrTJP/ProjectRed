package mrtjp.projectred.exploration.item;

import codechicken.microblock.Saw;
import net.minecraft.item.IItemTier;
import net.minecraft.item.ItemStack;

public class SawItem extends DamageableCraftingContainerItem implements Saw {

    private final IItemTier tier;

    public SawItem(IItemTier tier, Properties properties) {
        super(properties);
        this.tier = tier;
    }

    @Override
    public int getMaxCuttingStrength() {
        return Saw.super.getMaxCuttingStrength();
    }

    @Override
    public int getCuttingStrength(ItemStack item) {
        return tier.getLevel();
    }
}
