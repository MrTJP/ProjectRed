package mrtjp.projectred.core.item;

import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public class CraftingDamageItem extends Item {

    public CraftingDamageItem(Item.Properties properties) {
        super(properties.setNoRepair());
    }

    @Override
    public boolean hasCraftingRemainingItem(ItemStack stack) {
        return true;
    }

    @Override
    public ItemStack getCraftingRemainingItem(ItemStack itemStack) {
        if (isDamageable(itemStack)) {
            ItemStack ret = itemStack.copy();
            ret.setDamageValue(ret.getDamageValue() + 1);
            return ret;
        }
        return itemStack;
    }
}
