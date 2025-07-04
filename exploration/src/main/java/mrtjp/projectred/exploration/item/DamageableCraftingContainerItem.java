package mrtjp.projectred.exploration.item;

import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;

public class DamageableCraftingContainerItem extends Item {

    public DamageableCraftingContainerItem(Properties properties) {
        super(properties);
    }

    @Override
    public boolean hasCraftingRemainingItem(ItemStack stack) {
        return true;
    }

    @Override
    public ItemStack getCraftingRemainingItem(ItemStack stack) {
        if (stack.isDamageableItem()) {
            if (stack.getDamageValue() + 1 >= stack.getMaxDamage()) {
                return ItemStack.EMPTY;
            }
            ItemStack newStack = stack.copy();
            newStack.setDamageValue(stack.getDamageValue() + 1);
            return newStack;
        }
        return stack;
    }
}
