package mrtjp.projectred.exploration.item;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public class DamageableCraftingContainerItem extends Item {

    public DamageableCraftingContainerItem(Properties properties) {
        super(properties);
    }

    @Override
    public boolean hasContainerItem(ItemStack stack) {
        return true;
    }

    @Override
    public ItemStack getContainerItem(ItemStack stack) {
        if (canBeDepleted()) {
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
