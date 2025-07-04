package mrtjp.projectred.expansion.item;

import net.minecraft.util.Tuple;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.Enchantment;

public interface IChargable {

    default Tuple<ItemStack, Integer> addPower(ItemStack stack, int power) {

        if (stack.getItem() != this) return new Tuple<>(stack, 0);
        if (getChargedVariant() != getEmptyVariant() && stack.getItem() == getChargedVariant() && stack.getDamageValue() == 0) return new Tuple<>(stack, 0);

        if (getChargedVariant() != getEmptyVariant() && stack.getItem() == getEmptyVariant()) {
            ItemStack chargedStack = new ItemStack(getChargedVariant(), 1);
            chargedStack.applyComponents(stack.getComponents());
            chargedStack.setDamageValue(chargedStack.getMaxDamage());
            stack = chargedStack;
        }

        int spaceLeft = stack.getDamageValue();
        int toAdd = Math.min(spaceLeft, power);
        stack.setDamageValue(stack.getDamageValue() - toAdd);
        return new Tuple<>(stack, toAdd);
    }

    default Tuple<ItemStack, Integer> drawPower(ItemStack stack, int power) {

        if (stack.getItem() != this) return new Tuple<>(stack, 0);
        if (getChargedVariant() != getEmptyVariant() && stack.getItem() == getEmptyVariant()) return new Tuple<>(stack, 0);

        int powerLeft = stack.getMaxDamage() - stack.getDamageValue();
        int toDraw = Math.min(powerLeft, power);
        stack.setDamageValue(stack.getDamageValue() + toDraw);

        if (getChargedVariant() != getEmptyVariant() && stack.getDamageValue() >= stack.getMaxDamage()) {
            ItemStack emptyStack = new ItemStack(getEmptyVariant(), 1);
            emptyStack.applyComponents(stack.getComponents());
            stack = emptyStack;
        }

        return new Tuple<>(stack, toDraw);
    }

    default int getStoredPower(ItemStack stack) {
        if (stack.getItem() != this) return 0;
        if (stack.getItem() == getEmptyVariant()) return 0;
        return stack.getMaxDamage() - stack.getDamageValue();
    }

    default boolean isFullyCharged(ItemStack stack) {
        return stack.getItem() == getChargedVariant() && stack.getDamageValue() == 0;
    }

    default boolean canApplyElectricEnchantment(ItemStack stack, Enchantment enchantment) {
        return false;
    }

    Item getChargedVariant();

    Item getEmptyVariant();
}
