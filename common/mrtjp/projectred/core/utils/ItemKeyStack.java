package mrtjp.projectred.core.utils;

import net.minecraft.item.ItemStack;
import codechicken.lib.data.MCDataOutput;

public class ItemKeyStack implements Comparable<ItemKeyStack>{

    private final ItemKey key;
    public int stackSize;

    private ItemKeyStack(ItemKey key, int size) {
        this.key = key;
        this.stackSize = size;
    }

    public boolean equals(Object o) {
        if (o instanceof ItemKeyStack) {
            ItemKeyStack stack = (ItemKeyStack) o;
            return stack.key.equals(key) && stack.stackSize == stackSize;
        }
        return false;
    }

    public int hashCode() {
        return key.hashCode();
    }

    public ItemStack makeStack() {
        return key.makeStack(stackSize);
    }

    public static ItemKeyStack get(ItemKey key, int size) {
        return new ItemKeyStack(key, size);
    }

    public static ItemKeyStack get(ItemStack stack) {
        if (stack == null)
            return null;
        return new ItemKeyStack(ItemKey.get(stack), stack.stackSize);
    }

    public ItemKey key() {
        return key;
    }

    public ItemKeyStack copy() {
        return new ItemKeyStack(key.copy(), stackSize);
    }

    /** Stack Interactions **/
    public int getStackLimit() {
        return key.getStackLimit();
    }
    
    public String getName() {
        return key.getName();
    }

    @Override
    public int compareTo(ItemKeyStack o) {
        int c = key.compareTo(o.key);
        if (c == 0)
            return stackSize - o.stackSize;
        return c;
    }
}
