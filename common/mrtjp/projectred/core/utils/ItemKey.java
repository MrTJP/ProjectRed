package mrtjp.projectred.core.utils;

import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataOutput;

public class ItemKey implements Comparable<ItemKey>{
    
    private final int itemID;
    private final int itemDamage;
    private final NBTTagCompound tag;
    private final int hashCode;

    private ItemKey(int itemID, int itemDamage, NBTTagCompound tag) {
        this.itemID = itemID;
        this.itemDamage = itemDamage;
        this.tag = tag;
        
        int hash = (itemID*1000001) * itemDamage;
        if (tag != null)
            hash += tag.hashCode();
        
        hashCode = hash;
    }
    
    public static ItemKey get(ItemStack stack) {
        if (stack == null)
            return null;
        return new ItemKey(stack.itemID, stack.getItemDamage(), stack.getTagCompound());
    }
    
    public ItemStack makeStack(int size) {
        ItemStack stack = new ItemStack(itemID, size, itemDamage);
        stack.setTagCompound(tag);
        return stack;
    }
    
    @Override
    public boolean equals(Object o) {
        if (o instanceof ItemKey) {
            ItemKey key = (ItemKey) o;
            boolean itemEqual = key.itemID == itemID && key.itemDamage == itemDamage;
            boolean tagEqual = itemEqual && (key.tag == null || tag == null ? key.tag == tag : key.tag.equals(tag));
            return itemEqual && tagEqual;
        }
        return false;
    }
    
    @Override
    public int hashCode() {
        return hashCode;
    }
    
    @Override
    public int compareTo(ItemKey o) {
        int c = itemID - itemID;
        if (c == 0)
            c = itemDamage - o.itemDamage;
        return c;
    }

    
    public ItemKey copy() {
        return new ItemKey(itemID, itemDamage, tag);
    }
    
    /** Interactions **/
    
    public Item getItem() {
        return Item.itemsList[itemID];
    }
    
    public int getStackLimit() {
        return getItem().getItemStackLimit();
    }
    
    public String getName() {
        return makeStack(0).getDisplayName();
    }
}
