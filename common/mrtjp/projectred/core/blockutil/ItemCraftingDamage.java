package mrtjp.projectred.core.blockutil;

import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public abstract class ItemCraftingDamage extends Item {

    public ItemCraftingDamage(int par1) {
        super(par1);
        setMaxStackSize(1);
        setNoRepair();
    }

    @Override
    public abstract void registerIcons(IconRegister par1IconRegister);

    @Override
    public boolean hasContainerItem() {
        return true;
    }

    @Override
    public ItemStack getContainerItemStack(ItemStack stack) {
        if (stack.itemID == this.itemID) {
            stack.setItemDamage(stack.getItemDamage() + 1);
            return stack;
        } else {
            ItemStack newStack = new ItemStack(this);
            newStack.setItemDamage(newStack.getMaxDamage());
            return newStack;
        }
    }

    @Override
    public boolean doesContainerItemLeaveCraftingGrid(ItemStack is) {
        return false;
    }
}
