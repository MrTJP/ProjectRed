package mrtjp.projectred.core;

import net.minecraft.block.Block;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;

public class ShapedOreNBTRecipe extends ShapedOreRecipe {

    protected boolean checkNBT;
    protected boolean keepNBT;
    protected ItemStack output;

    public ShapedOreNBTRecipe(Block result, Object... recipe) {
        this(new ItemStack(result), recipe);
    }

    public ShapedOreNBTRecipe(Item result, Object... recipe) {
        this(new ItemStack(result), recipe);
    }

    public ShapedOreNBTRecipe(ItemStack result, Object... recipe) {
        super(result, recipe);
        this.output = result.copy();
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv) {
        ItemStack out = output.copy();
        if (keepNBT) {
            ItemStack oldItemWithNBT = null;
            for (int i = 0; i < 9; i++) {
                ItemStack slot = inv.getStackInSlot(i);
                if (slot != null) {
                    if (slot.hasTagCompound()) {
                        oldItemWithNBT = slot;
                        break;
                    }
                }
            }
            if (oldItemWithNBT != null) {
                out.setTagCompound(oldItemWithNBT.getTagCompound());
            }
        }
        return out;
    }

    public ShapedOreNBTRecipe setKeepNBT() {
        keepNBT = true;
        return this;
    }

    public ShapedOreNBTRecipe setCheckNBT() {
        checkNBT = true;
        return this;
    }

    private boolean checkItemEquals(ItemStack target, ItemStack input) {
        if (input == null && target != null || input != null && target == null) {
            return false;
        }
        if (target.itemID == input.itemID && (target.getItemDamage() == OreDictionary.WILDCARD_VALUE || target.getItemDamage() == input.getItemDamage())) {
            if (checkNBT) {
                return target.getTagCompound().equals(input.getTagCompound());
            }
            return true;
        }
        return false;
    }

}
