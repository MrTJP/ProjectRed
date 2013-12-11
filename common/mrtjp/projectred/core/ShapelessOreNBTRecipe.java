package mrtjp.projectred.core;

import net.minecraft.block.Block;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapelessOreRecipe;

public class ShapelessOreNBTRecipe extends ShapelessOreRecipe
{
    public boolean keepNBT = false;
    public boolean checkNBT = false;

    public ItemStack output;

    public ShapelessOreNBTRecipe(Block result, Object... recipe)
    {
        this(new ItemStack(result), recipe);
    }

    public ShapelessOreNBTRecipe(Item result, Object... recipe)
    {
        this(new ItemStack(result), recipe);
    }

    public ShapelessOreNBTRecipe(ItemStack result, Object... recipe)
    {
        super(result, recipe);
        this.output = result.copy();
    }

    public ShapelessOreNBTRecipe setKeepNBT()
    {
        keepNBT = true;
        return this;
    }

    public ShapelessOreNBTRecipe setCheckNBT()
    {
        checkNBT = true;
        return this;
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv)
    {
        ItemStack out = output.copy();
        if (keepNBT)
        {
            ItemStack oldItemWithNBT = null;
            for (int i = 0; i < 9; i++)
            {
                ItemStack slot = inv.getStackInSlot(i);
                if (slot != null)
                    if (slot.hasTagCompound())
                    {
                        oldItemWithNBT = slot;
                        break;
                    }
            }
            if (oldItemWithNBT != null)
                out.setTagCompound(oldItemWithNBT.getTagCompound());
        }
        return out;
    }

    private boolean checkItemEquals(ItemStack target, ItemStack input)
    {
        if (target.itemID == input.itemID && (target.getItemDamage() == OreDictionary.WILDCARD_VALUE || target.getItemDamage() == input.getItemDamage()))
        {
            if (checkNBT)
                return target.getTagCompound().equals(input.getTagCompound());
            return true;
        }
        return false;
    }
}
