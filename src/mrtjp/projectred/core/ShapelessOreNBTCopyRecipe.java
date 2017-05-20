package mrtjp.projectred.core;

import net.minecraft.block.Block;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapelessOreRecipe;

public class ShapelessOreNBTCopyRecipe extends ShapelessOreRecipe
{
    private ItemStack output;

    public ShapelessOreNBTCopyRecipe(Block result, Object... recipe)
    {
        this(new ItemStack(result), recipe);
    }

    public ShapelessOreNBTCopyRecipe(Item result, Object... recipe)
    {
        this(new ItemStack(result), recipe);
    }

    public ShapelessOreNBTCopyRecipe(ItemStack result, Object... recipe)
    {
        super(result, recipe);
        this.output = result.copy();
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv)
    {
        ItemStack out = output.copy();

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

        return out;
    }
}
