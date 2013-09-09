package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ShapelessOreNBTRecipe;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public class BackpackRecolouringRecipe extends ShapelessOreNBTRecipe {

    public BackpackRecolouringRecipe() {
        super(new ItemStack(ProjectRedExploration.itemBackpack, 1, Short.MAX_VALUE), new ItemStack(ProjectRedExploration.itemBackpack, 1, Short.MAX_VALUE), new ItemStack(Item.dyePowder, 1, Short.MAX_VALUE));
        this.setKeepNBT();
    }

    @Override
    public ItemStack getCraftingResult(InventoryCrafting inv) {
        ItemStack out = super.getCraftingResult(inv);
        ItemStack dye = null;
        for (int i = 0; i < 9; i++) {
            ItemStack slot = inv.getStackInSlot(i);
            if (slot != null) {
                if (slot.getItem() == Item.dyePowder) {
                    dye = slot;
                    break;
                }
            }
        }
        ItemStack trueout = new ItemStack(ProjectRedExploration.itemBackpack.itemID, 1, 15 - dye.getItemDamage());
        if (trueout.getItemDamage() == out.getItemDamage()) {
            return null;
        }
        trueout.setTagCompound(out.getTagCompound());
        return trueout;
    }
}
