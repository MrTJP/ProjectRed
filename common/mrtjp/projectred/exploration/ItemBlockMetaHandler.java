package mrtjp.projectred.exploration;

import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockMetaHandler extends ItemBlock {

    public ItemBlockMetaHandler(int par1) {
        super(par1);
        setHasSubtypes(true);
        setMaxDamage(0);
    }

    @Override
    public int getMetadata(int i) {
        return i;
    }

    @Override
    public String getUnlocalizedName(ItemStack stack) {
        return super.getUnlocalizedName(stack) + "|" + stack.getItemDamage();
    }
}