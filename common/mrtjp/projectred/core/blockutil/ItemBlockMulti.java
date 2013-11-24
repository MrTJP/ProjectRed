package mrtjp.projectred.core.blockutil;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockMulti extends ItemBlock {

    public ItemBlockMulti(int par1) {
        super(par1);
        setHasSubtypes(true);
    }

    @Override
    public int getMetadata(int md) {
        return md;
    }

    @Override
    public String getUnlocalizedName(ItemStack itemstack) {
        return getUnlocalizedName()+"|"+itemstack.getItemDamage();
    }

}
