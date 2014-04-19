package mrtjp.projectred.exploration;

import mrtjp.projectred.core.libmc.PRColors;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemBlockStainedSapling extends ItemBlockMetaHandler
{
    public ItemBlockStainedSapling(int par1)
    {
        super(par1);
    }

    @Override
    @SideOnly(Side.CLIENT)
    public int getColorFromItemStack(ItemStack is, int par2)
    {
        return PRColors.get(is.getItemDamage()).rgb;
    }
}
