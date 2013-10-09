package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import net.minecraft.item.ItemStack;

public class ItemBlockOre extends ItemBlockMetaHandler {

    public ItemBlockOre(int par1) {
        super(par1);
    }

    @Override
    public String getUnlocalizedName(ItemStack itemstack) {
        return ProjectRedExploration.blockOres.getUnlocalizedName() + "." + EnumOre.VALID_ORES[itemstack.getItemDamage()].unlocal;
    }

}
