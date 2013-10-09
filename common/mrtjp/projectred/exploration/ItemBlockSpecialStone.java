package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockSpecialStone extends ItemBlockMetaHandler {

    public ItemBlockSpecialStone(int par1) {
        super(par1);
    }

    @Override
    public String getUnlocalizedName(ItemStack itemstack) {
        return ProjectRedExploration.blockStones.getUnlocalizedName() + "." + EnumSpecialStone.VALID_STONE[itemstack.getItemDamage()].unlocal;
    }

}
