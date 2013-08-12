package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockSpecialStone extends ItemBlock{

    public ItemBlockSpecialStone(int par1) {
        super(par1);
        setHasSubtypes(true);
        setMaxDamage(0);
    }

    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE) {
            list.add(s.getItemStack());
        }
    }

    @Override
    public int getMetadata(int i) {
        return i;
    }

    public String getUnlocalizedName(ItemStack itemstack) {
        return ProjectRed.blockStones.getUnlocalizedName() + "." + EnumSpecialStone.VALID_STONE[itemstack.getItemDamage()].unlocal;
    }

}
