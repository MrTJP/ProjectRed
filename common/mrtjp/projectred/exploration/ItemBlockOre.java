package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockOre extends ItemBlock {

    public ItemBlockOre(int par1) {
        super(par1);
        setHasSubtypes(true);
        setMaxDamage(0);
    }

    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumOre e : EnumOre.VALID_ORES) {
            list.add(e.getItemStack(1));
        }
    }

    @Override
    public int getMetadata(int i) {
        return i;
    }

    public String getUnlocalizedName(ItemStack itemstack) {
        return ProjectRedExploration.blockOres.getUnlocalizedName() + "." + EnumOre.VALID_ORES[itemstack.getItemDamage()].unlocal;
    }

}
