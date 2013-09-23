package mrtjp.projectred.exploration;

import java.util.List;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import net.minecraft.creativetab.CreativeTabs;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;

public class ItemBlockSpecialStoneWalls extends ItemBlock{

    public ItemBlockSpecialStoneWalls(int par1) {
        super(par1);
        setHasSubtypes(true);
        setMaxDamage(0);
    }

    public void getSubItems(int id, CreativeTabs tab, List list) {
        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE)
            list.add(new ItemStack(ProjectRedExploration.blockStoneWalls, 1, s.meta));
    }

    @Override
    public int getMetadata(int i) {
        return i;
    }

    public String getUnlocalizedName(ItemStack itemstack) {
        return ProjectRedExploration.blockStones.getUnlocalizedName() + "." + EnumSpecialStone.VALID_STONE[itemstack.getItemDamage()].unlocal+"wall";
    }

}
