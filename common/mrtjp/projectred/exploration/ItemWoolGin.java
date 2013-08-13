package mrtjp.projectred.exploration;

import mrtjp.projectred.core.ItemCraftingDamage;
import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;

public class ItemWoolGin extends ItemCraftingDamage {

    public ItemWoolGin(int par1) {
        super(par1);
        setUnlocalizedName("projectred.items.woolgin");
        setMaxDamage(128);
        this.setCreativeTab(ProjectRedTabs.tabExploration);
    }

    @Override
    public void registerIcons(IconRegister par1IconRegister) {
        this.itemIcon = par1IconRegister.registerIcon("projectred:woolgin");
    }
}
