package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.blockutil.ItemCraftingDamage;
import net.minecraft.client.renderer.texture.IconRegister;

public class ItemWoolGin extends ItemCraftingDamage {

    public ItemWoolGin(int par1) {
        super(par1);
        setUnlocalizedName("projectred.exploration.woolgin");
        setMaxDamage(128);
        this.setCreativeTab(ProjectRedExploration.tabExploration);
    }

    @Override
    public void registerIcons(IconRegister par1IconRegister) {
        this.itemIcon = par1IconRegister.registerIcon("projectred:woolgin");
    }
}
