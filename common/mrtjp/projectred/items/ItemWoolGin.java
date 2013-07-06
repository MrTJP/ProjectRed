package mrtjp.projectred.items;

import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.creativetab.CreativeTabs;

public class ItemWoolGin extends ItemCraftingDamage {

	public ItemWoolGin(int par1) {
		super(par1);
		setUnlocalizedName("projectred.items.woolgin");
		setMaxDamage(128);
		setCreativeTab(ProjectRedTabs.tabTools);
	}

	@Override
	public void registerIcons(IconRegister par1IconRegister) {
		this.itemIcon = par1IconRegister.registerIcon("projectred:woolgin");
	}
}
