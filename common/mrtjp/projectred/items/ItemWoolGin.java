package mrtjp.projectred.items;

import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;

public class ItemWoolGin extends ItemCraftingDamage {

	public ItemWoolGin(int par1) {
		super(par1);
		setUnlocalizedName("projectred.items.woolgin");
		setMaxDamage(128);
		setCreativeTab(ProjectRedTabs.tabCore);
	}

	@Override
	public void registerIcons(IconRegister par1IconRegister) {
		this.itemIcon = par1IconRegister.registerIcon("projectred:woolgin");
	}
}
