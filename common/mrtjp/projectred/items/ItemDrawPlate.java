package mrtjp.projectred.items;

import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;

public class ItemDrawPlate extends ItemCraftingDamage {

	public ItemDrawPlate(int par1) {
		super(par1);
		setUnlocalizedName("projectred.items.drawplate");
		setMaxDamage(512);
		setCreativeTab(ProjectRedTabs.tabCore);
	}

	@Override
	public void registerIcons(IconRegister par1IconRegister) {
		this.itemIcon = par1IconRegister.registerIcon("projectred:drawplate");
	}
}
