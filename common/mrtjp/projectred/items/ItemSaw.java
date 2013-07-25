package mrtjp.projectred.items;

import mrtjp.projectred.crafting.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemSaw extends ItemCraftingDamage {

	public ItemSaw(int i) {
		super(i);
		setUnlocalizedName("projectred.items.saw");
		setMaxDamage(1152);
		setCreativeTab(ProjectRedTabs.tabTools);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void registerIcons(IconRegister par1IconRegister) {
		this.itemIcon = par1IconRegister.registerIcon("projectred:saw");
	}

}
