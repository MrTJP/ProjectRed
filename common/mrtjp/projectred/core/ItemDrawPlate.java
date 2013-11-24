package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.blockutil.ItemCraftingDamage;
import net.minecraft.client.renderer.texture.IconRegister;

public class ItemDrawPlate extends ItemCraftingDamage {

    public ItemDrawPlate(int par1) {
        super(par1);
        setUnlocalizedName("projectred.core.drawplate");
        setMaxDamage(512);
        setCreativeTab(ProjectRedCore.tabCore);
    }

    @Override
    public void registerIcons(IconRegister par1IconRegister) {
        this.itemIcon = par1IconRegister.registerIcon("projectred:drawplate");
    }
}
