package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.api.IScrewdriver;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemScrewdriver extends Item implements IScrewdriver
{
    public ItemScrewdriver(int id)
    {
        super(id);
        setUnlocalizedName("projectred.core.screwdriver");
        setMaxStackSize(1);
        setMaxDamage(128);
        setNoRepair();
        setCreativeTab(ProjectRedCore.tabCore);
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float par8, float par9, float par10)
    {
        return false;
    }

    @Override
    public boolean shouldPassSneakingClickToBlock(World w, int x, int y, int z)
    {
        return true;
    }

    @Override
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg)
    {
        this.itemIcon = reg.registerIcon("projectred:screwdriver");
    }
	
	@Override
	public void damageScrewdriver(World world, EntityPlayer player)
	{
		player.getHeldItem().damageItem(1, player);
	}
}
