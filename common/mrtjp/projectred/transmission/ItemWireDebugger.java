package mrtjp.projectred.transmission;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;
import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;

public class ItemWireDebugger extends Item {

    public ItemWireDebugger(int par1) {
        super(par1);
        setMaxStackSize(1);
        setMaxDamage(64);
        setNoRepair();
        setUnlocalizedName("projectred.transmission.wiredebug");
        setCreativeTab(ProjectRedTabs.tabTransmission);
    }

    @Override
    public boolean shouldPassSneakingClickToBlock(World par2World, int par4, int par5, int par6) {
        return true;
    }

    @Override
    public boolean onItemUseFirst(ItemStack par1ItemStack, EntityPlayer player, World par3World, int par4, int par5, int par6, int par7, float par8, float par9, float par10) {
        return false;
    }
    
    @Override
    public boolean onItemUse(ItemStack par1ItemStack, EntityPlayer player, World par3World, int par4, int par5, int par6, int par7, float par8, float par9, float par10) {
        return false;
    }
    
    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg) {
        this.itemIcon = reg.registerIcon("projectred:debugger");
    }
}
