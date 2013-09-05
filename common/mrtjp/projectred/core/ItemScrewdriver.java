package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemScrewdriver extends Item {
    public ItemScrewdriver(int id) {
        super(id);
        setUnlocalizedName("projectred:screwdriver");
        setMaxStackSize(1);
        setMaxDamage(64);
        setNoRepair();
        setCreativeTab(ProjectRedCore.tabCore);
    }

    @Override
    public boolean onItemUse(ItemStack stack, EntityPlayer player, World w, int x, int y, int z, int side, float par8, float par9, float par10) {
        return true;
    }

    @Override
    public boolean shouldPassSneakingClickToBlock(World w, int x, int y, int z) {
        return true;
    }

    @SideOnly(Side.CLIENT)
    public void registerIcons(IconRegister reg) {
        this.itemIcon = reg.registerIcon("projectred:screwdriver");
    }

}
