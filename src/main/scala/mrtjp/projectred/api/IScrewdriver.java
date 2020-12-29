package mrtjp.projectred.api;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraft.entity.player.EntityPlayer;

public interface IScrewdriver
{
    boolean canUse(EntityPlayer player, ItemStack stack);

    void damageScrewdriver(EntityPlayer player, ItemStack stack); // Damage the item on usage
}