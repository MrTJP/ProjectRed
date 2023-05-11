package mrtjp.projectred.api;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;

public interface IScrewdriver
{
    boolean canUse(PlayerEntity player, ItemStack stack);

    //TODO, Improve context here
    void damageScrewdriver(PlayerEntity player, ItemStack stack); // Damage the item on usage
}
