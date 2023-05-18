package mrtjp.projectred.api;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

public interface IScrewdriver
{
    boolean canUse(Player player, ItemStack stack);

    //TODO, Improve context here
    void damageScrewdriver(Player player, ItemStack stack); // Damage the item on usage
}
