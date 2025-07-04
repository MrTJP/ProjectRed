package mrtjp.projectred.api;

import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

public interface IScrewdriver
{
    boolean canUse(Player player, InteractionHand hand);

    void damageScrewdriver(Player player, InteractionHand hand);
}
