package mrtjp.projectred.core.item;

import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.LevelReader;

public class ScrewdriverItem extends Item implements IScrewdriver {

    public ScrewdriverItem() {
        super(new Item.Properties()
                .stacksTo(1)
                .durability(128)
                .setNoRepair());
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, LevelReader world, BlockPos pos, Player player) {
        return true;
    }

    @Override
    public boolean canUse(Player player, InteractionHand hand) {
        return true;
    }

    @Override
    public void damageScrewdriver(Player player, InteractionHand hand) {
        if (!Configurator.unbreakableScrewdriver) {
            player.getItemInHand(hand).hurtAndBreak(1, player, LivingEntity.getSlotForHand(hand));
        }
    }
}
