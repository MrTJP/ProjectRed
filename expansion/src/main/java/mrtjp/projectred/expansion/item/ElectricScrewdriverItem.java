package mrtjp.projectred.expansion.item;

import mrtjp.projectred.api.IScrewdriver;
import net.minecraft.core.BlockPos;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.LevelReader;

public class ElectricScrewdriverItem extends Item implements IScrewdriver, IChargable {

    public ElectricScrewdriverItem() {
        super(new Item.Properties()
                .durability(400)
                .setNoRepair());
    }

    @Override
    public InteractionResult useOn(UseOnContext context) {
        return InteractionResult.PASS;
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, LevelReader world, BlockPos pos, Player player) {
        return true;
    }

    @Override
    public boolean canUse(Player player, InteractionHand hand) {
        var stack = player.getItemInHand(hand);
        return stack.getDamageValue() < stack.getMaxDamage();
    }

    @Override
    public void damageScrewdriver(Player player, InteractionHand hand) {
        player.getItemInHand(hand).hurtAndBreak(1, player, LivingEntity.getSlotForHand(hand));
    }

    @Override
    public Item getChargedVariant() {
        return this;
    }

    @Override
    public Item getEmptyVariant() {
        return this;
    }
}
