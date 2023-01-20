package mrtjp.projectred.expansion.item;

import mrtjp.projectred.ProjectRedExpansion;
import mrtjp.projectred.api.IScrewdriver;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IWorldReader;

public class ElectricScrewdriverItem extends Item implements IScrewdriver, IChargable {

    public ElectricScrewdriverItem() {
        super(new Item.Properties()
                .tab(ProjectRedExpansion.EXPANSION_GROUP)
                .durability(400)
                .setNoRepair());
    }

    @Override
    public ActionResultType useOn(ItemUseContext context) {
        return ActionResultType.PASS;
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, IWorldReader world, BlockPos pos, PlayerEntity player) {
        return true;
    }

    @Override
    public boolean canUse(PlayerEntity player, ItemStack stack) {
        return stack.getDamageValue() < stack.getMaxDamage();
    }

    @Override
    public void damageScrewdriver(PlayerEntity player, ItemStack stack) {
        stack.hurtAndBreak(1, player, p -> {});
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
