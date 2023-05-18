package mrtjp.projectred.core.item;

import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IWorldReader;

import static mrtjp.projectred.core.ProjectRedCore.CORE_GROUP;

public class ScrewdriverItem extends Item implements IScrewdriver {

    public ScrewdriverItem() {
        super(new Item.Properties()
                .stacksTo(1)
                .durability(128)
                .setNoRepair()
                .tab(CORE_GROUP));
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, IWorldReader world, BlockPos pos, PlayerEntity player) {
        return true;
    }

    @Override
    public boolean canUse(PlayerEntity player, ItemStack stack) {
        return true;
    }

    @Override
    public void damageScrewdriver(PlayerEntity player, ItemStack stack) {
        if (!Configurator.unbreakableScrewdriver) {
            stack.hurtAndBreak(1, player, p -> {});
        }
    }
}
