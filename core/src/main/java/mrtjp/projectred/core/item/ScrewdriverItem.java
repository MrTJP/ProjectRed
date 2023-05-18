package mrtjp.projectred.core.item;

import mrtjp.projectred.api.IScrewdriver;
import mrtjp.projectred.core.Configurator;
import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.LevelReader;

import static mrtjp.projectred.core.ProjectRedCore.CORE_CREATIVE_TAB;

public class ScrewdriverItem extends Item implements IScrewdriver {

    public ScrewdriverItem() {
        super(new Item.Properties()
                .stacksTo(1)
                .durability(128)
                .setNoRepair()
                .tab(CORE_CREATIVE_TAB));
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, LevelReader world, BlockPos pos, Player player) {
        return true;
    }

    @Override
    public boolean canUse(Player player, ItemStack stack) {
        return true;
    }

    @Override
    public void damageScrewdriver(Player player, ItemStack stack) {
        if (!Configurator.unbreakableScrewdriver) {
            stack.hurtAndBreak(1, player, p -> {});
        }
    }
}
