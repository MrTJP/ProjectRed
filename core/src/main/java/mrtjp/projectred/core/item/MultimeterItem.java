package mrtjp.projectred.core.item;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.LevelReader;

import static mrtjp.projectred.core.ProjectRedCore.CORE_CREATIVE_TAB;

public class MultimeterItem extends Item {

    public MultimeterItem() {
        super(new Item.Properties().stacksTo(1).durability(256).setNoRepair().tab(CORE_CREATIVE_TAB));
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, LevelReader world, BlockPos pos, Player player) {
        return true;
    }

}
