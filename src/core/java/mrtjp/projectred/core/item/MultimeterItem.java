package mrtjp.projectred.core.item;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.IWorldReader;

import static mrtjp.projectred.ProjectRedCore.CORE_GROUP;

public class MultimeterItem extends Item {

    public MultimeterItem() {
        super(new Item.Properties().stacksTo(1).durability(256).setNoRepair().tab(CORE_GROUP));
    }

    @Override
    public boolean doesSneakBypassUse(ItemStack stack, IWorldReader world, BlockPos pos, PlayerEntity player) {
        return true;
    }
}
