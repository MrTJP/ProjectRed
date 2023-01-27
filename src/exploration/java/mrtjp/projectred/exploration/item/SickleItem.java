package mrtjp.projectred.exploration.item;

import net.minecraft.block.BlockState;
import net.minecraft.entity.LivingEntity;
import net.minecraft.item.IItemTier;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ToolItem;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

import java.util.Collections;

public class SickleItem extends ToolItem {

    public SickleItem(IItemTier itemTier, float attackDamage, float attackSpeed, Properties properties) {
        super(attackDamage, attackSpeed, itemTier, Collections.emptySet(), properties);
    }

    //TODO

    @Override
    public boolean mineBlock(ItemStack p_179218_1_, World p_179218_2_, BlockState p_179218_3_, BlockPos p_179218_4_, LivingEntity p_179218_5_) {
        return super.mineBlock(p_179218_1_, p_179218_2_, p_179218_3_, p_179218_4_, p_179218_5_);
    }

    @Override
    public float getDestroySpeed(ItemStack p_150893_1_, BlockState p_150893_2_) {
        return super.getDestroySpeed(p_150893_1_, p_150893_2_);
    }


}
