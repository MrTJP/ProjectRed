package mrtjp.projectred.exploration.item;

import net.minecraft.core.BlockPos;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.DiggerItem;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Tier;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;

public class SickleItem extends DiggerItem {

    public SickleItem(Tier itemTier, float attackDamage, float attackSpeed, Properties properties) {
        super(attackDamage, attackSpeed, itemTier, BlockTags.REPLACEABLE, properties); //TODO is this tag correct?
    }

    //TODO !!!!

    @Override
    public boolean mineBlock(ItemStack p_179218_1_, Level p_179218_2_, BlockState p_179218_3_, BlockPos p_179218_4_, LivingEntity p_179218_5_) {
        return super.mineBlock(p_179218_1_, p_179218_2_, p_179218_3_, p_179218_4_, p_179218_5_);
    }

    @Override
    public float getDestroySpeed(ItemStack p_150893_1_, BlockState p_150893_2_) {
        return super.getDestroySpeed(p_150893_1_, p_150893_2_);
    }


}
