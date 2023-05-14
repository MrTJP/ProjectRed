package mrtjp.projectred.exploration.item;

import mrtjp.projectred.ProjectRedExploration;
import net.minecraft.entity.LivingEntity;
import net.minecraft.item.IItemTier;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.SwordItem;

public class AthameItem extends SwordItem {

    public AthameItem(IItemTier itemTier, int attackDamage, float attackSpeed) {
        super(itemTier, attackDamage, attackSpeed, new Item.Properties().tab(ProjectRedExploration.EXPLORATION_GROUP));
    }

    @Override
    public boolean hurtEnemy(ItemStack stack, LivingEntity target, LivingEntity player) {
        return super.hurtEnemy(stack, target, player);
    }
}
