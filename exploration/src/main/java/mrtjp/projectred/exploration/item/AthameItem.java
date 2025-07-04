package mrtjp.projectred.exploration.item;

import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.SwordItem;
import net.minecraft.world.item.Tier;

public class AthameItem extends SwordItem {

    public AthameItem(Tier itemTier, int attackDamage, float attackSpeed) {
        super(itemTier, new Item.Properties().attributes(SwordItem.createAttributes(itemTier, attackDamage, attackSpeed)));
    }

    @Override
    public boolean hurtEnemy(ItemStack stack, LivingEntity target, LivingEntity player) {
        return super.hurtEnemy(stack, target, player);
    }
}
