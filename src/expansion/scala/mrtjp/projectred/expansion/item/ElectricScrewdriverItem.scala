package mrtjp.projectred.expansion.item

import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{Item, ItemStack, ItemUseContext}
import net.minecraft.util.ActionResultType
import net.minecraft.util.math.BlockPos
import net.minecraft.world.IWorldReader

class ElectricScrewdriverItem extends Item(
    new Item.Properties()
            .tab(ExpansionContent.expansionItemGroup)
            .stacksTo(1)
            .durability(400)
            .setNoRepair()) with IScrewdriver with IChargable
{
    override def useOn(context:ItemUseContext):ActionResultType = ActionResultType.PASS

    override def doesSneakBypassUse(stack:ItemStack, world:IWorldReader, pos:BlockPos, player:PlayerEntity):Boolean = true

    override def canUse(player:PlayerEntity, stack:ItemStack):Boolean = stack.getDamageValue < stack.getMaxDamage

    override def damageScrewdriver(player:PlayerEntity, stack:ItemStack):Unit = {
        stack.hurtAndBreak(1, player, (_:PlayerEntity) => ())
    }
}
