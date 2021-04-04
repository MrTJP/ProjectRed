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
            .group(ExpansionContent.expansionItemGroup)
            .maxStackSize(1)
            .maxDamage(400)
            .setNoRepair()) with IScrewdriver with IChargable
{
    override def onItemUse(context:ItemUseContext):ActionResultType = ActionResultType.PASS

    override def doesSneakBypassUse(stack:ItemStack, world:IWorldReader, pos:BlockPos, player:PlayerEntity):Boolean = true

    override def canUse(player:PlayerEntity, stack:ItemStack):Boolean = stack.getDamage < stack.getMaxDamage

    override def damageScrewdriver(player:PlayerEntity, stack:ItemStack):Unit = {
        stack.damageItem(1, player, (_:PlayerEntity) => ())
    }
}
