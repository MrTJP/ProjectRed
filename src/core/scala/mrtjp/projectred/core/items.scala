package mrtjp.projectred.core

import mrtjp.projectred.api.IScrewdriver
import mrtjp.projectred.core.CoreContent.itemGroupCore
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.IWorldReader

abstract class ItemCraftingDamage(properties: Item.Properties) extends Item(properties.setNoRepair())
{
    override def hasContainerItem(itemStack:ItemStack) = true

    override def getContainerItem(stack:ItemStack) =
        if (canBeDepleted) {
            val ret = stack.copy()
            ret.setDamageValue(ret.getDamageValue + 1);
            ret
        } else {
            stack
        }
}

class ItemDrawPlate extends ItemCraftingDamage(new Item.Properties().durability(512).tab(itemGroupCore))

class ItemScrewdriver extends Item(new Item.Properties().stacksTo(1).durability(128).setNoRepair().tab(itemGroupCore)) with IScrewdriver
{

    override def doesSneakBypassUse(stack: ItemStack, world: IWorldReader, pos: BlockPos, player: PlayerEntity) = true

    override def canUse(player: PlayerEntity, stack:ItemStack) = true

    override def damageScrewdriver(player: PlayerEntity, stack:ItemStack)
    {
        if (!Configurator.unbreakableScrewdriver)
            stack.hurtAndBreak(1, player, (p:PlayerEntity) => {})
    }
}

class ItemMultimeter extends Item(new Item.Properties().stacksTo(1).durability(256).setNoRepair().tab(itemGroupCore))
{
    override def doesSneakBypassUse(stack: ItemStack, world: IWorldReader, pos: BlockPos, player: PlayerEntity) = true
}
