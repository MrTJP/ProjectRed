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
        if (isDamageable) {
            val ret = stack.copy()
            ret.setDamage(ret.getDamage + 1);
            ret
        } else {
            stack
        }
}

class ItemDrawPlate extends ItemCraftingDamage(new Item.Properties().maxDamage(512).group(itemGroupCore))

class ItemScrewdriver extends Item(new Item.Properties().maxStackSize(1).maxDamage(128).setNoRepair().group(itemGroupCore)) with IScrewdriver
{

    override def doesSneakBypassUse(stack: ItemStack, world: IWorldReader, pos: BlockPos, player: PlayerEntity) = true

    override def canUse(player: PlayerEntity, stack:ItemStack) = true

    override def damageScrewdriver(player: PlayerEntity, stack:ItemStack)
    {
        if (!Configurator.unbreakableScrewdriver)
            stack.damageItem(1, player, (p:PlayerEntity) => {})
    }
}

class ItemMultimeter extends Item(new Item.Properties().maxStackSize(1).maxDamage(256).setNoRepair().group(itemGroupCore))
{
    override def doesSneakBypassUse(stack: ItemStack, world: IWorldReader, pos: BlockPos, player: PlayerEntity) = true
}
