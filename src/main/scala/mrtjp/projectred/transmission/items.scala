package mrtjp.projectred.transmission

import codechicken.lib.vec._
import codechicken.multipart.TItemMultiPart
import mrtjp.core.item.ItemCore
import mrtjp.projectred.core.PRLib
import net.minecraft.entity.player.PlayerEntity
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.util.Direction
import net.minecraft.world.World

class ItemPartWire(val wireType: WireType) extends Item(new Item.Properties().group(TransmissionContent.transmissionItemGroup)) with TItemMultiPart {

    def newPart(item:ItemStack, player:PlayerEntity, world:World, pos:BlockPos, side:Int, vhit:Vector3) = {
        val onPos = pos.offset(Direction.byIndex(side^1))
        if (!PRLib.canPlaceWireOnSide(world, onPos, side)) null
        else {
            val w = wireType.newPart()
            w.preparePlacement(side)
            w
        }
    }
}

class ItemPartFramedWire(wireType: WireType) extends ItemPartWire(wireType) {

    override def newPart(item:ItemStack, player:PlayerEntity, world:World, pos:BlockPos, side:Int, vhit:Vector3) = {
        val w = wireType.newPart()
        w.preparePlacement(side)
        w
    }
}
