package mrtjp.projectred.transmission

import codechicken.multipart.api.ItemMultiPart
import mrtjp.projectred.core.PRLib
import net.minecraft.item.{Item, ItemUseContext}

class ItemPartWire(val wireType: WireType) extends ItemMultiPart(new Item.Properties().group(TransmissionContent.transmissionItemGroup)) {

    override def newPart(context: ItemUseContext) = {
        val side = context.getFace
        val onPos = context.getPos.offset(side.getOpposite)
        if (!PRLib.canPlaceWireOnSide(context.getWorld, onPos, side)) {
            null
        } else {
            val w = wireType.newPart()
            w.preparePlacement(side)
            w
        }
    }
}

class ItemPartFramedWire(wireType: WireType) extends ItemPartWire(wireType) {

    override def newPart(context: ItemUseContext) = {
        val side = context.getFace
        val w = wireType.newPart()
        w.preparePlacement(side)
        w
    }
}
