package mrtjp.projectred.transmission

import codechicken.multipart.api.ItemMultiPart
import mrtjp.projectred.core.PRLib
import net.minecraft.item.{Item, ItemUseContext}

class ItemPartWire(val wireType: WireType) extends ItemMultiPart(new Item.Properties().tab(TransmissionContent.transmissionItemGroup)) {

    override def newPart(context: ItemUseContext) = {
        val side = context.getClickedFace
        val onPos = context.getClickedPos.relative(side.getOpposite)
        if (!PRLib.canPlaceWireOnSide(context.getLevel, onPos, side)) {
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
        val side = context.getClickedFace
        val w = wireType.newPart()
        w.preparePlacement(side)
        w
    }
}
