package mrtjp.projectred.illumination

import codechicken.multipart.api.{ItemMultiPart, MultiPartType}
import codechicken.multipart.api.part.TMultiPart
import net.minecraft.item.{BlockItemUseContext, Item, ItemUseContext}

import java.util.function.Supplier

class ItemBaseLight(val definition:LightPartDefinition, val colour:Int, val inverted:Boolean) extends ItemMultiPart(new Item.Properties().group(IlluminationContent.illuminationItemGroup))
{
    override def newPart(context:ItemUseContext):TMultiPart = {
        val side = context.getFace
        val onPos = context.getPos.offset(side.getOpposite)

        if (definition.canFloat || BaseLightPart.canPlaceLight(context.getWorld, onPos, side)) {
            val part = definition.multiPartType(colour, inverted).createPartServer(null).asInstanceOf[BaseLightPart]
            part.preparePlacement(side.getOpposite.getIndex)
            part
        } else
            null
    }
}

class ItemPartButton(partType:Supplier[MultiPartType[_]]) extends ItemMultiPart(new Item.Properties().group(IlluminationContent.illuminationItemGroup))
{
    override def newPart(context:ItemUseContext):TMultiPart = {
        val side = context.getFace
        val onPos = context.getPos.offset(side.getOpposite)

        if (context.getWorld.getBlockState(onPos).isSolidSide(context.getWorld, onPos, side)) {
            val button = partType.get().createPartServer(null).asInstanceOf[LightButtonPart]
            button.setStateOnPlacement(new BlockItemUseContext(context))
            button
        } else
            null
    }
}