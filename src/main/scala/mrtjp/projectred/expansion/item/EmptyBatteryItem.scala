package mrtjp.projectred.expansion.item

import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.item.Item

class EmptyBatteryItem extends Item(new Item.Properties().group(ExpansionContent.expansionItemGroup)) with TChargableBatteryItem
{
    override def isEmpty = true

    override def getEmptyVariant:Item = this

    override def getChargedVariant:Item = ExpansionContent.batteryItem.get()
}
