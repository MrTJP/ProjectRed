package mrtjp.projectred.expansion.item

import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.item.Item

class BatteryItem extends Item(new Item.Properties()
        .tab(ExpansionContent.expansionItemGroup)
        .stacksTo(1)
        .durability(1600)
        .setNoRepair()) with TChargableBatteryItem
{
    override def isEmpty = false

    override def getEmptyVariant:Item = ExpansionContent.emptyBatteryItem.get()
    override def getChargedVariant:Item = this
}
