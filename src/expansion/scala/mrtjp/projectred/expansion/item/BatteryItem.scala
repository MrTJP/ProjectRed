package mrtjp.projectred.expansion.item

import mrtjp.projectred.expansion.ExpansionContent
import net.minecraft.item.Item

class BatteryItem extends Item(new Item.Properties()
        .group(ExpansionContent.expansionItemGroup)
        .maxStackSize(1)
        .maxDamage(1600)
        .setNoRepair()) with TChargableBatteryItem
{
    override def isEmpty = false

    override def getEmptyVariant:Item = ExpansionContent.emptyBatteryItem.get()
    override def getChargedVariant:Item = this
}
