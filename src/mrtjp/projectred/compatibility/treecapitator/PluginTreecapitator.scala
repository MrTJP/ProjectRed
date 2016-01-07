package mrtjp.projectred.compatibility.treecapitator

import cpw.mods.fml.common.event.FMLInterModComms
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import net.minecraft.item.Item
import net.minecraft.nbt.NBTTagCompound

object AfterWorldCheck
{
    def checkedInit()
    {
        val tpModCfg = new NBTTagCompound
        tpModCfg.setString("modID", "ProjRed|Exploration")
        tpModCfg.setString("axeIDList", "%d; %d; %d".format(Item.getIdFromItem(ProjectRedExploration.itemPeridotAxe),
                Item.getIdFromItem(ProjectRedExploration.itemRubyAxe), Item.getIdFromItem(ProjectRedExploration.itemSapphireAxe)))
        tpModCfg.setString("shearsIDList", "")
        tpModCfg.setBoolean("useShiftedItemID", false)

        FMLInterModComms.sendMessage("TreeCapitator", "ThirdPartyModConfig", tpModCfg)
    }
}

object PluginTreecapitator extends IPRPlugin
{
    override def getModIDs = Array("TreeCapitator", "ProjRed|Exploration")

    override def isEnabled = Configurator.compat_Treecapitator

    override def preInit(){}

    override def init()
    {
        AfterWorldCheck.checkedInit()
    }

    override def postInit(){}

    override def desc() = "Treecapitator: gem axe compat"
}
