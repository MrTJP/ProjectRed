package mrtjp.projectred

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.{FMLPreInitializationEvent, FMLInitializationEvent, FMLPostInitializationEvent}
import cpw.mods.fml.common.network.NetworkMod
import mrtjp.projectred.expansion.{ExpansionProxy, BlockMachine}
import net.minecraft.block.Block
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Expansion", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = classOf[PacketCustom.CustomTinyPacketHandler])
object ProjectRedExpansion
{
    /** Blocks **/
    var machine1:BlockMachine = null

    val tabExpansion = new CreativeTabs("expansion")
    {
        override def getIconItemStack = new ItemStack(Block.hopperBlock)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        ExpansionProxy.versionCheck()
        ExpansionProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        ExpansionProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        ExpansionProxy.postinit()
    }
}
