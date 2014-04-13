package mrtjp.projectred

import codechicken.lib.packet.PacketCustom
import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.network.NetworkMod
import mrtjp.projectred.illumination._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Illumination", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true, tinyPacketHandler = classOf[PacketCustom.CustomTinyPacketHandler])
object ProjectRedIllumination
{
    /** Blocks **/
    var blockLamp:BlockLamp = null
    var blockAirousLight:BlockAirousLight = null

    /** Multipart items **/
    var itemPartLantern:ItemPartLantern = null
    var itemPartInvLantern:ItemPartLantern = null
    var itemPartIllumarButton:ItemPartIllumarButton = null
    var itemPartCageLamp:ItemPartCageLamp = null
    var itemPartInvCageLamp:ItemPartCageLamp = null
    var itemPartFixture:ItemPartFixture = null
    var itemPartInvFixture:ItemPartFixture = null

    var tabLighting = new CreativeTabs("ill")
    {
        override def getIconItemStack = new ItemStack(ProjectRedIllumination.itemPartInvLantern, 1, 14)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        IlluminationProxy.versionCheck()
        IlluminationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        IlluminationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        IlluminationProxy.postinit()
    }
}