package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import cpw.mods.fml.common.Loader
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.core.{Configurator, IProxy}

class ExpansionProxy_server extends IProxy
{
    var loadPowerTest = false

    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        if (Loader.isModLoaded("ProjRed|Transmission") && Configurator.version.contains("@"))
        {
            loadPowerTest = true
            import mrtjp.projectred.transmission._
            MultiPartRegistry.registerParts((name, _) => name match
            {
                case "pr_100v" => new PowerWire100v
                case "pr_f100v" => new FramedPowerWire100v
            })
        }

//        //Machine1 (processing)
//        ProjectRedExpansion.machine1 = new BlockMachine("projectred.expansion.machine1")
//        //Machine1 tiles
//        ProjectRedExpansion.machine1.addTile(classOf[TileFurnace], 0)

//        //Machine2 (devices)
//        ProjectRedExpansion.machine2 = new BlockMachine("projectred.expansion.machine2")
//        //Machine2 tiles

        ExpansionRecipes.initRecipes()
    }

    def postinit()
    {
        // In dev mode, this module may load before transmission, therefore this must go in postInit
        if (loadPowerTest)
        {
            import mrtjp.projectred.transmission._
            ItemPartWire.additionalWires :+= WireDef.POWER_100v.makeStack
            ItemPartFramedWire.additionalWires :+= WireDef.POWER_100v.makeFramedStack
        }
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
//        RenderLib.setRenderer(ProjectRedExpansion.machine1, 0, RenderFurnace)
    }
}

object ExpansionProxy extends ExpansionProxy_client