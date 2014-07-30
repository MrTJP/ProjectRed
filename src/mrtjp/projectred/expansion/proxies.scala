package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.common.Loader
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.libmc.RenderLib
import mrtjp.projectred.core.{GuiIDs, GuiManager, IProxy}
import mrtjp.projectred.transmission._

class ExpansionProxy_server extends IProxy with IPartFactory
{
    var loadPowerTest = false

    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        if (Loader.isModLoaded("ProjRed|Transmission"))
        {
            loadPowerTest = true
            MultiPartRegistry.registerParts(this, Array("pr_100v", "pr_f100v"))
        }

//        //Machine1 (processing)
//        ProjectRedExpansion.machine1 = new BlockMachine("projectred.expansion.machine1")
//        //Machine1 tiles
//        ProjectRedExpansion.machine1.addTile(classOf[TileFurnace], 0)

        //Machine2 (devices)
        ProjectRedExpansion.machine2 = new BlockMachine("projectred.expansion.machine2")
        //Machine2 tiles
        ProjectRedExpansion.machine2.addTile(classOf[TileRouterController], 0)
    }

    def postinit()
    {
        ExpansionRecipes.initRecipes()

        // In dev mode, this module may load before transmission, therefore this must go in postInit
        if (loadPowerTest)
        {
            ItemPartWire.additionalWires :+= WireDef.POWER_100v.makeStack
            ItemPartFramedWire.additionalWires :+= WireDef.POWER_100v.makeFramedStack
        }
    }

    def createPart(name:String, client:Boolean) = name match
    {
        case "pr_100v" => new PowerWire100v
        case "pr_f100v" => new FramedPowerWire100v
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
        GuiManager.register(GuiRouterController, GuiIDs.routingController)
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
//        RenderLib.setRenderer(ProjectRedExpansion.machine1, 0, RenderFurnace)
        RenderLib.setRenderer(ProjectRedExpansion.machine2, 0, RenderController)
    }
}

object ExpansionProxy extends ExpansionProxy_client