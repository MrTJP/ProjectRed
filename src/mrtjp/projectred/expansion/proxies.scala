package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.common.Loader
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.{GuiManager, IProxy}
import mrtjp.projectred.transmission._
import mrtjp.projectred.core.libmc.RenderLib
import mrtjp.projectred.transportation._

class ExpansionProxy_server extends IProxy with IPartFactory
{
    var loadPowerTest = false

    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        if (version.contains("@")) //dev only
        {
            if (Loader.isModLoaded("ProjRed|Transmission"))
            {
                loadPowerTest = true
                MultiPartRegistry.registerParts(this, Array("pr_100v", "pr_f100v"))
            }

            //Machine1 (processing)
            ProjectRedExpansion.machine1 = new BlockMachine("projectred.expansion.machine1")
            //Machine1 tiles
            ProjectRedExpansion.machine1.addTile(classOf[TileFurnace], 0)

            //Machine2 (devices)
            ProjectRedExpansion.machine2 = new BlockMachine("projectred.expansion.machine2")
            //Machine2 tiles
            ProjectRedExpansion.machine2.addTile(classOf[TileRouterController], 0)
        }
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
        case "pr_100v" => new PowerWire_100v
        case "pr_f100v" => new FramedPowerWire_100v
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        if (version.contains("@"))
        {
            RenderLib.setRenderer(ProjectRedExpansion.machine1, 0, RenderController)
            RenderLib.setRenderer(ProjectRedExpansion.machine1, 1, RenderFurnace)
        }

        import mrtjp.projectred.core.GuiIDs._
        GuiManager.register(GuiChipUpgrade, chipUpgrade)
        GuiManager.register(GuiCraftingPipe, craftingPipe)
        GuiManager.register(GuiExtensionPipe, extensionPipe)
        GuiManager.register(GuiInterfacePipe, interfacePipe)
        GuiManager.register(GuiFirewallPipe, firewallPipe)
        GuiManager.register(ChipGuiFactory, routingChips)
    }
}

object ExpansionProxy extends ExpansionProxy_client