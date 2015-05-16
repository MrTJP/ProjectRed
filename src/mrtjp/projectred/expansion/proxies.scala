package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import cpw.mods.fml.common.Loader
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.{Configurator, IProxy}
import ProjectRedExpansion._

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

//        //Machine1 (machines)
//        ProjectRedExpansion.machine1 = new BlockMachine("projectred.expansion.machine1")
//        ProjectRedExpansion.machine1.addTile(classOf[TileFurnace], 0)

        //Machine2 (devices)
        machine2 = new BlockMachine("projectred.expansion.machine2")
        machine2.addTile(classOf[TileBlockBreaker], 0)
        machine2.addTile(classOf[TileItemImporter], 1)
        machine2.addTile(classOf[TileBlockPlacer], 2)

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
    val blockPlacerGui = 20

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
        TileRenderRegistry.setRenderer(machine2, 0, RenderBlockBreaker)
        TileRenderRegistry.setRenderer(machine2, 1, RenderItemRemover)
        TileRenderRegistry.setRenderer(machine2, 2, RenderBlockPlacer)

        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
    }
}

object ExpansionProxy extends ExpansionProxy_client