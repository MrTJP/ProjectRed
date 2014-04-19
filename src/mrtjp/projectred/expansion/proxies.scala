package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.common.Loader
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.{Configurator, IProxy}
import mrtjp.projectred.transmission._
import mrtjp.projectred.core.libmc.{BasicRenderUtils, ItemBlockMulti}

class ExpansionProxy_server extends IProxy with IPartFactory
{
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
                MultiPartRegistry.registerParts(this, Array("pr_100v", "pr_f100v"))
                ItemPartWire.additionalWires :+= WireDef.POWER_100v.getItemStack
                ItemPartFramedWire.additionalWires :+= WireDef.POWER_100v.getFramedItemStack
            }

            //Machine1 (processing)
            ProjectRedExpansion.machine1 = new BlockMachine(Configurator.block_machinesID.getInt)
            ProjectRedExpansion.machine1.setUnlocalizedName("projectred.expansion.machine1")
            GameRegistry.registerBlock(ProjectRedExpansion.machine1, classOf[ItemBlockMulti], "projectred.expansion.machine1")
            //Machine1 tiles
            ProjectRedExpansion.machine1.addTile(0, classOf[TileRouterController], "projectred.expansion.machine1.controller")
            ProjectRedExpansion.machine1.addTile(1, classOf[TileFurnace], "projectred.expansion.machine1.furnace")

            //Machine2 (devices)
//            ProjectRedExpansion.machine2 = new BlockMachine(Configurator.block_machines2ID.getInt)
//            ProjectRedExpansion.machine2.setUnlocalizedName("projectred.expansion.machine2")
//            GameRegistry.registerBlock(ProjectRedExpansion.machine2, classOf[ItemBlockMulti], "projectred.expansion.machine2")
            //Machine2 tiles
            //...
        }
    }

    def postinit()
    {
        ExpansionRecipes.initRecipes()
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
            BasicRenderUtils.setRenderer(ProjectRedExpansion.machine1, 0, RenderController)
            BasicRenderUtils.setRenderer(ProjectRedExpansion.machine1, 1, RenderFurnace)
        }
    }
}

object ExpansionProxy extends ExpansionProxy_client