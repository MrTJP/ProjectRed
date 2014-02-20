package mrtjp.projectred.expansion

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.blockutil.ItemBlockMulti
import mrtjp.projectred.core.{BasicRenderUtils, Configurator, IProxy}
import mrtjp.projectred.transmission.{PowerWire_100v, FramedPowerWire_100v}

class ExpansionProxy_server extends IProxy with IPartFactory
{
    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        MultiPartRegistry.registerParts(this, Array("pr_100v", "pr_f100v"))

        ProjectRedExpansion.machine1 = new BlockMachine(Configurator.block_machinesID.getInt)
        ProjectRedExpansion.machine1.setUnlocalizedName("projectred.expansion.machine1")
        GameRegistry.registerBlock(ProjectRedExpansion.machine1, classOf[ItemBlockMulti], "projectred.expansion.machine1")
        ProjectRedExpansion.machine1.addTile(0, classOf[TileFurnace], "projectred.expansion.machine1.furnace")
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

        BasicRenderUtils.setRenderer(ProjectRedExpansion.machine1, 0, RenderFurnace)
    }
}

object ExpansionProxy extends ExpansionProxy_client