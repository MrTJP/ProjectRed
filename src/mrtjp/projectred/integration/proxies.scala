package mrtjp.projectred.integration

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.ProjectRedIntegration._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraftforge.client.MinecraftForgeClient

class IntegrationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        PacketCustom.assignHandler(IntegrationSPH.channel, IntegrationSPH)
    }

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String](
            "pr_sgate", "pr_igate", "pr_agate",
            "pr_bgate", "pr_tgate", "pr_rgate"
        ))

        itemPartGate = new ItemPartGate(Configurator.part_gate.getInt)
    }

    override def postinit()
    {
        IntegrationRecipes.initRecipes()
    }

    override def createPart(name:String, client:Boolean) = name match
    {
        case "pr_sgate" => new SimpleGatePart
        case "pr_igate" => new InstancedRsGatePart
        case "pr_bgate" => new BundledGatePart
        case "pr_agate" => new ArrayGatePart
        case "pr_tgate" => new InstancedRsGatePartT
        case "pr_rgate" => new RowGatePart
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IntegrationProxy_client extends IntegrationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        PacketCustom.assignHandler(IntegrationCPH.channel, IntegrationCPH)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(ProjectRedIntegration.itemPartGate.itemID, GateItemRenderer.instance)
    }
}

object IntegrationProxy extends IntegrationProxy_client