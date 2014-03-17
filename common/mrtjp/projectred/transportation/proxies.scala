package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedTransportation._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraftforge.client.MinecraftForgeClient

class TransportationProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        PacketCustom.assignHandler(TransportationSPH.channel, TransportationSPH)
    }

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String](
            "pr_ptube", "pr_rbasic", "pr_rinterface",
            "pr_rcrafting", "pr_rrequest", "pr_rextension"
        ))

        itemPartPipe = new ItemPartPipe(Configurator.part_pipe.getInt)
        itemRoutingChip = new ItemRoutingChip(Configurator.item_routingChipID.getInt)
        itemRouterUtility = new ItemRouterUtility(Configurator.item_routerUtilID.getInt)

        for (i <- 0 until Configurator.routerUpdateThreadCount) new TableUpdateThread(i)
    }

    override def postinit()
    {
        TransportationRecipes.initRecipes()
    }

    override def createPart(name:String, client:Boolean) = name match
    {
        case "pr_ptube" => new BasicPipePart
        case "pr_rbasic" => new RoutedJunctionPipePart
        case "pr_rinterface" => new RoutedInterfacePipePart
        case "pr_rcrafting" => new RoutedCraftingPipePart
        case "pr_rrequest" => new RoutedRequestPipePart
        case "pr_rextension" => new RoutedExtensionPipePart
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class TransportationProxy_client extends TransportationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        PacketCustom.assignHandler(TransportationCPH.channel, TransportationCPH)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(itemPartPipe.itemID, PipeItemRenderer.instance)
        MicroMaterialRegistry.registerHighlightRenderer(PipeRSHighlightRenderer)
    }
}

object TransportationProxy extends TransportationProxy_client
