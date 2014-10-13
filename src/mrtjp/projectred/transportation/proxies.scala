package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedTransportation._
import mrtjp.projectred.core.{Configurator, GuiManager, IProxy}
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
            "pr_pipe", "pr_rbasic", "pr_rinterface",
            "pr_rcrafting", "pr_rrequest", "pr_rextension",
            "pr_rfire"
        ))

        if (Configurator.version.contains("@"))
            MultiPartRegistry.registerParts((_, _) => new PressureTube, "pr_pt")

        itemPartPipe = new ItemPartPipe
        itemRoutingChip = new ItemRoutingChip
        itemRouterUtility = new ItemRouterUtility

        for (i <- 0 until Configurator.routerUpdateThreadCount) new TableUpdateThread(i)

        TransportationRecipes.initRecipes()
    }

    override def postinit(){}

    import mrtjp.projectred.transportation.PipeDefs._
    override def createPart(name:String, client:Boolean) = name match
    {
        case BASIC.partname => new BasicPipePart
        case ROUTEDJUNCTION.partname => new RoutedJunctionPipePart
        case ROUTEDINTERFACE.partname => new RoutedInterfacePipePart
        case ROUTEDCRAFTING.partname => new RoutedCraftingPipePart
        case ROUTEDREQUEST.partname => new RoutedRequestPipePart
        case ROUTEDEXTENSION.partname => new RoutedExtensionPipePart
        case ROUTEDFIREWALL.partname => new RoutedFirewallPipe
        case PRESSURETUBE.partname => new PressureTube
        case _ => null
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
        MinecraftForgeClient.registerItemRenderer(itemPartPipe, PipeItemRenderer)
        MicroMaterialRegistry.registerHighlightRenderer(PipeRSHighlightRenderer)

        import mrtjp.projectred.core.GuiIDs._
        GuiManager.register(GuiChipUpgrade, chipUpgrade)
        GuiManager.register(GuiCraftingPipe, craftingPipe)
        GuiManager.register(GuiExtensionPipe, extensionPipe)
        GuiManager.register(GuiInterfacePipe, interfacePipe)
        GuiManager.register(GuiFirewallPipe, firewallPipe)
        GuiManager.register(ChipGuiFactory, routingChips)
    }
}

object TransportationProxy extends TransportationProxy_client
