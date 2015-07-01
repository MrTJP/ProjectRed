package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.MultiPartRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedTransportation._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraftforge.client.MinecraftForgeClient

class TransportationProxy_server extends IProxy with IPartFactory
{
    val guiIDChipUpgrade = 2
    val guiIDCraftingPipe = 3
    val guiIDExtensionPipe = 4
    val guiIDInterfacePipe = 5
    val guiIDFirewallPipe = 6
    val guiIDRoutingChips = 7

    override def preinit()
    {
        PacketCustom.assignHandler(TransportationSPH.channel, TransportationSPH)
    }

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String](
            "pr_pipe", "pr_rbasic", "pr_rinterface",
            "pr_rcrafting", "pr_rrequest", "pr_rextension",
            "pr_rfire", "pr_pt", "pr_rpt"
        ))

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
        case RESISTANCETUBE.partname => new ResistanceTube
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
        MicroMaterialRegistry.registerHighlightRenderer(PipeColourHighlightRenderer)

        GuiHandler.register(GuiChipUpgrade, guiIDChipUpgrade)
        GuiHandler.register(GuiCraftingPipe, guiIDCraftingPipe)
        GuiHandler.register(GuiExtensionPipe, guiIDExtensionPipe)
        GuiHandler.register(GuiInterfacePipe, guiIDInterfacePipe)
        GuiHandler.register(GuiFirewallPipe, guiIDFirewallPipe)
        GuiHandler.register(GuiChipConfig, guiIDRoutingChips)
    }
}

object TransportationProxy extends TransportationProxy_client
