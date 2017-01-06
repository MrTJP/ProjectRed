package mrtjp.projectred.transportation

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.packet.PacketCustom
import codechicken.lib.texture.TextureUtils
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.{IPartFactory, MultiPartRegistry}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedTransportation._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TransportationProxy_server extends IProxy with IPartFactory
{
    val guiIDInterfacePipe = 5
    val guiIDFirewallPipe = 6
    val guiIDRoutingChips = 7

    override def preinit()
    {
        itemPartPipe = new ItemPartPipe
        itemPartPipe.setUnlocalizedName("projectred.transportation.pipe")
        GameRegistry.register(itemPartPipe.setRegistryName("pipe"))

        itemRoutingChip = new ItemRoutingChip
        itemRoutingChip.setUnlocalizedName("projectred.transportation.routingChip")
        GameRegistry.register(itemRoutingChip.setRegistryName("routing_chip"))

        itemRouterUtility = new ItemRouterUtility
        itemRouterUtility.setUnlocalizedName("projectred.transportation.routerUtility")
        GameRegistry.register(itemRouterUtility.setRegistryName("router_utility"))

        MultiPartRegistry.registerParts(this, PipeDefs.values.map{_.partname}.toArray)
    }

    override def init()
    {
        TransportationRecipes.initRecipes()
    }

    override def postinit()
    {
        PacketCustom.assignHandler(TransportationSPH.channel, TransportationSPH)

        for (i <- 0 until Configurator.routerUpdateThreadCount) new TableUpdateThread(i)
    }

    import mrtjp.projectred.transportation.PipeDefs._
    override def createPart(name:String, client:Boolean) = name match
    {
        case BASIC.partname => new BasicPipePart
        case ROUTEDJUNCTION.partname => new RoutedJunctionPipePart
        case ROUTEDINTERFACE.partname => new RoutedInterfacePipePart
        case ROUTEDREQUEST.partname => new RoutedRequestPipePart
        case ROUTEDFIREWALL.partname => new RoutedFirewallPipe
        case PRESSURETUBE.partname => new PressureTube
        case RESISTANCETUBE.partname => new ResistanceTube
        case NETWORKVALVE.partname => new NetworkValvePipePart
        case NETWORKLATENCY.partname => new NetworkLatencyPipePart
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

        ModelRegistryHelper.registerItemRenderer(itemPartPipe, PipeItemRenderer)

        for (i <- RoutingChipDefs.values)
            i.setCustomModelResourceLocations()

        ModelLoader.setCustomModelResourceLocation(itemRouterUtility, 0,
            new ModelResourceLocation("projectred:mechanical/tools", "type=router_utility"))
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MicroMaterialRegistry.registerHighlightRenderer(PipeRSHighlightRenderer)
        MicroMaterialRegistry.registerHighlightRenderer(PipeColourHighlightRenderer)

        TextureUtils.addIconRegister(RenderPipe)
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
        PacketCustom.assignHandler(TransportationCPH.channel, TransportationCPH)

        GuiHandler.register(GuiInterfacePipe, guiIDInterfacePipe)
        GuiHandler.register(GuiFirewallPipe, guiIDFirewallPipe)
        GuiHandler.register(GuiChipConfig, guiIDRoutingChips)
    }
}

object TransportationProxy extends TransportationProxy_client