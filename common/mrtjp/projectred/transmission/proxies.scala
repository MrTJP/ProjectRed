package mrtjp.projectred.transmission

import codechicken.multipart.{TMultiPart, MultiPartRegistry}
import codechicken.multipart.MultiPartRegistry.IPartFactory
import mrtjp.projectred.ProjectRedTransmission._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraftforge.client.MinecraftForgeClient
import codechicken.microblock.MicroMaterialRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}

class TransmissionProxy_server extends IProxy with IPartFactory
{
    override def preinit() {}

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String](
            "pr_redwire", "pr_insulated", "pr_bundled",
            "pr_fredwire", "pr_finsulated", "pr_fbundled",
            "pr_sredwire", "pr_sinsulated", "pr_sbundled" //legacy
        ))
        itemPartWire = new ItemPartWire(Configurator.part_wire.getInt)
        itemPartFramedWire = new ItemPartFramedWire(Configurator.part_jwire.getInt)
    }

    override def postinit()
    {
        WireDef.initOreDict()
        TransmissionRecipes.initTransmissionRecipes()
    }

    override def createPart(name:String, client:Boolean) = name match
    {
        case "pr_redwire" => new RedAlloyWirePart
        case "pr_insulated" => new InsulatedRedAlloyPart
        case "pr_bundled" => new BundledCablePart
        case "pr_fredwire" => new FramedRedAlloyWirePart
        case "pr_finsulated" => new FramedInsulatedRedAlloyPart
        case "pr_fbundled" => new FramedBundledCablePart
        //legacy
        case "pr_sredwire" => new FramedRedAlloyWirePart
        case "pr_sinsulated" => new FramedInsulatedRedAlloyPart
        case "pr_sbundled" => new FramedBundledCablePart
    }
}

class TransmissionProxy_client extends TransmissionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(itemPartWire.itemID, WireItemRenderer.instance)
        MinecraftForgeClient.registerItemRenderer(itemPartFramedWire.itemID, FramedWireItemRenderer.instance)
        MicroMaterialRegistry.registerHighlightRenderer(new JacketedHighlightRenderer)
    }
}

object TransmissionProxy extends TransmissionProxy_client