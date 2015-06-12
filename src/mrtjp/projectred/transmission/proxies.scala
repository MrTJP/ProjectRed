package mrtjp.projectred.transmission

import codechicken.lib.data.MCDataInput
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.MultiPartRegistry.IPartFactory2
import codechicken.multipart.{MultiPartRegistry, TMultiPart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedTransmission._
import mrtjp.projectred.core.IProxy
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.client.MinecraftForgeClient

class TransmissionProxy_server extends IProxy with IPartFactory2
{
    override def preinit() {}

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String](
            "pr_redwire", "pr_insulated", "pr_bundled",
            "pr_fredwire", "pr_finsulated", "pr_fbundled",
            "pr_pwrlow", "pr_fpwrlow"
//            "pr_sredwire", "pr_sinsulated", "pr_sbundled" //legacy
        ))
        itemPartWire = new ItemPartWire
        itemPartFramedWire = new ItemPartFramedWire

        TransmissionRecipes.initTransmissionRecipes()
    }

    override def postinit(){}

    override def createPart(name:String, nbt:NBTTagCompound) = createPart(name)
    override def createPart(name:String, packet:MCDataInput) = createPart(name)

    def createPart(name:String):TMultiPart = name match
    {
        case "pr_redwire" => new RedAlloyWirePart
        case "pr_insulated" => new InsulatedRedAlloyPart
        case "pr_bundled" => new BundledCablePart
        case "pr_fredwire" => new FramedRedAlloyWirePart
        case "pr_finsulated" => new FramedInsulatedRedAlloyPart
        case "pr_fbundled" => new FramedBundledCablePart
        case "pr_pwrlow" => new LowLoadPowerLine
        case "pr_fpwrlow" => new FramedLowLoadPowerLine
//        //legacy
//        case "pr_sredwire" => new FramedRedAlloyWirePart
//        case "pr_sinsulated" => new FramedInsulatedRedAlloyPart
//        case "pr_sbundled" => new FramedBundledCablePart
        case _ => null
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class TransmissionProxy_client extends TransmissionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(itemPartWire, WireItemRenderer)
        MinecraftForgeClient.registerItemRenderer(itemPartFramedWire, FramedWireItemRenderer)
        MicroMaterialRegistry.registerHighlightRenderer(JacketedHighlightRenderer)
    }
}

object TransmissionProxy extends TransmissionProxy_client