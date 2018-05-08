package mrtjp.projectred.transmission

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.texture.TextureUtils
import codechicken.microblock.MicroMaterialRegistry
import codechicken.multipart.api.IPartFactory
import codechicken.multipart.{MultiPartRegistry, TMultiPart}
import mrtjp.projectred.ProjectRedTransmission._
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.fml.common.registry.{ForgeRegistries, GameRegistry}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TransmissionProxy_server extends IProxy with IPartFactory
{
    override def preinit()
    {
        itemPartWire = new ItemPartWire
        itemPartWire.setUnlocalizedName("projectred.transmission.wire")
        ForgeRegistries.ITEMS.register(itemPartWire.setRegistryName("wire"))

        itemPartFramedWire = new ItemPartFramedWire
        itemPartFramedWire.setUnlocalizedName("projectred.transmission.wireFramed")
        ForgeRegistries.ITEMS.register(itemPartFramedWire.setRegistryName("framed_wire"))

        import WireDef._
        MultiPartRegistry.registerParts(this, Array(
            typeRedAlloy, typeInsulated, typeBundled,
            typeFramedRedAlloy, typeFramedInsulated, typeFramedBundled,
            typeLowLoad, typeFramedLowLoad
        ))
    }

    override def init()
    {
        WireDef.initOreDict()
    }

    override def postinit(){}

    override def createPart(name:ResourceLocation, client:Boolean):TMultiPart = name match
    {
        case WireDef.typeRedAlloy => new RedAlloyWirePart
        case WireDef.typeInsulated => new InsulatedRedAlloyPart
        case WireDef.typeBundled => new BundledCablePart
        case WireDef.typeFramedRedAlloy=> new FramedRedAlloyWirePart
        case WireDef.typeFramedInsulated => new FramedInsulatedRedAlloyPart
        case WireDef.typeFramedBundled => new FramedBundledCablePart
        case WireDef.typeLowLoad => new LowLoadPowerLine
        case WireDef.typeFramedLowLoad => new FramedLowLoadPowerLine
        case _ => null
    }
}

class TransmissionProxy_client extends TransmissionProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        ModelRegistryHelper.registerItemRenderer(itemPartWire, WireItemRenderer)
        ModelRegistryHelper.registerItemRenderer(itemPartFramedWire, FramedWireItemRenderer)
        TextureUtils.addIconRegister(RenderWire)
        MicroMaterialRegistry.registerHighlightRenderer(RenderFramedWire)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
    }
}

object TransmissionProxy extends TransmissionProxy_client
