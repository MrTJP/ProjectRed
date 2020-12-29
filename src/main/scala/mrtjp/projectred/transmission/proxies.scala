package mrtjp.projectred.transmission

import codechicken.lib.texture.SpriteRegistryHelper
import codechicken.microblock.MicroMaterialRegistry
import mrtjp.projectred.ProjectRedTransmission.MOD_ID
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.event.ModelRegistryEvent
import net.minecraftforge.client.model.ModelLoaderRegistry
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

class TransmissionProxy extends IProxy
{
//    override def preinit()
//    {
//        itemPartWire = new ItemPartWire
//        itemPartWire.setUnlocalizedName("projectred.transmission.wire")
//        ForgeRegistries.ITEMS.register(itemPartWire.setRegistryName("wire"))
//
//        itemPartFramedWire = new ItemPartFramedWire
//        itemPartFramedWire.setUnlocalizedName("projectred.transmission.wireFramed")
//        ForgeRegistries.ITEMS.register(itemPartFramedWire.setRegistryName("framed_wire"))
//
//        import WireDef._
//        MultiPartRegistry.registerParts(this, Array(
//            typeRedAlloy, typeInsulated, typeBundled,
//            typeFramedRedAlloy, typeFramedInsulated, typeFramedBundled,
//            typeLowLoad, typeFramedLowLoad
//        ))
//
//        WireDef.initOreDict()
//    }

//    override def init(){}
//
//    override def postinit(){}

//    override def createPart(name:ResourceLocation, client:Boolean):TMultiPart = name match
//    {
//        case WireDef.typeRedAlloy => new RedAlloyWirePart
//        case WireDef.typeInsulated => new InsulatedRedAlloyPart
//        case WireDef.typeBundled => new BundledCablePart
//        case WireDef.typeFramedRedAlloy=> new FramedRedAlloyWirePart
//        case WireDef.typeFramedInsulated => new FramedInsulatedRedAlloyPart
//        case WireDef.typeFramedBundled => new FramedBundledCablePart
//        case WireDef.typeLowLoad => new LowLoadPowerLine
//        case WireDef.typeFramedLowLoad => new FramedLowLoadPowerLine
//        case _ => null
//    }
}

class TransmissionProxyClient extends TransmissionProxy
{

    lazy val spriteHelper = new SpriteRegistryHelper(ScorgeModLoadingContext.get.getModEventBus)

    override def construct() {
        super.construct()
        ScorgeModLoadingContext.get.getModEventBus.addListener(onModelRegistryEvent)
        for(wireType <- WireType.values()) wireType.registerTextures(spriteHelper)
    }

    override def clientSetup(event: FMLClientSetupEvent) {
        MicroMaterialRegistry.registerHighlightRenderer(RenderFramedWire)
    }

    def onModelRegistryEvent(event: ModelRegistryEvent) {
        ModelLoaderRegistry.registerLoader(new ResourceLocation(MOD_ID, "wire"), new WireModelLoader)
        ModelLoaderRegistry.registerLoader(new ResourceLocation(MOD_ID, "framed_wire"), new FramedWireModelLoader)
    }
}
