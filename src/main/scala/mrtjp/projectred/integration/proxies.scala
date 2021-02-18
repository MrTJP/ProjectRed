/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.texture.{AtlasRegistrar, IIconRegister, SpriteRegistryHelper}
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.event.ModelRegistryEvent
import net.minecraftforge.client.model.ModelLoaderRegistry
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

//class IntegrationProxy_server extends IProxy with IPartFactory
//{
//    override def preinit()
//    {
//        itemPartGate = new ItemPartGate
//        itemPartGate.setUnlocalizedName("projectred.integration.gate")
//        ForgeRegistries.ITEMS.register(itemPartGate.setRegistryName("gate"))
//
//        import GateDefinition._
//        MultiPartRegistry.registerParts(this, Array(
//            typeSimpleGate, typeComplexGate, typeArrayGate,
//            typeBundledGate, typeNeighborGate
//        ))
//    }
//
//    override def init()
//    {
//        PacketCustom.assignHandler(IntegrationSPH.channel, IntegrationSPH)
//    }
//
//    override def postinit(){}
//
//    override def createPart(name:ResourceLocation, client:Boolean) = name match
//    {
//        case GateDefinition.typeSimpleGate => new ComboGatePart
//        case GateDefinition.typeComplexGate => new SequentialGatePart
//        case GateDefinition.typeArrayGate => new ArrayGatePart
//        case GateDefinition.typeBundledGate => new BundledGatePart
//        case GateDefinition.typeNeighborGate => new SequentialGatePartT
//        case _ => null
//    }
//}
//
//class IntegrationProxy_client extends IntegrationProxy_server
//{
//    val timerGui = 10
//    val counterGui = 11
//
//    @SideOnly(Side.CLIENT)
//    override def preinit()
//    {
//        super.preinit()
//
//        ModelRegistryHelper.registerItemRenderer(itemPartGate, GateItemRenderer)
//        TextureUtils.addIconRegister(RenderGate)
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def init()
//    {
//        super.init()
//
//        PacketCustom.assignHandler(IntegrationCPH.channel, IntegrationCPH)
//
//        GuiHandler.register(GuiTimer, timerGui)
//        GuiHandler.register(GuiCounter, counterGui)
//    }
//}
//
//object IntegrationProxy extends IntegrationProxy_client

class IntegrationProxy extends IProxy
{
    override def construct():Unit = {
        ScorgeModLoadingContext.get.getModEventBus.register(this)
        IntegrationContent.register(ScorgeModLoadingContext.get.getModEventBus)
    }

    @SubscribeEvent
    override def commonSetup(event:FMLCommonSetupEvent):Unit = {
        IntegrationNetwork.init()
    }
}

class IntegrationProxyClient extends IntegrationProxy
{
    override def construct():Unit = {
        super.construct()
        ScorgeModLoadingContext.get.getModEventBus.addListener(onModelRegistryEvent)
        val spriteHelper = new SpriteRegistryHelper(ScorgeModLoadingContext.get.getModEventBus)
        spriteHelper.addIIconRegister((registrar:AtlasRegistrar) => {
            RenderGate.registerIcons(registrar)
        })
    }

    @SubscribeEvent
    @OnlyIn(Dist.CLIENT)
    override def clientSetup(event:FMLClientSetupEvent):Unit = {
    }

    @OnlyIn(Dist.CLIENT)
    def onModelRegistryEvent(event:ModelRegistryEvent) {
        ModelLoaderRegistry.registerLoader(new ResourceLocation(ProjectRedIntegration.MOD_ID, "gate"), new GateItemRenderer.Loader)
    }
}