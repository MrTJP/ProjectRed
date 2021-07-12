/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.lib.texture.{AtlasRegistrar, IIconRegister, SpriteRegistryHelper}
import codechicken.lib.util.ResourceUtils
import mrtjp.projectred.ProjectRedIntegration
import mrtjp.projectred.core.IProxy
import net.minecraft.util.ResourceLocation
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.event.ModelRegistryEvent
import net.minecraftforge.client.model.ModelLoaderRegistry
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

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
        ResourceUtils.registerReloadListener(RenderGate)
    }

    @OnlyIn(Dist.CLIENT)
    def onModelRegistryEvent(event:ModelRegistryEvent) {
        ModelLoaderRegistry.registerLoader(new ResourceLocation(ProjectRedIntegration.MOD_ID, "gate"), new GateItemRenderer.Loader)
    }
}