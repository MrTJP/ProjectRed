package mrtjp.projectred.illumination

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.render.item.IItemRenderer
import codechicken.lib.texture.{AtlasRegistrar, SpriteRegistryHelper}
import com.google.gson.{JsonDeserializationContext, JsonObject}
import com.mojang.datafixers.util.Pair
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.IProxy
import net.minecraft.client.renderer.model._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.util.ResourceLocation
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.client.event.ModelRegistryEvent
import net.minecraftforge.client.model.geometry.IModelGeometry
import net.minecraftforge.client.model.{IModelConfiguration, IModelLoader, ModelLoaderRegistry}
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.event.lifecycle.{FMLClientSetupEvent, FMLCommonSetupEvent}
import net.minecraftforge.scorge.lang.ScorgeModLoadingContext

import java.util
import java.util.{Collections, function}

//class IlluminationProxy_server extends IProxy with IPartFactory
//{
//    val lights = Seq(LightFactoryLantern, LightFactoryFixture, LightFactoryFallout, LightFactoryCage)
//
//    override def preinit()
//    {
//        blockLamp = new BlockLamp
//        blockLamp.setUnlocalizedName("projectred.illumination.lamp")
//        ForgeRegistries.BLOCKS.register(blockLamp.setRegistryName("lamp"))
//        itemBlockLamp = new ItemBlockLamp
//        ForgeRegistries.ITEMS.register(itemBlockLamp.setRegistryName(blockLamp.getRegistryName))
//        blockLamp.addTile(classOf[TileLamp], 0)
//
//        itemPartIllumarButton = new ItemPartButton
//        itemPartIllumarButton.setUnlocalizedName("projectred.illumination.lightButton")
//        ForgeRegistries.ITEMS.register(itemPartIllumarButton.setRegistryName("light_button"))
//
//        itemPartIllumarFButton = new ItemPartFButton
//        itemPartIllumarFButton.setUnlocalizedName("projectred.illumination.lightButtonFeedback")
//        ForgeRegistries.ITEMS.register(itemPartIllumarFButton.setRegistryName("feedback_light_button"))
//
//        //TODO Add one of these lights!
////        blockAirousLight = new BlockAirousLight
////        blockAirousLight.bindTile(classOf[TileAirousLight])
//
//        lights.foreach(_.register())
//
//        MultiPartRegistry.registerParts(this, Array(LightButtonPart.typeID, FLightButtonPart.typeID))
//    }
//
//    override def init()
//    {
//        LightMicroMaterial.register()
//    }
//
//    override def postinit(){}
//
//    override def createPart(name:ResourceLocation, client:Boolean) = name match
//    {
//        case LightButtonPart.typeID => new LightButtonPart
//        case FLightButtonPart.typeID => new FLightButtonPart
//        case _ => null
//    }
//}
//
//class IlluminationProxy_client extends IlluminationProxy_server
//{
//
//    @SideOnly(Side.CLIENT)
//    override def preinit()
//    {
//        super.preinit()
//
//        ModelLoader.setCustomStateMapper(blockLamp, new StateMap.Builder().ignore(MultiTileBlock.TILE_INDEX).build())
//        ModelRegistryHelper.register(new ModelResourceLocation(blockLamp.getRegistryName, "normal"), new CCBakeryModel())
//        ModelRegistryHelper.registerItemRenderer(itemBlockLamp, LampRenderer)
//        TextureUtils.addIconRegister(LampBakery)
//        ModelBakery.registerBlockKeyGenerator(blockLamp, new IBlockStateKeyGenerator {
//            override def generateKey(state: IExtendedBlockState):String = {
//                val colour = state.getValue(BlockProperties.UNLISTED_COLOUR_PROPERTY)
//                val isOn = state.getValue(BlockProperties.UNLISTED_ON_PROPERTY)
//                val meta = state.getBlock.getMetaFromState(state)
//                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$colour,o=$isOn"
//            }
//        })
//
//        lights.foreach(_.registerClient())
//
//        ModelRegistryHelper.registerItemRenderer(itemPartIllumarButton, ButtonItemRenderer)
//        ModelRegistryHelper.registerItemRenderer(itemPartIllumarFButton, FButtonItemRenderer)
//    }
//
//    @SideOnly(Side.CLIENT)
//    override def init()
//    {
//        super.init()
////        MinecraftForgeClient.registerItemRenderer(itemPartIllumarButton, RenderButton)
////        MinecraftForgeClient.registerItemRenderer(itemPartIllumarFButton, RenderFButton)
//
//        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileLamp], LampRenderer)
//    }
//
//    var getLightValue = (meta:Int, brightness:Int) => brightness
//}
//
//object IlluminationProxy extends IlluminationProxy_client

class IlluminationProxy extends IProxy {
    override def construct():Unit = {
        super.construct()
        ScorgeModLoadingContext.get.getModEventBus.register(this)
        IlluminationContent.register(ScorgeModLoadingContext.get.getModEventBus)
        LightMicroMaterial.register()
    }

    @SubscribeEvent
    override def commonSetup(event:FMLCommonSetupEvent):Unit = {

    }
}

class IlluminationProxyClient extends IlluminationProxy {
    override def construct():Unit = {
        super.construct()
        ScorgeModLoadingContext.get.getModEventBus.addListener(onModelRegistryEvent)
        val spriteHelper = new SpriteRegistryHelper(ScorgeModLoadingContext.get.getModEventBus)
        val modelHelper = new ModelRegistryHelper(ScorgeModLoadingContext.get.getModEventBus)

        spriteHelper.addIIconRegister((registrar:AtlasRegistrar) => {
            for (light <- IlluminationContent.lightDefinitions) {
                light.registerIcons(registrar)
                light.loadModels()
            }
        })

        // Register a custom renderer for Illumar Lamps in item form (for rendering the glow)
        modelHelper.registerCallback { e =>
            for (colour <- 0 until 16) {
                val blockModel = e.getModelRegistry.get(
                    new ModelResourceLocation(IlluminationContent.invertedIllumarLampBlocks(colour).get().getRegistryName, "lit=true"))

                e.getModelRegistry.put(
                    new ModelResourceLocation(IlluminationContent.invertedIllumarLampBlockItems(colour).get().getRegistryName, "inventory"),
                    new IllumarLampItemRenderer(blockModel))
            }
        }
    }

    @OnlyIn(Dist.CLIENT)
    @SubscribeEvent
    override def clientSetup(event:FMLClientSetupEvent):Unit = {
        super.clientSetup(event)

        for (tile <- IlluminationContent.illumarLampTiles ++ IlluminationContent.invertedIllumarLampTiles)
            ClientRegistry.bindTileEntityRenderer(tile.get(), disp => new IllumarLampTileRender(disp))
    }

    @OnlyIn(Dist.CLIENT)
    def onModelRegistryEvent(event:ModelRegistryEvent):Unit = {

        class GenericModelLoader(renderer:IItemRenderer) extends IModelLoader[GenericModelLoader] with IModelGeometry[GenericModelLoader] {
            override def read(deserializationContext:JsonDeserializationContext, modelContents:JsonObject):GenericModelLoader = this
            override def getTextures(owner:IModelConfiguration,
                                     modelGetter:function.Function[ResourceLocation, IUnbakedModel],
                                     missingTextureErrors:util.Set[Pair[String, String]]):util.Collection[Material] = Collections.emptyList()
            override def bake(owner:IModelConfiguration,
                              bakery:ModelBakery,
                              spriteGetter:function.Function[Material, TextureAtlasSprite],
                              modelTransform:IModelTransform,
                              overrides:ItemOverrideList,
                              modelLocation:ResourceLocation):IBakedModel = renderer
        }

        for (light <- IlluminationContent.lightDefinitions)
            ModelLoaderRegistry.registerLoader(
                new ResourceLocation(ProjectRedIllumination.MOD_ID,
                light.getItemModelLoaderPath),
                new GenericModelLoader(light.getItemRenderer)
            )
    }
}