package mrtjp.projectred.illumination

import codechicken.lib.model.ModelRegistryHelper
import codechicken.lib.model.blockbakery.{BlockBakery, CCBakeryModel, IBlockStateKeyGenerator}
import codechicken.lib.texture.TextureUtils
import mrtjp.core.block.MultiTileBlock
import mrtjp.projectred.ProjectRedIllumination._
import mrtjp.projectred.core.IProxy
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.client.renderer.block.statemap.StateMap
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class IlluminationProxy_server extends IProxy
{
    val lights = Seq(LightFactoryLantern, LightFactoryFixture, LightFactoryFallout, LightFactoryCage)

    override def preinit()
    {
        blockLamp = new BlockLamp
        blockLamp.setUnlocalizedName("projectred.illumination.lamp")
        GameRegistry.register(blockLamp.setRegistryName("lamp"))
        itemBlockLamp = new ItemBlockLamp
        GameRegistry.register(itemBlockLamp.setRegistryName(blockLamp.getRegistryName))
        blockLamp.addTile(classOf[TileLamp], 0)

        //TODO Add one of these lights!
//        blockAirousLight = new BlockAirousLight
//        blockAirousLight.bindTile(classOf[TileAirousLight])

        lights.foreach(_.register())
    }

    override def init()
    {
//        MultiPartRegistry.registerParts(this, (lights.map(_.getType) :+ "pr_lightbutton" :+ "pr_flightbutton").toArray)
//        for (l <- lights) l.initServer()

//        itemPartIllumarButton = new ItemPartButton
//        itemPartIllumarFButton = new ItemPartFButton

        IlluminationRecipes.initRecipes()
        LightMicroMaterial.register()
    }

    override def postinit(){}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IlluminationProxy_client extends IlluminationProxy_server
{

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()

        ModelLoader.setCustomStateMapper(blockLamp, new StateMap.Builder().ignore(MultiTileBlock.TILE_INDEX).build())
        ModelRegistryHelper.register(new ModelResourceLocation(blockLamp.getRegistryName, "normal"), new CCBakeryModel(""))
        ModelRegistryHelper.registerItemRenderer(itemBlockLamp, LampRenderer)
        TextureUtils.addIconRegister(LampBakery)
        BlockBakery.registerBlockKeyGenerator(blockLamp, new IBlockStateKeyGenerator {
            override def generateKey(state: IExtendedBlockState):String = {
                val colour = state.getValue(BlockProperties.UNLISTED_COLOUR_PROPERTY)
                val isOn = state.getValue(BlockProperties.UNLISTED_ON_PROPERTY)
                val meta = state.getBlock.getMetaFromState(state)
                state.getBlock.getRegistryName.toString + s",meta=$meta,c=$colour,o=$isOn"
            }
        })

        lights.foreach(_.registerClient())
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
//        MinecraftForgeClient.registerItemRenderer(itemPartIllumarButton, RenderButton)
//        MinecraftForgeClient.registerItemRenderer(itemPartIllumarFButton, RenderFButton)

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileLamp], LampRenderer)
    }

    var getLightValue = (meta:Int, brightness:Int) => brightness
}

object IlluminationProxy extends IlluminationProxy_client
