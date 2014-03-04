package mrtjp.projectred.illumination

import codechicken.multipart.{TMultiPart, MultiPartRegistry}
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.ProjectRedIllumination._
import mrtjp.projectred.core.{Configurator, IProxy}
import codechicken.multipart.MultiPartRegistry.IPartFactory
import cpw.mods.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.client.MinecraftForgeClient
import mrtjp.projectred.ProjectRedIllumination
import cpw.mods.fml.client.registry.ClientRegistry
import net.minecraftforge.common.MinecraftForge
import mrtjp.projectred.core.utils.ItemKey

class IlluminationProxy_server extends IProxy with IPartFactory
{
    override def preinit() {}

    override def init()
    {
        MultiPartRegistry.registerParts(this, Array[String]("pr_lantern", "pr_lightbutton", "pr_cagelamp", "pr_fixture"))

        itemPartLantern = new ItemPartLantern(Configurator.part_lantern.getInt, false)
        itemPartInvLantern = new ItemPartLantern(Configurator.part_invlantern.getInt, true)
        itemPartCageLamp = new ItemPartCageLamp(Configurator.part_cagelamp.getInt, false)
        itemPartInvCageLamp = new ItemPartCageLamp(Configurator.part_invcagelamp.getInt, true)
        itemPartFixture = new ItemPartFixture(Configurator.part_fixture.getInt, false)
        itemPartInvFixture = new ItemPartFixture(Configurator.part_invfixture.getInt, true)

        itemPartIllumarButton = new ItemPartIllumarButton(Configurator.part_lightButton.getInt)

        blockLamp = new BlockLamp(Configurator.block_lampID.getInt)

        GameRegistry.registerBlock(blockLamp, classOf[ItemBlockLamp], "projectred.illumination.lamp")
        GameRegistry.registerTileEntity(classOf[TileLamp], "tile.projectred.illumination.lamp")

        blockAirousLight = new BlockAirousLight(Configurator.block_airousID.getInt)
        GameRegistry.registerTileEntity(classOf[TileAirousLight], "tile.projectred.illumination.airousLight")
    }

    override def postinit()
    {
        IlluminationRecipes.initRecipes()
    }

    override def createPart(name:String, client:Boolean) = name match
    {
        case "pr_lantern" => new LanternPart
        case "pr_lightbutton" => new IllumarButtonPart
        case "pr_cagelamp" => new CageLampPart
        case "pr_fixture" => new FixturePart
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class IlluminationProxy_client extends IlluminationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartLantern.itemID, RenderLantern.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvLantern.itemID, RenderLantern.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartIllumarButton.itemID, RenderIllumarButton.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartCageLamp.itemID, RenderCageLamp.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvCageLamp.itemID, RenderCageLamp.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartFixture.itemID, RenderFixture.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.itemPartInvFixture.itemID, RenderFixture.instance)
        MinecraftForgeClient.registerItemRenderer(ProjectRedIllumination.blockLamp.blockID, LampTESR.instance)

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileLamp], LampTESR.instance)

        MinecraftForge.EVENT_BUS.register(RenderHalo.instance)
    }
}

object IlluminationProxy extends IlluminationProxy_client