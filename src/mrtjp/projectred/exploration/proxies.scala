package mrtjp.projectred.exploration

import codechicken.microblock.BlockMicroMaterial
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.ProjectRedExploration._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.common.util.EnumHelper

class ExplorationProxy_server extends IProxy
{
    val guiIDBackpack = 1

    override def preinit() {}

    override def init()
    {
        if (Configurator.retroGeneration) RetroGenerationManager.registerRetroGenerators()
        else GameRegistry.registerWorldGenerator(GenerationManager.instance, 0)

        itemWoolGin = new ItemWoolGin
        itemBackpack = new ItemBackpack

        blockOres = new BlockOre
        for (o <- OreDefs.values) blockOres.setHarvestLevel("pickaxe", o.harvest, o.meta)

        blockDecoratives = new BlockDecoratives

        blockDecorativeWalls = new BlockDecorativeWalls

//        if (Configurator.gen_SpreadingMoss.getBoolean(true)) //TODO fix
//        {
//            val mc = Block.cobblestoneMossy.blockID
//            Block.blocksList(mc) = null
//            new BlockPhotosyntheticCobblestone(mc)
//            val sb = Block.stoneBrick.blockID
//            Block.blocksList(sb) = null
//            new BlockPhotosyntheticStoneBrick(sb)
//        }

        toolMaterialRuby = EnumHelper.addToolMaterial("RUBY", 2, 512, 8.0F, 4, 12)
        toolMaterialSapphire = EnumHelper.addToolMaterial("SAPPHIRE", 2, 512, 8.0F, 3, 16)
        toolMaterialPeridot = EnumHelper.addToolMaterial("PERIDOT", 2, 512, 8.75F, 3.25F, 12)

        itemRubyAxe = new ItemGemAxe(ToolDefs.RUBYAXE)
        itemSapphireAxe = new ItemGemAxe(ToolDefs.SAPPHIREAXE)
        itemPeridotAxe = new ItemGemAxe(ToolDefs.PERIDOTAXE)

        itemRubyHoe = new ItemGemHoe(ToolDefs.RUBYHOE)
        itemSapphireHoe = new ItemGemHoe(ToolDefs.SAPPHIREHOE)
        itemPeridotHoe = new ItemGemHoe(ToolDefs.PERIDOTHOE)

        itemRubyPickaxe = new ItemGemPickaxe(ToolDefs.RUBYPICKAXE)
        itemSapphirePickaxe = new ItemGemPickaxe(ToolDefs.SAPPHIREPICKAXE)
        itemPeridotPickaxe = new ItemGemPickaxe(ToolDefs.PERIDOTPICKAXE)

        itemRubyShovel = new ItemGemShovel(ToolDefs.RUBYSHOVEL)
        itemSapphireShovel = new ItemGemShovel(ToolDefs.SAPPHIRESHOVEL)
        itemPeridotShovel = new ItemGemShovel(ToolDefs.PERIDOTSHOVEL)

        itemRubySword = new ItemGemSword(ToolDefs.RUBYSWORD)
        itemSapphireSword = new ItemGemSword(ToolDefs.SAPPHIRESWORD)
        itemPeridotSword = new ItemGemSword(ToolDefs.PERIDOTSWORD)

        itemGoldSaw = new ItemGemSaw(ToolDefs.GOLDSAW)
        itemRubySaw = new ItemGemSaw(ToolDefs.RUBYSAW)
        itemSapphireSaw = new ItemGemSaw(ToolDefs.SAPPHIRESAW)
        itemPeridotSaw = new ItemGemSaw(ToolDefs.PERIDOTSAW)

        itemWoodSickle = new ItemGemSickle(ToolDefs.WOODSICKLE)
        itemStoneSickle = new ItemGemSickle(ToolDefs.STONESICKLE)
        itemIronSickle = new ItemGemSickle(ToolDefs.IRONSICKLE)
        itemGoldSickle = new ItemGemSickle(ToolDefs.GOLDSICKLE)
        itemRubySickle = new ItemGemSickle(ToolDefs.RUBYSICKLE)
        itemSapphireSickle = new ItemGemSickle(ToolDefs.SAPPHIRESICKLE)
        itemPeridotSickle = new ItemGemSickle(ToolDefs.PERIDOTSICKLE)
        itemDiamondSickle = new ItemGemSickle(ToolDefs.DIAMONDSICKLE)

        for (s <- DecorativeStoneDefs.values)
            BlockMicroMaterial.createAndRegister(ProjectRedExploration.blockDecoratives, s.meta)

        ExplorationRecipes.initOreDict()
        ExplorationRecipes.initRecipes()
    }

    override def postinit(){}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class ExplorationProxy_client extends ExplorationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(itemGoldSaw, GemSawRenderer)
        MinecraftForgeClient.registerItemRenderer(itemRubySaw, GemSawRenderer)
        MinecraftForgeClient.registerItemRenderer(itemSapphireSaw, GemSawRenderer)
        MinecraftForgeClient.registerItemRenderer(itemPeridotSaw, GemSawRenderer)

        GuiHandler.register(GuiBackpack, guiIDBackpack)
    }
}

object ExplorationProxy extends ExplorationProxy_client
