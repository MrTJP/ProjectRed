package mrtjp.projectred.exploration

import codechicken.microblock.BlockMicroMaterial
import cpw.mods.fml.common.network.IGuiHandler
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.ProjectRedExploration._
import mrtjp.projectred.core.{PRColors, Configurator, IProxy}
import mrtjp.projectred.exploration.BlockOre.EnumOre
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone
import mrtjp.projectred.exploration.BlockStainedLeaf.EnumDyeTrees
import mrtjp.projectred.exploration.ItemGemSaw.GemSawItemRenderer
import net.minecraft.block.Block
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.common.{EnumHelper, MinecraftForge}
import net.minecraftforge.oredict.OreDictionary

class ExplorationProxy_server extends IProxy with IGuiHandler
{
    override def preinit() {}

    override def init()
    {
        if (Configurator.retroGeneration) RetroGenerationManager.registerRetroGenerators()
        else GameRegistry.registerWorldGenerator(GenerationManager.instance)

        itemWoolGin = new ItemWoolGin(Configurator.item_woolginID.getInt)
        itemBackpack = new ItemBackpack(Configurator.item_backpackID.getInt)

        blockOres = new BlockOre(Configurator.block_oresID.getInt)
        GameRegistry.registerBlock(blockOres, classOf[ItemBlockOre], "projectred.exploration.ore")
        for (o <- EnumOre.VALID_ORES) MinecraftForge.setBlockHarvestLevel(blockOres, "pickaxe", o.harvesLevel)

        blockStones = new BlockSpecialStone(Configurator.block_stonesID.getInt)
        GameRegistry.registerBlock(blockStones, classOf[ItemBlockSpecialStone], "projectred.exploration.stone")

        blockStoneWalls = new BlockSpecialStoneWall(Configurator.block_stoneWallsID.getInt)
        GameRegistry.registerBlock(blockStoneWalls, classOf[ItemBlockSpecialStoneWalls], "projectred.exploration.stonewalls")

        blockStainedLeaf = new BlockStainedLeaf(Configurator.block_stainedLeafID.getInt)
        GameRegistry.registerBlock(blockStainedLeaf, classOf[ItemBlockMetaHandler], "projectred.exploration.dyeleaf")

        blockStainedSapling = new BlockStainedSapling(Configurator.block_stainedSaplingID.getInt)
        GameRegistry.registerBlock(blockStainedSapling, classOf[ItemBlockStainedSapling], "projectred.exploration.dyesapling")
        for (i <- 0 until 16) OreDictionary.registerOre(PRColors.get(i).getOreDict, EnumDyeTrees.VALID_FOLIAGE(i).getSappling)

        if (Configurator.gen_SpreadingMoss.getBoolean(true))
        {
            val mc = Block.cobblestoneMossy.blockID
            Block.blocksList(mc) = null
            new BlockPhotosyntheticCobblestone(mc)
            val sb = Block.stoneBrick.blockID
            Block.blocksList(sb) = null
            new BlockPhotosyntheticStoneBrick(sb)
        }

        toolMaterialRuby = EnumHelper.addToolMaterial("RUBY", 2, 500, 8.0F, 4, 12)
        toolMaterialSapphire = EnumHelper.addToolMaterial("SAPPHIRE", 2, 500, 8.0F, 3, 16)
        toolMaterialPeridot = EnumHelper.addToolMaterial("PERIDOT", 2, 500, 8.75F, 3.25F, 12)

        itemRubyAxe = new ItemGemAxe(Configurator.item_rubyAxe.getInt, EnumSpecialTool.RUBYAXE)
        itemSapphireAxe = new ItemGemAxe(Configurator.item_sapphireAxe.getInt, EnumSpecialTool.SAPPHIREAXE)
        itemPeridotAxe = new ItemGemAxe(Configurator.item_peridotAxe.getInt, EnumSpecialTool.PERIDOTAXE)
        MinecraftForge.setToolClass(itemRubyAxe, "axe", 2)
        MinecraftForge.setToolClass(itemSapphireAxe, "axe", 2)
        MinecraftForge.setToolClass(itemPeridotAxe, "axe", 2)

        itemRubyHoe = new ItemGemHoe(Configurator.item_rubyHoe.getInt, EnumSpecialTool.RUBYHOE)
        itemSapphireHoe = new ItemGemHoe(Configurator.item_sapphireHoe.getInt, EnumSpecialTool.SAPPHIREHOE)
        itemPeridotHoe = new ItemGemHoe(Configurator.item_peridotHoe.getInt, EnumSpecialTool.PERIDOTHOE)
        MinecraftForge.setToolClass(itemRubyHoe, "hoe", 2)
        MinecraftForge.setToolClass(itemSapphireHoe, "hoe", 2)
        MinecraftForge.setToolClass(itemPeridotHoe, "hoe", 2)

        itemRubyPickaxe = new ItemGemPickaxe(Configurator.item_rubyPickaxe.getInt, EnumSpecialTool.RUBYPICKAXE)
        itemSapphirePickaxe = new ItemGemPickaxe(Configurator.item_sapphirePickaxe.getInt, EnumSpecialTool.SAPPHIREPICKAXE)
        itemPeridotPickaxe = new ItemGemPickaxe(Configurator.item_peridotPickaxe.getInt, EnumSpecialTool.PERIDOTPICKAXE)
        MinecraftForge.setToolClass(itemRubyPickaxe, "pickaxe", 2)
        MinecraftForge.setToolClass(itemSapphirePickaxe, "pickaxe", 2)
        MinecraftForge.setToolClass(itemPeridotPickaxe, "pickaxe", 2)

        itemRubyShovel = new ItemGemShovel(Configurator.item_rubyShovel.getInt, EnumSpecialTool.RUBYSHOVEL)
        itemSapphireShovel = new ItemGemShovel(Configurator.item_sapphireShovel.getInt, EnumSpecialTool.SAPPHIRESHOVEL)
        itemPeridotShovel = new ItemGemShovel(Configurator.item_peridotShovel.getInt, EnumSpecialTool.PERIDOTSHOVEL)

        MinecraftForge.setToolClass(itemRubyShovel, "shovel", 2)
        MinecraftForge.setToolClass(itemSapphireShovel, "shovel", 2)
        MinecraftForge.setToolClass(itemPeridotShovel, "shovel", 2)

        itemRubySword = new ItemGemSword(Configurator.item_rubySword.getInt, EnumSpecialTool.RUBYSWORD)
        itemSapphireSword = new ItemGemSword(Configurator.item_sapphireSword.getInt, EnumSpecialTool.SAPPHIRESWORD)
        itemPeridotSword = new ItemGemSword(Configurator.item_peridotSword.getInt, EnumSpecialTool.PERIDOTSWORD)

        MinecraftForge.setToolClass(itemRubySword, "sword", 2)
        MinecraftForge.setToolClass(itemSapphireSword, "sword", 2)
        MinecraftForge.setToolClass(itemPeridotSword, "sword", 2)

        itemGoldSaw = new ItemGemSaw(Configurator.item_goldSaw.getInt, EnumSpecialTool.GOLDSAW)
        itemRubySaw = new ItemGemSaw(Configurator.item_rubySaw.getInt, EnumSpecialTool.RUBYSAW)
        itemSapphireSaw = new ItemGemSaw(Configurator.item_sapphireSaw.getInt, EnumSpecialTool.SAPPHIRESAW)
        itemPeridotSaw = new ItemGemSaw(Configurator.item_peridotSaw.getInt, EnumSpecialTool.PERIDOTSAW)

        itemWoodSickle = new ItemGemSickle(Configurator.item_woodSickle.getInt, EnumSpecialTool.WOODSICKLE)
        itemStoneSickle = new ItemGemSickle(Configurator.item_stoneSickle.getInt, EnumSpecialTool.STONESICKLE)
        itemIronSickle = new ItemGemSickle(Configurator.item_ironSickle.getInt, EnumSpecialTool.IRONSICKLE)
        itemGoldSickle = new ItemGemSickle(Configurator.item_goldSickle.getInt, EnumSpecialTool.GOLDSICKLE)
        itemRubySickle = new ItemGemSickle(Configurator.item_rubySickle.getInt, EnumSpecialTool.RUBYSICKLE)
        itemSapphireSickle = new ItemGemSickle(Configurator.item_sapphireSickle.getInt, EnumSpecialTool.SAPPHIRESICKLE)
        itemPeridotSickle = new ItemGemSickle(Configurator.item_peridotSickle.getInt, EnumSpecialTool.PERIDOTSICKLE)
        itemDiamondSickle = new ItemGemSickle(Configurator.item_diamondSickle.getInt, EnumSpecialTool.DIAMONDSICKLE)

        for (s <- EnumSpecialStone.VALID_STONE) BlockMicroMaterial.createAndRegister(ProjectRedExploration.blockStones, s.meta)
    }

    override def postinit()
    {
        ExplorationRecipes.initOreDict()
        ExplorationRecipes.initRecipes()
    }

    final val ID_Bag = 1

    def getServerGuiElement(ID:Int, player:EntityPlayer, world:World, x:Int, y:Int, z:Int):AnyRef =
    {
        if (ID == ID_Bag)
        {
            val held = player.getHeldItem
            if (held.itemID == ProjectRedExploration.itemBackpack.itemID) return ItemBackpack.getContainer(player)
        }
        null
    }

    def getClientGuiElement(ID:Int, player:EntityPlayer, world:World, x:Int, y:Int, z:Int):AnyRef =
    {
        if (ID == ID_Bag)
        {
            val held = player.getHeldItem
            if (held.itemID == ProjectRedExploration.itemBackpack.itemID) return new GuiBackpack(player, ItemBackpack.getBackpackInventory(player), held)
        }
        null
    }
}

class ExplorationProxy_client extends ExplorationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MinecraftForgeClient.registerItemRenderer(itemGoldSaw.itemID, GemSawItemRenderer.instance)
        MinecraftForgeClient.registerItemRenderer(itemRubySaw.itemID, GemSawItemRenderer.instance)
        MinecraftForgeClient.registerItemRenderer(itemSapphireSaw.itemID, GemSawItemRenderer.instance)
        MinecraftForgeClient.registerItemRenderer(itemPeridotSaw.itemID, GemSawItemRenderer.instance)
    }
}

object ExplorationProxy extends ExplorationProxy_client
