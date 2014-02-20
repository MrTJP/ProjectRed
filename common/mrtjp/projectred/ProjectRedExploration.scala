package mrtjp.projectred

import cpw.mods.fml.common.Mod
import cpw.mods.fml.common.event.FMLInitializationEvent
import cpw.mods.fml.common.event.FMLPostInitializationEvent
import cpw.mods.fml.common.event.FMLPreInitializationEvent
import cpw.mods.fml.common.network.NetworkMod
import cpw.mods.fml.common.network.NetworkRegistry
import mrtjp.projectred.exploration._
import net.minecraft.block.Block
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.item.EnumToolMaterial
import net.minecraft.item.ItemStack

@Mod(modid = "ProjRed|Exploration", useMetadata = true, modLanguage = "scala")
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
object ProjectRedExploration
{
    /** Blocks **/
    var blockOres:BlockOre = null
    var blockStones:BlockSpecialStone = null
    var blockStoneWalls:BlockSpecialStoneWall = null
    var blockStainedLeaf:BlockStainedLeaf = null
    var blockStainedSapling:BlockStainedSapling = null

    /** Items **/
    var itemWoolGin:ItemWoolGin = null
    var itemBackpack:ItemBackpack = null
    var toolMaterialRuby:EnumToolMaterial = null
    var toolMaterialSapphire:EnumToolMaterial = null
    var toolMaterialPeridot:EnumToolMaterial = null
    var itemRubyAxe:ItemGemAxe = null
    var itemSapphireAxe:ItemGemAxe = null
    var itemPeridotAxe:ItemGemAxe = null
    var itemRubyHoe:ItemGemHoe = null
    var itemSapphireHoe:ItemGemHoe = null
    var itemPeridotHoe:ItemGemHoe = null
    var itemRubyPickaxe:ItemGemPickaxe = null
    var itemSapphirePickaxe:ItemGemPickaxe = null
    var itemPeridotPickaxe:ItemGemPickaxe = null
    var itemRubyShovel:ItemGemShovel = null
    var itemSapphireShovel:ItemGemShovel = null
    var itemPeridotShovel:ItemGemShovel = null
    var itemRubySword:ItemGemSword = null
    var itemSapphireSword:ItemGemSword = null
    var itemPeridotSword:ItemGemSword = null
    var itemGoldSaw:ItemGemSaw = null
    var itemRubySaw:ItemGemSaw = null
    var itemSapphireSaw:ItemGemSaw = null
    var itemPeridotSaw:ItemGemSaw = null
    var itemWoodSickle:ItemGemSickle = null
    var itemStoneSickle:ItemGemSickle = null
    var itemIronSickle:ItemGemSickle = null
    var itemGoldSickle:ItemGemSickle = null
    var itemRubySickle:ItemGemSickle = null
    var itemSapphireSickle:ItemGemSickle = null
    var itemPeridotSickle:ItemGemSickle = null
    var itemDiamondSickle:ItemGemSickle = null

    var tabExploration:CreativeTabs = new CreativeTabs("exploration")
    {
        override def getIconItemStack = new ItemStack(Block.grass)
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        ExplorationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        NetworkRegistry.instance.registerGuiHandler(this, ExplorationProxy)
        ExplorationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        ExplorationProxy.postinit()
    }
}