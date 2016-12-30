package mrtjp.projectred

import mrtjp.core.world.SimpleGenHandler
import mrtjp.projectred.exploration._
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.init.Blocks
import net.minecraft.item.Item.ToolMaterial
import net.minecraft.item.ItemArmor.ArmorMaterial
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.Mod
import net.minecraftforge.fml.common.event.{FMLInitializationEvent, FMLPostInitializationEvent, FMLPreInitializationEvent}

@Mod(modid = "projectred-exploration", useMetadata = true, modLanguage = "scala")
object ProjectRedExploration
{
    /** Blocks **/
    var blockOres:BlockOre = _
    var blockDecorativeStone:BlockDecorativeStone = _
    var blockDecorativeWall:BlockDecorativeWall = _
    //var blockLily:BlockLily = _
    var blockBarrel:BlockBarrel = _

    /** Materials **/
    var toolMaterialRuby:ToolMaterial = _
    var toolMaterialSapphire:ToolMaterial = _
    var toolMaterialPeridot:ToolMaterial = _
    var armorMatrialRuby:ArmorMaterial = _
    var armorMatrialSapphire:ArmorMaterial = _
    var armorMatrialPeridot:ArmorMaterial = _

    /** Items **/
    var itemWoolGin:ItemWoolGin = _
    var itemBackpack:ItemBackpack = _
    var itemAthame:ItemAthame = _
    var itemRubyAxe:ItemGemAxe = _
    var itemSapphireAxe:ItemGemAxe = _
    var itemPeridotAxe:ItemGemAxe = _
    var itemRubyHoe:ItemGemHoe = _
    var itemSapphireHoe:ItemGemHoe = _
    var itemPeridotHoe:ItemGemHoe = _
    var itemRubyPickaxe:ItemGemPickaxe = _
    var itemSapphirePickaxe:ItemGemPickaxe = _
    var itemPeridotPickaxe:ItemGemPickaxe = _
    var itemRubyShovel:ItemGemShovel = _
    var itemSapphireShovel:ItemGemShovel = _
    var itemPeridotShovel:ItemGemShovel = _
    var itemRubySword:ItemGemSword = _
    var itemSapphireSword:ItemGemSword = _
    var itemPeridotSword:ItemGemSword = _
    var itemGoldSaw:ItemGemSaw = _
    var itemRubySaw:ItemGemSaw = _
    var itemSapphireSaw:ItemGemSaw = _
    var itemPeridotSaw:ItemGemSaw = _
    var itemWoodSickle:ItemGemSickle = _
    var itemStoneSickle:ItemGemSickle = _
    var itemIronSickle:ItemGemSickle = _
    var itemGoldSickle:ItemGemSickle = _
    var itemRubySickle:ItemGemSickle = _
    var itemSapphireSickle:ItemGemSickle = _
    var itemPeridotSickle:ItemGemSickle = _
    var itemDiamondSickle:ItemGemSickle = _
    //var itemLilySeed:ItemLilySeeds = _
    var itemRubyHelmet:ItemGemArmor = _
    var itemRubyChestplate:ItemGemArmor = _
    var itemRubyLeggings:ItemGemArmor = _
    var itemRubyBoots:ItemGemArmor = _
    var itemSapphireHelmet:ItemGemArmor = _
    var itemSapphireChestplate:ItemGemArmor = _
    var itemSapphireLeggings:ItemGemArmor = _
    var itemSapphireBoots:ItemGemArmor = _
    var itemPeridotHelmet:ItemGemArmor = _
    var itemPeridotChestplate:ItemGemArmor = _
    var itemPeridotLeggings:ItemGemArmor = _
    var itemPeridotBoots:ItemGemArmor = _

    var tabExploration:CreativeTabs = new CreativeTabs("projectred.exploration")
    {
        override def getIconItemStack = new ItemStack(Blocks.GRASS)
        override def getTabIconItem = getIconItemStack.getItem
    }

    @Mod.EventHandler
    def preInit(event:FMLPreInitializationEvent)
    {
        SimpleGenHandler.init()
        ExplorationProxy.versionCheck()
        ExplorationProxy.preinit()
    }

    @Mod.EventHandler
    def init(event:FMLInitializationEvent)
    {
        ExplorationProxy.init()
    }

    @Mod.EventHandler
    def postInit(event:FMLPostInitializationEvent)
    {
        ExplorationProxy.postinit()
    }
}
