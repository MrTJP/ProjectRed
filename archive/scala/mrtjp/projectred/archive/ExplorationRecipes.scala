package mrtjp.projectred.archive

import mrtjp.projectred.ProjectRedExploration
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.OreDictionary
import java.lang.{Character => JChar}

import codechicken.lib.colour.EnumColour
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.exploration.{DecorativeStoneDefs, ItemBackpack}

object ExplorationRecipes
{
    var dumper:RecipeDumper = _
    def initRecipes()
    {
        dumper = new RecipeDumper("exploration")
        initEtcRecipes()
        initGemToolRecipes()
        initToolRecipes()
        initWorldRecipes()
        dumper.dump()
    }

    private def initGemToolRecipes()
    {
        /** Axes **/
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemRubyAxe), "gemRuby")
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemSapphireAxe), "gemSapphire")
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotAxe), "gemPeridot")

        /** Hoes **/
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemRubyHoe), "gemRuby")
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemSapphireHoe), "gemSapphire")
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemPeridotHoe), "gemPeridot")

        /** Pickaxe **/
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemRubyPickaxe), "gemRuby")
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemSapphirePickaxe), "gemSapphire")
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotPickaxe), "gemPeridot")

        /** Shovel **/
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemRubyShovel), "gemRuby")
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemSapphireShovel), "gemSapphire")
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemPeridotShovel), "gemPeridot")

        /** Sword **/
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemRubySword), "gemRuby")
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemSapphireSword), "gemSapphire")
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemPeridotSword), "gemPeridot")

        /** Saw **/
        addSawRecipe(new ItemStack(ProjectRedExploration.itemGoldSaw), "ingotGold")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemRubySaw), "gemRuby")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemSapphireSaw), "gemSapphire")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemPeridotSaw), "gemPeridot")

        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemWoodSickle), "plankWood")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemStoneSickle), new ItemStack(Items.FLINT))
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemIronSickle), "ingotIron")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemGoldSickle), "ingotGold")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemRubySickle), "gemRuby")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemSapphireSickle), "gemSapphire")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemPeridotSickle), "gemPeridot")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemDiamondSickle), "gemDiamond")

        /** Armor **/
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemRubyHelmet), "gemRuby")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemRubyChestplate), "gemRuby")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemRubyLeggings), "gemRuby")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemRubyBoots), "gemRuby")
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemSapphireHelmet), "gemSapphire")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemSapphireChestplate), "gemSapphire")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemSapphireLeggings), "gemSapphire")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemSapphireBoots), "gemSapphire")
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemPeridotHelmet), "gemPeridot")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemPeridotChestplate), "gemPeridot")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemPeridotLeggings), "gemPeridot")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemPeridotBoots), "gemPeridot")
    }

    private def addHelmetRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o, "mmm", "m m", 'm':JChar, m)).setJsonName(s"armor\\${m.substring(indexOfUpper(m)).toLowerCase()}_helmet")
    }

    private def addChestplateRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o, "m m", "mmm", "mmm", 'm':JChar, m)).setJsonName(s"armor\\${m.substring(indexOfUpper(m)).toLowerCase()}_chestplate")
    }

    private def addLeggingsRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o, "mmm", "m m", "m m", 'm':JChar, m)).setJsonName(s"armor\\${m.substring(indexOfUpper(m)).toLowerCase()}_leggings")
    }

    private def addBootsRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o, "m m", "m m", 'm':JChar, m)).setJsonName(s"armor\\${m.substring(indexOfUpper(m)).toLowerCase()}_boots")
    }

    private def addAxeRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "mm", "ms", " s",
            'm':JChar, m,
            's':JChar, "stickWood")).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_axe")
    }

    private def addHoeRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "mm", " s", " s",
            'm':JChar, m,
            's':JChar, "stickWood")).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_hoe")
    }

    private def addPickaxeRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "mmm", " s ", " s ",
            'm':JChar, m,
            's':JChar, "stickWood")).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_pickaxe")
    }

    private def addShovelRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "m", "s", "s",
            'm':JChar, m,
            's':JChar, "stickWood")).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_shovel")
    }

    private def addSwordRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "m", "m", "s",
            'm':JChar, m,
            's':JChar, "stickWood")).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_sword")
    }

    private def addSawRecipe(o:ItemStack, m:String)
    {
        dumper.addRecipe(new ShapedOreRecipe(o,
            "srr", "sbb",
            's':JChar, "stickWood",
            'r':JChar, "rodStone",
            'b':JChar, m)).setJsonName(s"tools\\${m.substring(indexOfUpper(m)).toLowerCase()}_saw")
    }

    private def addSickleRecipe(o:ItemStack, m:AnyRef)
    {
        val s:String = m match {
            case stack:ItemStack => "stone"
            case str:String => str.substring(indexOfUpper(str))
            case _ => throw new IllegalArgumentException("Wot")
        }
        dumper.addRecipe(new ShapedOreRecipe(o,
            " m ", "  m", "sm ",
            's':JChar, "stickWood",
            'm':JChar, m)).setJsonName(s"tools\\${s.toLowerCase()}_sickle")
    }

    private def initEtcRecipes()
    {
        /** Wool Gin to string recipe **/
        dumper.addRecipe(new ItemStack(Items.STRING, 4),
            "gw",
            'g':JChar, new ItemStack(ProjectRedExploration.itemWoolGin, 1, OreDictionary.WILDCARD_VALUE),
            'w':JChar, Blocks.WOOL).setJsonName("misc\\wool_2_string")

        /** Item Barrel  **/
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.blockBarrel),
            "lwl", "i i", "lll",
            'l':JChar, "logWood",
            'w':JChar, "slabWood",
            'i':JChar, "ingotIron")).setJsonName("blocks\\barrel")
    }

    private def initToolRecipes()
    {
        /** Wool Gin **/
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemWoolGin),
            "sis", "sss", " s ",
            's':JChar, "stickWood",
            'i':JChar, PartDefs.IRONCOIL.makeStack)).setJsonName("tools\\wool_gin")

        /** Backpacks **/
        for (i <- 0 until 16)
        {
            dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemBackpack, 1, i),
                "ccc",
                if(i == 0) "c c" else "cdc",
                "ccc",
                'c':JChar, PartDefs.WOVENCLOTH.makeStack,
                'd':JChar, EnumColour.fromWoolMeta(i).getDyeOreName)).setJsonName(s"items\\backpack\\${EnumColour.fromWoolMeta(i).getName}")

            dumper.addRecipe(new ShapelessOreNBTCopyRecipe(new ItemStack(ProjectRedExploration.itemBackpack, 1, i),
                ItemBackpack.oreDictionaryVal, EnumColour.fromWoolMeta(i).getDyeOreName)
            ).setJsonName(s"items\\backpack\\${EnumColour.fromWoolMeta(i).getName}_re_colour")
        }

        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemAthame),
           "s", "w",
           's':JChar, "ingotSilver",
           'w':JChar, "stickWood")).setJsonName("tools\\athame")
    }

    private def initWorldRecipes()
    {
        /** Marble brick **/
        dumper.addRecipe(new ShapedOreRecipe(DecorativeStoneDefs.MARBLEBRICK.makeStack(4),
            "bb", "bb",
            'b':JChar, "blockMarble")).setJsonName("blocks\\marble_brick")

        /** Basalt brick **/
        dumper.addRecipe(DecorativeStoneDefs.BASALTBRICK.makeStack(4),
            "bb", "bb",
            'b':JChar, DecorativeStoneDefs.BASALT.makeStack).setJsonName("blocks\\basalt_brick")


        /** Storage blocks **/
        addStorageBlockRecipe("gemRuby", PartDefs.RUBY.makeStack(9), "blockRuby", DecorativeStoneDefs.RUBYBLOCK.makeStack)
        addStorageBlockRecipe("gemSapphire", PartDefs.SAPPHIRE.makeStack(9), "blockSapphire", DecorativeStoneDefs.SAPPHIREBLOCK.makeStack)
        addStorageBlockRecipe("gemPeridot", PartDefs.PERIDOT.makeStack(9), "blockPeridot", DecorativeStoneDefs.PERIDOTBLOCK.makeStack)
        addStorageBlockRecipe("ingotCopper", PartDefs.COPPERINGOT.makeStack(9), "blockCopper", DecorativeStoneDefs.COPPERBLOCK.makeStack)
        addStorageBlockRecipe("ingotTin", PartDefs.TININGOT.makeStack(9), "blockTin", DecorativeStoneDefs.TINBLOCK.makeStack)
        addStorageBlockRecipe("ingotSilver", PartDefs.SILVERINGOT.makeStack(9), "blockSilver", DecorativeStoneDefs.SILVERBLOCK.makeStack)
        addStorageBlockRecipe("dustElectrotine", PartDefs.ELECTROTINE.makeStack(9), "blockElectrotine", DecorativeStoneDefs.ELECTROTINEBLOCK.makeStack)

        for (i <- DecorativeStoneDefs.values.indices)
        {
            val s:DecorativeStoneDefs.StoneVal = DecorativeStoneDefs.values.apply(i)
            addWallRecipe(new ItemStack(ProjectRedExploration.blockDecorativeWall, 6, s.meta), s.makeStack).setJsonName(s"blocks\\wall\\${s.getName}_wall")
        }
    }

    private def addStorageBlockRecipe(itemOre:String, item:ItemStack, blockOre:String, block:ItemStack)
    {
        dumper.addRecipe(new ShapedOreRecipe(block, "xxx", "xxx", "xxx", 'x':JChar, itemOre)).setJsonName(s"blocks\\storage\\${blockOre.substring(indexOfUpper(blockOre))}_block")
        dumper.addRecipe(new ShapelessOreRecipe(item, blockOre)).setJsonName(s"blocks\\storage\\${blockOre.substring(indexOfUpper(blockOre))}_block_un")
    }

    private def addWallRecipe(o:ItemStack, m:ItemStack):Recipe =
    {
        dumper.addRecipe(o, "mmm", "mmm", 'm':JChar, m)
    }
    def indexOfUpper(str:String):Int = {
        var i = -1
        for (ch <- 'A' to 'Z') {
            val idx = str.indexOf(ch)
            if (i == -1  || (idx < i && idx != -1)) {
                i = idx
            }
        }
        i
    }
}
