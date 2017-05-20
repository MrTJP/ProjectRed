package mrtjp.projectred.core

import codechicken.lib.colour.EnumColour
import codechicken.microblock.{BlockMicroMaterial, EdgeMicroFactory, FaceMicroFactory, ItemMicroPart}
import codechicken.microblock.handler.MicroblockProxy
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.core.PartDefs._
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.oredict.{OreDictionary, ShapedOreRecipe, ShapelessOreRecipe}
import java.lang.{Character => JChar}

object CoreRecipes
{
    def initCoreRecipes()
    {
        initOreDict()
        initPartRecipes()
        initToolRecipes()
    }

    private def initOreDict()
    {
        for (i <- 0 until 16)
            OreDictionary.registerOre(PartDefs.oreDictDefinitionIllumar, PartDefs.ILLUMARS(i).makeStack)

        OreDictionary.registerOre("gemRuby", PartDefs.RUBY.makeStack)
        OreDictionary.registerOre("gemSapphire", PartDefs.SAPPHIRE.makeStack)
        OreDictionary.registerOre("gemPeridot", PartDefs.PERIDOT.makeStack)
        OreDictionary.registerOre("ingotRedAlloy", PartDefs.REDINGOT.makeStack)
        OreDictionary.registerOre("ingotCopper", PartDefs.COPPERINGOT.makeStack)
        OreDictionary.registerOre("ingotTin", PartDefs.TININGOT.makeStack)
        OreDictionary.registerOre("ingotSilver", PartDefs.SILVERINGOT.makeStack)
        OreDictionary.registerOre("ingotElectrotineAlloy", PartDefs.ELECTROTINEINGOT.makeStack)
        OreDictionary.registerOre("dustElectrotine", PartDefs.ELECTROTINE.makeStack)
    }

    private def initToolRecipes()
    {
        /** Draw Plate **/
        GameRegistry.addRecipe(new ShapedNBTSensitiveRecipe(ProjectRedCore.itemDrawPlate,
            " i ", "idi", " i ",
            'i':JChar, ItemMicroPart.create(EdgeMicroFactory.getFactoryID, 2, BlockMicroMaterial.materialKey(Blocks.IRON_BLOCK)),
            'd':JChar, ItemMicroPart.create(FaceMicroFactory.getFactoryID, 2, BlockMicroMaterial.materialKey(Blocks.DIAMOND_BLOCK))
        ))

        /** Panel Reset recipe **/
        GameRegistry.addRecipe(new ShapelessNBTSensitiveRecipe(new ItemStack(Items.DIAMOND, 2),
            ItemMicroPart.create(FaceMicroFactory.getFactoryID, 2, BlockMicroMaterial.materialKey(Blocks.DIAMOND_BLOCK))
        ))

        /** Screwdriver **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemScrewdriver),
            "i  ", " ib", " bi",
            'i':JChar, "ingotIron",
            'b':JChar, "dyeBlue"
        ))

        /** Wire Debugger **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemMultimeter),
            "a a", "ber", "bgr",
            'a':JChar, "ingotRedAlloy",
            'b':JChar, "dyeBlack",
            'e':JChar, "dyeGreen",
            'r':JChar, "dyeRed",
            'g':JChar, "dustGlowstone"
        ))
    }

    private def initPartRecipes()
    {
        /** Circuit Plate **/
        GameRegistry.addSmelting(Blocks.STONE, PLATE.makeStack(2), 0f)

        /** Conductive Plate **/
        GameRegistry.addRecipe(new ShapedOreRecipe(CONDUCTIVEPLATE.makeStack,
            "r", "p",
            'r':JChar, "dustRedstone",
            'p':JChar, PLATE.makeStack
        ))

        /** Anode **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ANODE.makeStack(3),
            " r ", "rrr", "ppp",
            'r':JChar, "dustRedstone",
            'p':JChar, PLATE.makeStack
        ))

        /** Cathode **/
        GameRegistry.addRecipe(CATHODE.makeStack,
            "t", "p",
            't':JChar, Blocks.REDSTONE_TORCH,
            'p':JChar, PLATE.makeStack
        )

        /** Pointer **/
        GameRegistry.addRecipe(new ShapedOreRecipe(POINTER.makeStack,
            "b", "m", "c",
            'b':JChar, "stone",
            'm':JChar, Blocks.REDSTONE_TORCH,
            'c':JChar, PLATE.makeStack
        ))

        /** Silicon Chip **/
        GameRegistry.addRecipe(SILICONCHIP.makeStack,
            " s ", "ppp", 's':JChar,
            INFUSEDSILICON.makeStack,
            'p':JChar, PLATE.makeStack
        )

        /** Energized Silicon Chip **/
        GameRegistry.addRecipe(ENERGIZEDSILICONCHIP.makeStack,
            " e ", "ppp", 'e':JChar,
            ENERGIZEDSILICON.makeStack,
            'p':JChar, PLATE.makeStack
        )

        /** Platformed Plate **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PLATFORMEDPLATE.makeStack,
            " r ", "sps", "prp",
            'r':JChar, WIREDPLATE.makeStack,
            's':JChar, "stickWood",
            'p':JChar, PLATE.makeStack)
        )

        /** Silicon Boule **/
        GameRegistry.addSmelting(SANDYCOALCOMPOUND.makeStack, SILICONBOULE.makeStack, 0)

        /** Silicon **/
        GameRegistry.addRecipe(SILICON.makeStack(8),
            "s", "b",
            's':JChar, new ItemStack(MicroblockProxy.sawDiamond, 1, OreDictionary.WILDCARD_VALUE),
            'b':JChar, SILICONBOULE.makeStack
        )

        /** Infused Silicon **/
        GameRegistry.addSmelting(REDSILICONCOMPOUND.makeStack, INFUSEDSILICON.makeStack, 0)

        /** Energized Silicon **/
        GameRegistry.addSmelting(GLOWINGSILICONCOMPOUND.makeStack, ENERGIZEDSILICON.makeStack, 0)

        /** Motor **/
        GameRegistry.addRecipe(new ShapedOreRecipe(MOTOR.makeStack,
            " i ", "scs", "rcr",
            'i':JChar, "ingotIron",
            's':JChar, "stone", 'c':JChar, COPPERCOIL.makeStack,
            'r':JChar, "dustRedstone")
        )

        /** Copper Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(COPPERCOIL.makeStack,
            "cd",
            'c':JChar, "ingotCopper",
            'd':JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        )

        /** Iron Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(IRONCOIL.makeStack,
            "cd",
            'c':JChar, "ingotIron",
            'd':JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        )

        /** Gold Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GOLDCOIL.makeStack,
            "cd",
            'c':JChar, "ingotGold",
            'd':JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        )

        /** Red Alloy Ingot **/
        GameRegistry.addSmelting(REDIRONCOMPOUND.makeStack, REDINGOT.makeStack, 0)

        /** Illumar **/
        for (i <- 0 until 16) {
            val p:PartDefs.PartVal = PartDefs.ILLUMARS(i)
            GameRegistry.addRecipe(new ShapelessOreRecipe(p.makeStack, "dustGlowstone", "dustGlowstone",
                EnumColour.values.apply(i).getOreDictionaryName, EnumColour.values.apply(i).getOreDictionaryName))
        }

        /** Woven Cloth **/
        GameRegistry.addRecipe(new ShapedOreRecipe(WOVENCLOTH.makeStack,
            "sss", "sws", "sss",
            's':JChar, Items.STRING,
            'w':JChar, "stickWood")
        )

        /** Sail **/
        GameRegistry.addRecipe(SAIL.makeStack,
            "ss", "ss", "ss",
            's':JChar, WOVENCLOTH.makeStack
        )

        /** Red Iron Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(REDIRONCOMPOUND.makeStack,
            "rrr", "rir", "rrr",
            'r':JChar, "dustRedstone",
            'i':JChar, "ingotIron")
        )

        /** Sandy Coal Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(SANDYCOALCOMPOUND.makeStack,
            "sss", "scs", "sss",
            'c':JChar, "blockCoal",
            's':JChar, Blocks.SAND)
        )

        /** Red Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(REDSILICONCOMPOUND.makeStack,
            "rrr", "rsr", "rrr",
            'r':JChar, "dustRedstone",
            's':JChar, SILICON.makeStack)
        )

        /** Glowing Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GLOWINGSILICONCOMPOUND.makeStack,
            "ggg", "gsg", "ggg",
            'g':JChar, "dustGlowstone",
            's':JChar, SILICON.makeStack)
        )

        /** Electrotine Ingot **/
        GameRegistry.addSmelting(ELECTROTINEIRONCOMPOUND.makeStack, ELECTROTINEINGOT.makeStack, 0)

        /** Electrotine Iron Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ELECTROTINEIRONCOMPOUND.makeStack,
            "bbb", "bib", "bbb",
            'b':JChar, "dustElectrotine",
            'i':JChar, "ingotIron")
        )

        /** Electrotine Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ELECTROTINESILICONCOMPOUND.makeStack,
            "bbb", "bsb", "bbb",
            'b':JChar, "dustElectrotine",
            's':JChar, SILICON.makeStack)
        )

        /** Electrosilicon **/
        GameRegistry.addSmelting(ELECTROTINESILICONCOMPOUND.makeStack, ELECTROSILICON.makeStack, 0)
    }
}