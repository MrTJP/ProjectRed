package mrtjp.projectred.archive

import java.lang.{Character => JChar}

import codechicken.lib.colour.EnumColour
import codechicken.microblock.handler.MicroblockProxy
import com.google.gson.JsonObject
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.core.PartDefs._
import Conversions._
import net.minecraft.block.Block
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.OreDictionary

object CoreRecipes {

    var dumper: RecipeDumper = _

    def initCoreRecipes() {
        dumper = new RecipeDumper("core")
        initPartRecipes()
        initToolRecipes()
        dumper.dump
    }

    private def initToolRecipes() {
        def p(fac: String, size: Int, block: Block): JsonObject = {
            new JsonObject().add("type", "microblockcbe:micro_material").add("factory", fac).add("size", size).add_("material", new JsonObject().add("block", block.getRegistryName.toString))
        }

        /** Draw Plate **/
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemDrawPlate),
            " i ", "idi", " i ",
            'i': JChar, p("edge", 2, Blocks.IRON_BLOCK),
            'd': JChar, p("face", 2, Blocks.DIAMOND_BLOCK)
        )).setJsonName("tools\\draw_plate")

        /** Panel Reset recipe **/
        dumper.addRecipe(new ShapelessOreRecipe(new ItemStack(Items.DIAMOND, 2),
            p("face", 2, Blocks.DIAMOND_BLOCK)
        )).setJsonName("misc\\panel_2_diamonds")

        /** Screwdriver **/
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemScrewdriver),
            "i  ", " ib", " bi",
            'i': JChar, "ingotIron",
            'b': JChar, "dyeBlue"
        )).setJsonName("tools\\screwdriver")

        /** Wire Debugger **/
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemMultimeter),
            "a a", "ber", "bgr",
            'a': JChar, "ingotRedAlloy",
            'b': JChar, "dyeBlack",
            'e': JChar, "dyeGreen",
            'r': JChar, "dyeRed",
            'g': JChar, "dustGlowstone"
        )).setJsonName("tools\\multimeter")
    }

    private def initPartRecipes() {
        /** Conductive Plate **/
        dumper.addRecipe(new ShapedOreRecipe(CONDUCTIVEPLATE.makeStack,
            "r", "p",
            'r': JChar, "dustRedstone",
            'p': JChar, PLATE.makeStack
        )).setJsonName("parts\\conductive_plate")

        /** Anode **/
        dumper.addRecipe(new ShapedOreRecipe(ANODE.makeStack(3),
            " r ", "rrr", "ppp",
            'r': JChar, "dustRedstone",
            'p': JChar, PLATE.makeStack
        )).setJsonName("parts\\anode")

        /** Cathode **/
        dumper.addRecipe(CATHODE.makeStack,
            "t", "p",
            't': JChar, Blocks.REDSTONE_TORCH,
            'p': JChar, PLATE.makeStack
        ).setJsonName("parts\\cathode")

        /** Pointer **/
        dumper.addRecipe(new ShapedOreRecipe(POINTER.makeStack,
            "b", "m", "c",
            'b': JChar, "stone",
            'm': JChar, Blocks.REDSTONE_TORCH,
            'c': JChar, PLATE.makeStack
        )).setJsonName("parts\\pointer")

        /** Silicon Chip **/
        dumper.addRecipe(SILICONCHIP.makeStack,
            " s ", "ppp", 's': JChar,
            INFUSEDSILICON.makeStack,
            'p': JChar, PLATE.makeStack
        ).setJsonName("parts\\silicon_chip")

        /** Energized Silicon Chip **/
        dumper.addRecipe(ENERGIZEDSILICONCHIP.makeStack,
            " e ", "ppp", 'e': JChar,
            ENERGIZEDSILICON.makeStack,
            'p': JChar, PLATE.makeStack
        ).setJsonName("parts\\energized_silicon_chip")

        /** Platformed Plate **/
        dumper.addRecipe(new ShapedOreRecipe(PLATFORMEDPLATE.makeStack,
            " r ", "sps", "prp",
            'r': JChar, WIREDPLATE.makeStack,
            's': JChar, "stickWood",
            'p': JChar, PLATE.makeStack)
        ).setJsonName("parts\\platformed_plate")


        /** Silicon **/
        dumper.addRecipe(SILICON.makeStack(8),
            "s", "b",
            's': JChar, new ItemStack(MicroblockProxy.sawDiamond, 1, OreDictionary.WILDCARD_VALUE),
            'b': JChar, SILICONBOULE.makeStack
        ).setJsonName("resource\\silicon")


        /** Motor **/
        dumper.addRecipe(new ShapedOreRecipe(MOTOR.makeStack,
            " i ", "scs", "rcr",
            'i': JChar, "ingotIron",
            's': JChar, "stone", 'c': JChar, COPPERCOIL.makeStack,
            'r': JChar, "dustRedstone")
        ).setJsonName("misc\\motor")

        /** Copper Coil **/
        dumper.addRecipe(new ShapedOreRecipe(COPPERCOIL.makeStack,
            "cd",
            'c': JChar, "ingotCopper",
            'd': JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        ).setJsonName("misc\\copper_coil")

        /** Iron Coil **/
        dumper.addRecipe(new ShapedOreRecipe(IRONCOIL.makeStack,
            "cd",
            'c': JChar, "ingotIron",
            'd': JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        ).setJsonName("misc\\iron_coil")

        /** Gold Coil **/
        dumper.addRecipe(new ShapedOreRecipe(GOLDCOIL.makeStack,
            "cd",
            'c': JChar, "ingotGold",
            'd': JChar, new ItemStack(ProjectRedCore.itemDrawPlate, 1, OreDictionary.WILDCARD_VALUE))
        ).setJsonName("misc\\gold_coil")


        //TODO
        /** Illumar **/
        for (i <- 0 until 16) {
            val p: PartDefs.PartVal = PartDefs.ILLUMARS(i)
            dumper.addRecipe(new ShapelessOreRecipe(p.makeStack, "dustGlowstone", "dustGlowstone",
                EnumColour.values.apply(i).getDyeOreName, EnumColour.values.apply(i).getDyeOreName).setJsonName(s"lumar\\lumar_${EnumColour.values.apply(i).getName}"))
        }

        /** Woven Cloth **/
        dumper.addRecipe(new ShapedOreRecipe(WOVENCLOTH.makeStack,
            "sss", "sws", "sss",
            's': JChar, "string",
            'w': JChar, "stickWood")
        ).setJsonName("misc\\woven_cloth")

        /** Sail **/
        dumper.addRecipe(SAIL.makeStack,
            "ss", "ss", "ss",
            's': JChar, WOVENCLOTH.makeStack
        ).setJsonName("misc\\sail")

        /** Red Iron Compound **/
        dumper.addRecipe(new ShapedOreRecipe(REDIRONCOMPOUND.makeStack,
            "rrr", "rir", "rrr",
            'r': JChar, "dustRedstone",
            'i': JChar, "ingotIron")
        ).setJsonName("resource\\red_iron_compound")

        /** Sandy Coal Compound **/
        dumper.addRecipe(new ShapedOreRecipe(SANDYCOALCOMPOUND.makeStack,
            "sss", "scs", "sss",
            'c': JChar, "blockCoal",
            's': JChar, "sand")
        ).setJsonName("resource\\sandy_coal_compound")

        /** Red Silicon Compound **/
        dumper.addRecipe(new ShapedOreRecipe(REDSILICONCOMPOUND.makeStack,
            "rrr", "rsr", "rrr",
            'r': JChar, "dustRedstone",
            's': JChar, SILICON.makeStack)
        ).setJsonName("resource\\red_silicon_compound")

        /** Glowing Silicon Compound **/
        dumper.addRecipe(new ShapedOreRecipe(GLOWINGSILICONCOMPOUND.makeStack,
            "ggg", "gsg", "ggg",
            'g': JChar, "dustGlowstone",
            's': JChar, SILICON.makeStack)
        ).setJsonName("resource\\glowing_silicon_compound")

        /** Electrotine Iron Compound **/
        dumper.addRecipe(new ShapedOreRecipe(ELECTROTINEIRONCOMPOUND.makeStack,
            "bbb", "bib", "bbb",
            'b': JChar, "dustElectrotine",
            'i': JChar, "ingotIron")
        ).setJsonName("resource\\electrotine_iron_compound")

        /** Electrotine Silicon Compound **/
        dumper.addRecipe(new ShapedOreRecipe(ELECTROTINESILICONCOMPOUND.makeStack,
            "bbb", "bsb", "bbb",
            'b': JChar, "dustElectrotine",
            's': JChar, SILICON.makeStack)
        ).setJsonName("resource\\electrotine_silicon_compound")
    }
}
