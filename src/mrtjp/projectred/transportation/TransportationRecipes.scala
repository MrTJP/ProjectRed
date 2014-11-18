package mrtjp.projectred.transportation

import java.lang.{Character => JC}

import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.core.color.Colors
import mrtjp.projectred.ProjectRedTransportation
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.init.{Blocks, Items}
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraft.world.World
import net.minecraftforge.oredict.RecipeSorter.Category._
import net.minecraftforge.oredict.{RecipeSorter, ShapedOreRecipe}

object TransportationRecipes
{
    def initRecipes()
    {
        initPipeRecipes()
        initChipRecipes()
        initUpgradeRecipes()
        initMiscRecipes()
    }

    private def initPipeRecipes()
    {
        /** Item Transport pipe **/
        GameRegistry.addRecipe(PipeDefs.BASIC.makeStack(16),
            "sgs",
            'g':JC, "paneGlassColorless",
            's':JC, "stone"
        )

        /** Routed Junction pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDJUNCTION.makeStack(16),
            "RrG",
            "dgd",
            "GrR",
            'R':JC, PartDefs.REDILLUMAR.makeStack,
            'r':JC, PartDefs.INFUSEDSILICON.makeStack,
            'G':JC, PartDefs.GREENILLUMAR.makeStack,
            'd':JC, "gemDiamond",
            'g':JC, "paneGlassColorless"
        )

        /** Routed Interface Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDINTERFACE.makeStack,
            "rgr",
            "gjg",
            "rgr",
            'g':JC, "nuggetGold",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack,
            'r':JC, "dustRedstone"
        )

        /** Routed Crafting Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDCRAFTING.makeStack,
            "rgr", "rjr", "rgr",
            'r':JC, "dustRedstone",
            'g':JC, "dustGlowstone",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )

        /** Routed Request Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDREQUEST.makeStack,
            "rdr", "rjr", "rdr",
            'r':JC, "dustRedstone",
            'd':JC, "gemDiamond",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )

        /** Routed Extension Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDEXTENSION.makeStack,
            " r ", "rjr", " r ",
            'r':JC, "dustRedstone",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )

        /** Routed Firewall Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDFIREWALL.makeStack,
            "bcb", "cjc", "bcb",
            'b':JC, Blocks.nether_brick,
            'c':JC, Items.magma_cream,
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )
    }

    private def initChipRecipes()
    {
        RecipeSorter.register("projectred:chipreset", classOf[ChipResetRecipe], SHAPED, "after:forge:shaped")

        /** Chip reset **/
        GameRegistry.addRecipe(new ChipResetRecipe)

        /** Null chip **/
        GameRegistry.addRecipe(PartDefs.NULLROUTINGCHIP.makeStack,
            "gpp", "grr", "g  ",
            'g':JC, "nuggetGold",
            'p':JC, Items.paper,
            'r':JC, "dustRedstone"
        )

        /** Item Responder **/
        addChipRecipe(RoutingChipDefs.ITEMRESPONDER.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.ORANGEILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack)

        /** Dynamic Item Responder **/
        addChipRecipe(RoutingChipDefs.DYNAMICITEMRESPONDER.makeStack,
            "ingotIron", "dustRedstone",
            PartDefs.CYANILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack)

        /** Item Overflow Responder **/
        addChipRecipe(RoutingChipDefs.ITEMOVERFLOWRESPONDER.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.GREENILLUMAR.makeStack,
            PartDefs.GREENILLUMAR.makeStack)

        /** Item Terminator **/
        addChipRecipe(RoutingChipDefs.ITEMTERMINATOR.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.PURPLEILLUMAR.makeStack,
            PartDefs.GREYILLUMAR.makeStack)

        /** Item Extractor **/
        addChipRecipe(RoutingChipDefs.ITEMEXTRACTOR.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.CYANILLUMAR.makeStack,
            PartDefs.CYANILLUMAR.makeStack)

        /** Item Broadcaster **/
        addChipRecipe(RoutingChipDefs.ITEMBROADCASTER.makeStack,
            "ingotGold", "dustRedstone", "dustRedstone",
            PartDefs.MAGENTAILLUMAR.makeStack,
            PartDefs.MAGENTAILLUMAR.makeStack)

        /** Item Stock Keeper **/
        addChipRecipe(RoutingChipDefs.ITEMSTOCKKEEPER.makeStack,
            "gemDiamond", "dustRedstone", "dustRedstone",
            PartDefs.BLUEILLUMAR.makeStack,
            PartDefs.BLUEILLUMAR.makeStack)

        /** Item Crafting **/
        addChipRecipe(RoutingChipDefs.ITEMCRAFTING.makeStack,
            "dustGlowstone", "dustRedstone", "dustGlowstone",
            PartDefs.LIMEILLUMAR.makeStack,
            PartDefs.LIMEILLUMAR.makeStack)

        def addChipRecipe(result:ItemStack, bus:AnyRef, material2:AnyRef, material1:AnyRef, dyeLeft:AnyRef, dyeRight:AnyRef)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(result,
                "dMD", "bcm", "dMD",
                'd':JC, dyeLeft,
                'M':JC, material2,
                'D':JC, dyeRight,
                'b':JC, bus,
                'c':JC, PartDefs.NULLROUTINGCHIP.makeStack,
                'm':JC, material1))
        }
    }

    private def initUpgradeRecipes()
    {
        /** Router Utility **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedTransportation.itemRouterUtility),
            "  r", "iei", "iii",
            'r':JC, "dustRedstone",
            'i':JC, "ingotIron",
            'e':JC, "gemEmerald")

        /** Null Upgrade **/
        GameRegistry.addRecipe(PartDefs.NULLUPGRADECHIP.makeStack,
            "prp", "rrr", "prp",
            'p':JC, Items.paper, 'r':JC, "dustRedstone")

        /** LX **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_LX.makeStack,
            "rrr", " ng", "r r",
            'r':JC, "dustRedstone",
            'n':JC, PartDefs.NULLUPGRADECHIP.makeStack,
            'g':JC, "nuggetGold")

        /** RX **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_RX.makeStack,
            "r r", "gn ", "rrr",
            'r':JC, "dustRedstone",
            'n':JC, PartDefs.NULLUPGRADECHIP.makeStack,
            'g':JC, "nuggetGold")

        /** LY **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_LY.makeStack,
            "l l", " n ", " l ",
            'l':JC, Colors.BLUE.getDye,
            'n':JC, PartDefs.CHIPUPGRADE_LX.makeStack)

        /** RY **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_RY.makeStack,
            "l l", " n ", " l ",
            'l':JC, Colors.BLUE.getDye,
            'n':JC, PartDefs.CHIPUPGRADE_RX.makeStack)

        /** LZ **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_LZ.makeStack,
            "r r", " n ", "rer",
            'r':JC, "dustRedstone",
            'n':JC, PartDefs.CHIPUPGRADE_LY.makeStack,
            'e':JC, "gemEmerald")

        /** RZ **/
        GameRegistry.addRecipe(PartDefs.CHIPUPGRADE_RZ.makeStack,
            "r r", " n ", "rer",
            'r':JC, "dustRedstone",
            'n':JC, PartDefs.CHIPUPGRADE_RY.makeStack,
            'e':JC, "gemEmerald")
    }

    def initMiscRecipes()
    {
    }
}

class ChipResetRecipe extends IRecipe
{
    def matches(inv:InventoryCrafting, world:World) = getCraftingResult(inv) != null

    def getCraftingResult(inv:InventoryCrafting):ItemStack =
    {
        val cdef = getType(inv)
        if (cdef != null) if (isTypeExclusive(cdef, inv)) return cdef.makeStack(countUnits(inv))
        null
    }

    def getType(inv:InventoryCrafting):ChipVal =
    {
        for (i <- 0 until inv.getSizeInventory)
        {
            val cdef = RoutingChipDefs.getForStack(inv.getStackInSlot(i))
            if (cdef != null) return cdef
        }
        null
    }

    def isTypeExclusive(cdef:ChipVal, inv:InventoryCrafting):Boolean =
    {
        for (i <- 0 until inv.getSizeInventory)
        {
            val stack = inv.getStackInSlot(i)
            if (stack != null && !stack.getItem.isInstanceOf[ItemRoutingChip]) return false
            val type2 = RoutingChipDefs.getForStack(stack)
            if (type2 != null && !(type2 == cdef)) return false
        }
        true
    }

    def countUnits(inv:InventoryCrafting):Int =
    {
        var count = 0
        for (i <- 0 until inv.getSizeInventory)
            if (inv.getStackInSlot(i) != null) count += 1
        count
    }

    def getRecipeSize = 2
    def getRecipeOutput = null
}