package mrtjp.projectred.transportation

import java.lang.{Character => JC}

import mrtjp.projectred.ProjectRedTransportation
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.init.{Blocks, Items}
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraft.world.World
import net.minecraftforge.common.ForgeHooks
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.oredict.RecipeSorter.Category._
import net.minecraftforge.oredict.{RecipeSorter, ShapedOreRecipe}

object TransportationRecipes
{
    def initRecipes()
    {
        initNetPipeRecipes()
        initPressureTubeRecipes()
        initChipRecipes()
        initUpgradeRecipes()
        initMiscRecipes()
    }

    private def initNetPipeRecipes()
    {
        /** Item Transport pipe **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.BASIC.makeStack(16),
            "sgs",
            'g':JC, "paneGlassColorless",
            's':JC, "stone"
        ))

        /** Routed Junction pipe **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDJUNCTION.makeStack(16),
            "RrG",
            "dgd",
            "GrR",
            'R':JC, PartDefs.REDILLUMAR.makeStack,
            'r':JC, PartDefs.INFUSEDSILICON.makeStack,
            'G':JC, PartDefs.GREENILLUMAR.makeStack,
            'd':JC, "gemDiamond",
            'g':JC, "paneGlassColorless"
        ))

        /** Routed Interface Pipe **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDINTERFACE.makeStack,
            "rgr",
            "gjg",
            "rgr",
            'g':JC, "nuggetGold",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack,
            'r':JC, "dustRedstone"
        ))

        /** Routed Request Pipe **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDREQUEST.makeStack,
            "rdr", "rjr", "rdr",
            'r':JC, "dustRedstone",
            'd':JC, "gemDiamond",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        ))

        /** Routed Firewall Pipe **/
        GameRegistry.addRecipe(PipeDefs.ROUTEDFIREWALL.makeStack,
            "bcb", "cjc", "bcb",
            'b':JC, Blocks.NETHER_BRICK,
            'c':JC, Items.MAGMA_CREAM,
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )

        /** Network Valve pipe **/
        GameRegistry.addRecipe(PipeDefs.NETWORKVALVE.makeStack,
            " g ", "gbg", " l ",
            'g':JC, Blocks.GLASS_PANE,
            'b':JC, PipeDefs.BASIC.makeStack,
            'l':JC, Blocks.LEVER
        )

        /** Network Latency pipe **/
        GameRegistry.addRecipe(PipeDefs.NETWORKLATENCY.makeStack,
            " c ", "cbc", " c ",
            'c':JC, Blocks.COBBLESTONE,
            'b':JC, PipeDefs.BASIC.makeStack
        )
    }

    private def initPressureTubeRecipes()
    {
        /** Pressure Tube **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.PRESSURETUBE.makeStack(8),
            "gpg", "p p", "gpg",
            'g':JC, "nuggetGold",
            'p':JC, Blocks.GLASS_PANE
        ))

        /** Pressure Resistance Tube **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PipeDefs.RESISTANCETUBE.makeStack,
            "i", "t",
            'i':JC, "ingotIron",
            't':JC, PipeDefs.PRESSURETUBE.makeStack
        ))
    }

    private def initChipRecipes()
    {
        RecipeSorter.register("projectred:chipreset", classOf[ChipResetRecipe], SHAPED, "after:forge:shaped")

        /** Chip reset **/
        GameRegistry.addRecipe(new ChipResetRecipe)

        /** Null chip **/
        GameRegistry.addRecipe(new ShapedOreRecipe(PartDefs.NULLROUTINGCHIP.makeStack,
            "gpp", "grr", "g  ",
            'g':JC, "nuggetGold",
            'p':JC, Items.PAPER,
            'r':JC, "dustRedstone"
        ))

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

        /** Item Crafting Extension **/
        addChipRecipe(RoutingChipDefs.ITEMEXTENSION.makeStack,
            "dustGlowstone", "dustRedstone", "dustRedstone",
            PartDefs.REDILLUMAR.makeStack,
            PartDefs.REDILLUMAR.makeStack)


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
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedTransportation.itemRouterUtility),
            "  r", "iei", "iii",
            'r':JC, "dustRedstone",
            'i':JC, "ingotIron",
            'e':JC, "dyeGreen"))
    }

    def initMiscRecipes()
    {
    }
}

class ChipResetRecipe extends IRecipe
{
    override def getRemainingItems(inv:InventoryCrafting) = ForgeHooks.defaultRecipeGetRemainingItems(inv)

    def matches(inv:InventoryCrafting, world:World) = !getCraftingResult(inv).isEmpty

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
            if (!stack.isEmpty && !stack.getItem.isInstanceOf[ItemRoutingChip]) return false
            val type2 = RoutingChipDefs.getForStack(stack)
            if (type2 != null && !(type2 == cdef)) return false
        }
        true
    }

    def countUnits(inv:InventoryCrafting):Int =
    {
        var count = 0
        for (i <- 0 until inv.getSizeInventory)
            if (!inv.getStackInSlot(i).isEmpty) count += 1
        count
    }

    def getRecipeSize = 2
    def getRecipeOutput = ItemStack.EMPTY
}
