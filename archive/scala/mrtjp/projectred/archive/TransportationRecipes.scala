package mrtjp.projectred.archive

import java.lang.{Character => JC}

import mrtjp.projectred.ProjectRedTransportation
import mrtjp.projectred.core.PartDefs
import mrtjp.projectred.transportation.{PipeDefs, RoutingChipDefs}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack

object TransportationRecipes
{
    var dumper:RecipeDumper = _
    def initRecipes()
    {
        dumper = new RecipeDumper("transportation")
        initNetPipeRecipes()
        initPressureTubeRecipes()
        initChipRecipes()
        initUpgradeRecipes()
        dumper.dump()
    }

    private def initNetPipeRecipes()
    {
        /** Item Transport pipe **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.BASIC.makeStack(16),
            "sgs",
            'g':JC, "paneGlassColorless",
            's':JC, "stone"
        )).setJsonName("pipe\\item_transport_pipe")

        /** Routed Junction pipe **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDJUNCTION.makeStack(16),
            "RrG",
            "dgd",
            "GrR",
            'R':JC, PartDefs.REDILLUMAR.makeStack,
            'r':JC, PartDefs.INFUSEDSILICON.makeStack,
            'G':JC, PartDefs.GREENILLUMAR.makeStack,
            'd':JC, "gemDiamond",
            'g':JC, "paneGlassColorless"
        )).setJsonName("pipe\\routed_junction_pipe")

        /** Routed Interface Pipe **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDINTERFACE.makeStack,
            "rgr",
            "gjg",
            "rgr",
            'g':JC, "nuggetGold",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack,
            'r':JC, "dustRedstone"
        )).setJsonName("pipe\\routed_interface_pipe")

        /** Routed Request Pipe **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.ROUTEDREQUEST.makeStack,
            "rdr", "rjr", "rdr",
            'r':JC, "dustRedstone",
            'd':JC, "gemDiamond",
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        )).setJsonName("pipe\\routed_request_pipe")

        /** Routed Firewall Pipe **/
        dumper.addRecipe(PipeDefs.ROUTEDFIREWALL.makeStack,
            "bcb", "cjc", "bcb",
            'b':JC, Blocks.NETHER_BRICK,
            'c':JC, Items.MAGMA_CREAM,
            'j':JC, PipeDefs.ROUTEDJUNCTION.makeStack
        ).setJsonName("pipe\\routed_firewall_pipe")

        /** Network Valve pipe **/
        dumper.addRecipe(PipeDefs.NETWORKVALVE.makeStack,
            " g ", "gbg", " l ",
            'g':JC, Blocks.GLASS_PANE,
            'b':JC, PipeDefs.BASIC.makeStack,
            'l':JC, Blocks.LEVER
        ).setJsonName("pipe\\network_valve_pipe")

        /** Network Latency pipe **/
        dumper.addRecipe(PipeDefs.NETWORKLATENCY.makeStack,
            " c ", "cbc", " c ",
            'c':JC, Blocks.COBBLESTONE,
            'b':JC, PipeDefs.BASIC.makeStack
        ).setJsonName("pipe\\network_latency_pipe")
    }

    private def initPressureTubeRecipes()
    {
        /** Pressure Tube **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.PRESSURETUBE.makeStack(8),
            "gpg", "p p", "gpg",
            'g':JC, "nuggetGold",
            'p':JC, Blocks.GLASS_PANE
        )).setJsonName("tube\\pressure_tube")

        /** Pressure Resistance Tube **/
        dumper.addRecipe(new ShapedOreRecipe(PipeDefs.RESISTANCETUBE.makeStack,
            "i", "t",
            'i':JC, "ingotIron",
            't':JC, PipeDefs.PRESSURETUBE.makeStack
        )).setJsonName("tube\\pressure_resistance_tube")
    }

    private def initChipRecipes()
    {
        /** Null chip **/
        dumper.addRecipe(new ShapedOreRecipe(PartDefs.NULLROUTINGCHIP.makeStack,
            "gpp", "grr", "g  ",
            'g':JC, "nuggetGold",
            'p':JC, Items.PAPER,
            'r':JC, "dustRedstone"
        )).setJsonName("chip\\null_chip")

        /** Item Responder **/
        addChipRecipe(RoutingChipDefs.ITEMRESPONDER.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.ORANGEILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack
        ).setJsonName("chip\\item_responder")

        /** Dynamic Item Responder **/
        addChipRecipe(RoutingChipDefs.DYNAMICITEMRESPONDER.makeStack,
            "ingotIron", "dustRedstone",
            PartDefs.CYANILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack,
            PartDefs.ORANGEILLUMAR.makeStack
        ).setJsonName("chip\\dynamic_item_responder")

        /** Item Overflow Responder **/
        addChipRecipe(RoutingChipDefs.ITEMOVERFLOWRESPONDER.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.GREENILLUMAR.makeStack,
            PartDefs.GREENILLUMAR.makeStack
        ).setJsonName("chip\\item_overflow_responder")

        /** Item Terminator **/
        addChipRecipe(RoutingChipDefs.ITEMTERMINATOR.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.PURPLEILLUMAR.makeStack,
            PartDefs.GREYILLUMAR.makeStack
        ).setJsonName("chip\\item_terminator")

        /** Item Extractor **/
        addChipRecipe(RoutingChipDefs.ITEMEXTRACTOR.makeStack,
            "ingotIron", "dustRedstone", "dustRedstone",
            PartDefs.CYANILLUMAR.makeStack,
            PartDefs.CYANILLUMAR.makeStack
        ).setJsonName("chip\\item_extractor")

        /** Item Broadcaster **/
        addChipRecipe(RoutingChipDefs.ITEMBROADCASTER.makeStack,
            "ingotGold", "dustRedstone", "dustRedstone",
            PartDefs.MAGENTAILLUMAR.makeStack,
            PartDefs.MAGENTAILLUMAR.makeStack
        ).setJsonName("chip\\item_broadcaster")

        /** Item Stock Keeper **/
        addChipRecipe(RoutingChipDefs.ITEMSTOCKKEEPER.makeStack,
            "gemDiamond", "dustRedstone", "dustRedstone",
            PartDefs.BLUEILLUMAR.makeStack,
            PartDefs.BLUEILLUMAR.makeStack
        ).setJsonName("chip\\item_stock_keeper")

        /** Item Crafting **/
        addChipRecipe(RoutingChipDefs.ITEMCRAFTING.makeStack,
            "dustGlowstone", "dustRedstone", "dustGlowstone",
            PartDefs.LIMEILLUMAR.makeStack,
            PartDefs.LIMEILLUMAR.makeStack
        ).setJsonName("chip\\item_crafting")

        /** Item Crafting Extension **/
        addChipRecipe(RoutingChipDefs.ITEMEXTENSION.makeStack,
            "dustGlowstone", "dustRedstone", "dustRedstone",
            PartDefs.REDILLUMAR.makeStack,
            PartDefs.REDILLUMAR.makeStack
        ).setJsonName("chip\\item_crafting_extension")


        def addChipRecipe(result:ItemStack, bus:AnyRef, material2:AnyRef, material1:AnyRef, dyeLeft:AnyRef, dyeRight:AnyRef):Recipe =
        {
            dumper.addRecipe(new ShapedOreRecipe(result,
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
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedTransportation.itemRouterUtility),
            "  r", "iei", "iii",
            'r':JC, "dustRedstone",
            'i':JC, "ingotIron",
            'e':JC, "dyeGreen")
        ).setJsonName("misc\\router_utility")
    }
}
