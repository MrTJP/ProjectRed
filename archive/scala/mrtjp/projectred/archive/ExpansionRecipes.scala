package mrtjp.projectred.archive

import java.lang.{Character => JC}
import mrtjp.projectred.ProjectRedExpansion._
import com.google.gson.{JsonArray, JsonObject}
import mrtjp.projectred.core.PartDefs
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack

object ExpansionRecipes
{
    var dumper:RecipeDumper = _
    def initRecipes()
    {
        dumper = new RecipeDumper("expansion")
        initItemRecipes()
        initMachineRecipes()
        initDeviceRecipes()
        initMiscRecipes()
        dumper.dump
    }

    private def initItemRecipes()
    {
        //Battery
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemBattery),
            "ete","ece", "ete",
            'e':JC, PartDefs.ELECTROTINE.makeStack,
            't':JC, "ingotTin",
            'c':JC, "ingotCopper"
        )).setJsonName("items\\battery")

        //Electric Screwdriver
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemScrewdriver),
            "i  ", " s ", "  b",
            'i':JC, "ingotIron",
            's':JC, "gemSapphire",
            'b':JC, itemBattery
        )).setJsonName("items\\electric_screwdriver")

        //Jetpack
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemJetpack),
            "b b","bcb","eae",
            'b':JC, itemBattery,
            'c':JC, Items.DIAMOND_CHESTPLATE,
            'e':JC, "gemEmerald",
            'a':JC, new ItemStack(machine2, 1, 5)
        )).setJsonName("items\\jetpack")

        //Recipe Plan
        dumper.addRecipe(new ShapelessOreRecipe(new ItemStack(itemPlan), "dyeBlue", Items.PAPER)).setJsonName("items\\recipe_plan")
    }

    private def initMachineRecipes()
    {
        //Inductive Furnace
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 0),
            "bbb", "b b", "iei",
            'b':JC, Blocks.BRICK_BLOCK,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        )).setJsonName("machines\\inductive_furnace")

        //Electrotine generator
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 1),
            "bbb", "a a", "cec",
            'b':JC, Blocks.BRICK_BLOCK,
            'a':JC, new ItemStack(itemBattery),
            'c':JC, Blocks.CLAY,
            'e':JC, "ingotElectrotineAlloy"
        )).setJsonName("machines\\electrotine_generator")
    }

    private def initDeviceRecipes()
    {
        //Block Breaker
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 0),
            "sas", "sps", "srs",
            's':JC, "cobblestone",
            'a':JC, Items.IRON_PICKAXE,
            'p':JC, Blocks.PISTON,
            'r':JC, "dustRedstone"
        )).setJsonName("device\\block_breaker")

        //Item Importer
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 1),
            "www", "sps", "srs",
            'w':JC, "slabWood",
            's':JC, "cobblestone",
            'p':JC, Blocks.PISTON,
            'r':JC, "dustRedstone"
        )).setJsonName("device\\item_importer")

        //Block Placer
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 2),
            "ihi","cpc", "crc",
            'i':JC, "ingotIron",
            'h':JC, "chestWood",
            'c':JC, "cobblestone",
            'p':JC, Blocks.PISTON,
            'r':JC, "dustRedstone"
        )).setJsonName("device\\block_placer")

        //Filtered Importer
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 3),
            "tct", "gig", "sgs",
            't':JC, new ItemStack(Blocks.STONE_SLAB,1, 0),
            'c':JC, "chestWood",
            'g':JC, "ingotGold",
            'i':JC, new ItemStack(machine2, 1, 1),
            's':JC, "cobblestone"
        )).setJsonName("device\\filtered_importer")

        //Fire Starter
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 4),
            "nfn", "cpc", "crc",
            'n':JC, "netherrack",
            'f':JC, Items.FLINT_AND_STEEL,
            'c':JC, "cobblestone",
            'p':JC, new ItemStack(machine2, 1, 2),
            'r':JC, "dustRedstone"
        )).setJsonName("device\\fire_starter")

        //Battery Box
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 5),
            "bwb","bib","iei",
            'b':JC, new ItemStack(itemBattery),
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        )).setJsonName("device\\battery_box")

        //Solar Panel
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(itemSolar),
            "sss","iwi","wew",
            's':JC, PartDefs.ELECTROSILICON.makeStack,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy",
            'w':JC, "slabWood"
        )).setJsonName("device\\solar_panel")

        //Charging Bench
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 6),
            "scs","wbw","iei",
            's':JC, "stone",
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'w':JC, "plankWood",
            'b':JC, new ItemStack(itemBattery),
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        )).setJsonName("device\\charging_bench")

//        //Teleposer
//        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 7),
//            "odo","wbw","iei",
//            'o':JC, Blocks.OBSIDIAN,
//            'd':JC, Items.DIAMOND,
//            'w':JC, "plankWood",
//            'b':JC, Items.ENDER_PEARL,
//            'i':JC, "ingotIron",
//            'e':JC, "ingotElectrotineAlloy"
//        ))

//        //Frame Motor
//        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 8),
//            "wiw","cmc","iei",
//            'w':JC, "plankWood",
//            'i':JC, "ingotIron",
//            'c':JC, PartDefs.COPPERCOIL.makeStack,
//            'm':JC, PartDefs.MOTOR.makeStack,
//            'e':JC, "ingotElectrotineAlloy"
//        ))
//
//        //Frame Linear Actuator
//        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 9),
//            "wiw","cic","cec",
//            'w':JC, "plankWood",
//            'i':JC, "ingotIron",
//            'c':JC, PartDefs.COPPERCOIL.makeStack,
//            'e':JC, "ingotElectrotineAlloy"
//        ))

        //Project Bench
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 10),
            "sss","wbw","wcw",
            's':JC, "stone",
            'w':JC, "plankWood",
            'b':JC, "workbench",
            'c':JC, "chestWood"
        )).setJsonName("device\\project_bench")

        //Auto Crafting Bench
        dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 11),
            "sbs","ici","wew",
            's':JC, "stone",
            'b':JC, "workbench",
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, "chestWood",
            'e':JC, "ingotElectrotineAlloy"
        )).setJsonName("device\\auto_crafting_bench")

        //Diamond Block Breaker
        import Conversions._
            dumper.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 12),
                "sas", "sps", "srs",
                's':JC, Blocks.COBBLESTONE,
                'a':JC, Items.DIAMOND_PICKAXE,
                'p':JC, Blocks.PISTON,
                'r':JC, Items.REDSTONE
            )).setJsonName("device\\diamond_block_breaker")//
                .setConditions(new JsonArray()//
                    .add_(new JsonObject()//
                        .add("type", "projectred-core:config")//
                        .add("category", "Machine Settings")//
                        .add("key", "Enable the Diamond Block Breaker")//
                    )//
                )//
    }

    private def initMiscRecipes()
    {
    }
}
