package mrtjp.projectred.expansion

import java.lang.{Character => JC}

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry
import cpw.mods.fml.common.Loader
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExpansion._
import mrtjp.projectred.core.{Configurator, IProxy}
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.ShapedOreRecipe

class ExpansionProxy_server extends IProxy
{
    var loadPowerTest = false

    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        if (Loader.isModLoaded("ProjRed|Transmission") && Configurator.version.contains("@"))
        {
            loadPowerTest = true
            import mrtjp.projectred.transmission._
            MultiPartRegistry.registerParts((name, _) => name match
            {
                case "pr_100v" => new PowerWire100v
                case "pr_f100v" => new FramedPowerWire100v
            })
        }

//        //Machine1 (machines)
//        ProjectRedExpansion.machine1 = new BlockMachine("projectred.expansion.machine1")
//        ProjectRedExpansion.machine1.addTile(classOf[TileFurnace], 0)

        //Machine2 (devices)
        machine2 = new BlockMachine("projectred.expansion.machine2")
        machine2.addTile(classOf[TileBlockBreaker], 0)
        machine2.addTile(classOf[TileItemImporter], 1)
        machine2.addTile(classOf[TileBlockPlacer], 2)
        machine2.addTile(classOf[TileFilteredImporter], 3)

        ExpansionRecipes.initRecipes()
    }

    def postinit()
    {
        // In dev mode, this module may load before transmission, therefore this must go in postInit
        if (loadPowerTest)
        {
            import mrtjp.projectred.transmission._
            ItemPartWire.additionalWires :+= WireDef.POWER_100v.makeStack
            ItemPartFramedWire.additionalWires :+= WireDef.POWER_100v.makeFramedStack
        }
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    val blockPlacerGui = 20
    val filteredImporterGui = 21

    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        PacketCustom.assignHandler(ExpansionCPH.channel, ExpansionCPH)
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()
        TileRenderRegistry.setRenderer(machine2, 0, RenderBlockBreaker)
        TileRenderRegistry.setRenderer(machine2, 1, RenderItemImporter)
        TileRenderRegistry.setRenderer(machine2, 2, RenderBlockPlacer)
        TileRenderRegistry.setRenderer(machine2, 3, RenderFilteredImporter)

        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
        GuiHandler.register(GuiFilteredImporter, filteredImporterGui)
    }
}

object ExpansionProxy extends ExpansionProxy_client

object ExpansionRecipes
{
    def initRecipes()
    {
        //FurnaceRecipeLib.init()
        initMachineRecipes()
        initMiscRecipes()
    }

    private def initMiscRecipes()
    {
    }

    private def initMachineRecipes()
    {
        //Block Breaker
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 0),
            "sas", "sps", "srs",
            's':JC, Blocks.cobblestone,
            'a':JC, Items.iron_pickaxe,
            'p':JC, Blocks.piston,
            'r':JC, Items.redstone
        ))

        //Item Importer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 1),
            "www", "sps", "srs",
            'w':JC, "slabWood",
            's':JC, Blocks.cobblestone,
            'p':JC, Blocks.piston,
            'r':JC, Items.redstone
        ))

        //Block Placer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 2),
            "ihi","cpc", "crc",
            'i':JC, "ingotIron",
            'h':JC, Blocks.chest,
            'c':JC, Blocks.cobblestone,
            'p':JC, Blocks.piston,
            'r':JC, Items.redstone
        ))

        //Filtered Importer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 3),
            "tct", "gig", "sgs",
            't':JC, new ItemStack(Blocks.stone_slab,1, 0),
            'c':JC, Blocks.chest,
            'g':JC, "ingotGold",
            'i':JC, new ItemStack(machine2, 1, 1),
            's':JC, Blocks.cobblestone
        ))
    }
}