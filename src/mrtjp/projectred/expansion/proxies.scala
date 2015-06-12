package mrtjp.projectred.expansion

import java.lang.{Character => JC}

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.multipart.{TMultiPart, MultiPartRegistry}
import codechicken.multipart.MultiPartRegistry.IPartFactory2
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.color.Colors
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExpansion._
import mrtjp.projectred.core.{PartDefs, IProxy}
import mrtjp.projectred.transmission.WireDef
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.oredict.ShapedOreRecipe

class ExpansionProxy_server extends IProxy with IPartFactory2
{
    var loadPowerTest = false

    def preinit()
    {
        PacketCustom.assignHandler(ExpansionSPH.channel, ExpansionSPH)
    }

    def init()
    {
        MultiPartRegistry.registerParts(this, Array("pr_solar"))

        //Parts
        itemSolar = new ItemSolarPanel

        //Items
        emptybattery = new ItemBatteryEmpty
        battery = new ItemBattery

        //Machine1 (machines)
        machine1 = new BlockMachine("projectred.expansion.machine1")
        machine1.addTile(classOf[TileInductiveFurnace], 0)
        machine1.addTile(classOf[TileElectrotineGenerator], 1)

        //Machine2 (devices)
        machine2 = new BlockMachine("projectred.expansion.machine2")
        machine2.addTile(classOf[TileBlockBreaker], 0)
        machine2.addTile(classOf[TileItemImporter], 1)
        machine2.addTile(classOf[TileBlockPlacer], 2)
        machine2.addTile(classOf[TileFilteredImporter], 3)
        machine2.addTile(classOf[TileFireStarter], 4)
        machine2.addTile(classOf[TileBatteryBox], 5)

        ExpansionRecipes.initRecipes()
    }

    def postinit(){}

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"

    override def createPart(name:String, nbt:NBTTagCompound) = createPart(name)
    override def createPart(name:String, packet:MCDataInput) = createPart(name)

    def createPart(name:String):TMultiPart = name match
    {
        case "pr_solar" => new SolarPanelPart
    }
}

class ExpansionProxy_client extends ExpansionProxy_server
{
    val furnaceGui = 20
    val generatorGui = 21

    val blockPlacerGui = 22
    val filteredImporterGui = 23
    val batteryBoxGui = 24

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
        TileRenderRegistry.setRenderer(machine1, 0, RenderInductiveFurnace)
        TileRenderRegistry.setRenderer(machine1, 1, RenderElectrotineGenerator)
        TileRenderRegistry.setRenderer(machine2, 0, RenderBlockBreaker)
        TileRenderRegistry.setRenderer(machine2, 1, RenderItemImporter)
        TileRenderRegistry.setRenderer(machine2, 2, RenderBlockPlacer)
        TileRenderRegistry.setRenderer(machine2, 3, RenderFilteredImporter)
        TileRenderRegistry.setRenderer(machine2, 4, RenderFireStarter)
        TileRenderRegistry.setRenderer(machine2, 5, RenderBatteryBox)

        MinecraftForgeClient.registerItemRenderer(Item.getItemFromBlock(machine2), RenderBatteryBox)
        MinecraftForgeClient.registerItemRenderer(itemSolar, RenderSolarPanel)

        GuiHandler.register(GuiInductiveFurnace, furnaceGui)
        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
        GuiHandler.register(GuiFilteredImporter, filteredImporterGui)
        GuiHandler.register(GuiBatteryBox, batteryBoxGui)
        GuiHandler.register(GuiElectrotineGenerator, generatorGui)
    }
}

object ExpansionProxy extends ExpansionProxy_client

object ExpansionRecipes
{
    def initRecipes()
    {
        InductiveFurnaceRecipeLib.init()
        initItemRecipes()
        initMachineRecipes()
        initDeviceRecipes()
        initMiscRecipes()
    }

    private def initItemRecipes()
    {
        //Battery
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(battery),
            "ete","ece", "ete",
            'e':JC, PartDefs.ELECTROTINE.makeStack,
            't':JC, "ingotTin",
            'c':JC, "ingotCopper"
        ))
    }

    private def initMachineRecipes()
    {
        //Inductive Furnace
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 0),
            "bbb", "b b", "iei",
            'b':JC, Blocks.brick_block,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotine"
        ))

        //Electrotine generator
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 1),
            "bbb", "a a", "cec",
            'b':JC, Blocks.brick_block,
            'a':JC, new ItemStack(battery),
            'c':JC, Blocks.clay,
            'e':JC, "ingotElectrotine"
        ))
    }

    private def initDeviceRecipes()
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

        //Fire starter
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 4),
            "nfn", "cpc", "crc",
            'n':JC, Blocks.netherrack,
            'f':JC, Items.flint_and_steel,
            'c':JC, Blocks.cobblestone,
            'p':JC, new ItemStack(machine2, 1, 2),
            'r':JC, Items.redstone
        ))

        //Battery box
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 5),
            "bwb","bbb","iei",
            'b':JC, new ItemStack(battery),
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotine"
        ))

        //Solar panel
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemSolar),
            "sss","iwi","wew",
            's':JC, PartDefs.ELECTROSILICON.makeStack,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotine",
            'w':JC, "slabWood"
        ))
    }

    private def initMiscRecipes()
    {
    }
}