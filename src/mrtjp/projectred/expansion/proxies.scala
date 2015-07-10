package mrtjp.projectred.expansion

import java.lang.{Character => JC}

import codechicken.lib.data.MCDataInput
import codechicken.lib.packet.PacketCustom
import codechicken.multipart.MultiPartRegistry.IPartFactory2
import codechicken.multipart.{MultiPartRegistry, TMultiPart}
import cpw.mods.fml.common.registry.GameRegistry
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TileRenderRegistry
import mrtjp.core.data.{TClientKeyTracker, TServerKeyTracker}
import mrtjp.core.gui.GuiHandler
import mrtjp.projectred.ProjectRedExpansion._
import mrtjp.projectred.core.{IProxy, PartDefs}
import net.minecraft.client.Minecraft
import net.minecraft.init.{Blocks, Items}
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.client.MinecraftForgeClient
import net.minecraftforge.common.MinecraftForge
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
        itemEmptybattery = new ItemBatteryEmpty
        itemBattery = new ItemBattery
        itemJetpack = new ItemJetpack
        itemScrewdriver = new ItemElectronicScrewdriver
        itemInfusedEnderPearl = new ItemInfusedEnderPearl

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
        machine2.addTile(classOf[TileChargingBench], 6)
        machine2.addTile(classOf[TileTeleposer], 7)
        machine2.addTile(classOf[TileFrameMotor], 8)
        machine2.addTile(classOf[TileFrameActuator], 9)

        ExpansionRecipes.initRecipes()
    }

    def postinit()
    {
        SpacebarServerTracker.register()
        ForwardServerTracker.register()

        MinecraftForge.EVENT_BUS.register(TeleposedEnderPearlProperty)
    }

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
    val chargingBenchBui = 25

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
        TileRenderRegistry.setRenderer(machine2, 6, RenderChargingBench)
        TileRenderRegistry.setRenderer(machine2, 7, RenderTeleposer)
        TileRenderRegistry.setRenderer(machine2, 8, RenderFrameMotor)
        TileRenderRegistry.setRenderer(machine2, 9, RenderFrameActuator)

        MinecraftForgeClient.registerItemRenderer(Item.getItemFromBlock(machine2), RenderBatteryBox)
        MinecraftForgeClient.registerItemRenderer(itemSolar, RenderSolarPanel)

        GuiHandler.register(GuiInductiveFurnace, furnaceGui)
        GuiHandler.register(GuiBlockPlacer, blockPlacerGui)
        GuiHandler.register(GuiFilteredImporter, filteredImporterGui)
        GuiHandler.register(GuiBatteryBox, batteryBoxGui)
        GuiHandler.register(GuiElectrotineGenerator, generatorGui)
        GuiHandler.register(GuiChargingBench, chargingBenchBui)

        SpacebarClientTracker.register()
        ForwardClientTracker.register()

        ItemJetpack.register()
    }
}

object ExpansionProxy extends ExpansionProxy_client

object SpacebarServerTracker extends TServerKeyTracker
object SpacebarClientTracker extends TClientKeyTracker
{
    override def getTracker = SpacebarServerTracker
    override def getIsKeyPressed = Minecraft.getMinecraft.gameSettings.keyBindJump.getIsKeyPressed
}

object ForwardServerTracker extends TServerKeyTracker
object ForwardClientTracker extends TClientKeyTracker
{
    override def getTracker = ForwardServerTracker
    override def getIsKeyPressed = Minecraft.getMinecraft.gameSettings.keyBindForward.getIsKeyPressed
}

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
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemBattery),
            "ete","ece", "ete",
            'e':JC, PartDefs.ELECTROTINE.makeStack,
            't':JC, "ingotTin",
            'c':JC, "ingotCopper"
        ))

        //Electric Screwdriver
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemScrewdriver),
            "i  ", " s ", "  b",
            'i':JC, "ingotIron",
            's':JC, "gemSapphire",
            'b':JC, itemBattery
        ))

        //Jetpack
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemJetpack),
            "b b","bcb","eae",
            'b':JC, itemBattery,
            'c':JC, Items.diamond_chestplate,
            'e':JC, Items.emerald,
            'a':JC, new ItemStack(machine2, 1, 5)
        ))
    }

    private def initMachineRecipes()
    {
        //Inductive Furnace
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 0),
            "bbb", "b b", "iei",
            'b':JC, Blocks.brick_block,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Electrotine generator
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine1, 1, 1),
            "bbb", "a a", "cec",
            'b':JC, Blocks.brick_block,
            'a':JC, new ItemStack(itemBattery),
            'c':JC, Blocks.clay,
            'e':JC, "ingotElectrotineAlloy"
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

        //Fire Starter
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 4),
            "nfn", "cpc", "crc",
            'n':JC, Blocks.netherrack,
            'f':JC, Items.flint_and_steel,
            'c':JC, Blocks.cobblestone,
            'p':JC, new ItemStack(machine2, 1, 2),
            'r':JC, Items.redstone
        ))

        //Battery Box
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 5),
            "bwb","bib","iei",
            'b':JC, new ItemStack(itemBattery),
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Solar Panel
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(itemSolar),
            "sss","iwi","wew",
            's':JC, PartDefs.ELECTROSILICON.makeStack,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy",
            'w':JC, "slabWood"
        ))

        //Charging Bench
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 6),
            "scs","wbw","iei",
            's':JC, Blocks.stone,
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'w':JC, "plankWood",
            'b':JC, new ItemStack(itemBattery),
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Teleposer
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 7),
            "odo","wbw","iei",
            'o':JC, Blocks.obsidian,
            'd':JC, Items.diamond,
            'w':JC, "plankWood",
            'b':JC, Items.ender_pearl,
            'i':JC, "ingotIron",
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Frame Motor
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 8),
            "wiw","cmc","iei",
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'm':JC, PartDefs.MOTOR.makeStack,
            'e':JC, "ingotElectrotineAlloy"
        ))

        //Frame Linear Actuator
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(machine2, 1, 9),
            "wiw","cic","cec",
            'w':JC, "plankWood",
            'i':JC, "ingotIron",
            'c':JC, PartDefs.COPPERCOIL.makeStack,
            'e':JC, "ingotElectrotineAlloy"
        ))
    }

    private def initMiscRecipes()
    {
    }
}