package mrtjp.projectred.expansion

import codechicken.lib.datagen.ItemModelProvider
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.inventory.container.ICCLContainerType
import codechicken.lib.util.CrashLock
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.api.{MultiPartType, SimpleMultiPartType}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.item.{BatteryItem, ElectricScrewdriverItem, EmptyBatteryItem, InfusedEnderPearlItem, PlanItem}
import net.minecraft.block.{Block, Blocks}
import net.minecraft.data.DataGenerator
import net.minecraft.item.{BlockItem, Item, ItemStack}
import net.minecraft.tileentity.TileEntityType
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.generators.{BlockStateProvider, ConfiguredModel}
import net.minecraftforge.common.data.ExistingFileHelper
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

object ExpansionContent
{
    private val LOCK = new CrashLock("Already Initialized.")
    private val BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, ProjectRedExpansion.MOD_ID)
    private val TILES = DeferredRegister.create(ForgeRegistries.TILE_ENTITIES, ProjectRedExpansion.MOD_ID)
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, ProjectRedExpansion.MOD_ID)
    private val PARTS = DeferredRegister.create(classOf[MultiPartType[_]], ProjectRedExpansion.MOD_ID)
    private val CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, ProjectRedExpansion.MOD_ID)

    val expansionItemGroup = new SimpleItemGroup(ProjectRedExpansion.MOD_ID, () => new ItemStack(Blocks.DISPENSER))

    val projectBenchBlock = BLOCKS.register("project_bench", () => new BaseMachineBlock(() => new TileProjectBench))
    val projectBenchTile = TILES.register("project_bench", () => TileEntityType.Builder.of(() => new TileProjectBench, projectBenchBlock.get()).build(null))
    val projectBenchItem = ITEMS.register("project_bench", () => new BlockItem(projectBenchBlock.get(), new Item.Properties().tab(expansionItemGroup)))
    val projectBenchContainer = CONTAINERS.register("project_bench", () => ICCLContainerType.create(ContainerProjectBench))

    val batteryBoxBlock = BLOCKS.register("battery_box", () => new BaseMachineBlock(() => new TileBatteryBox))
    val batteryBoxTile = TILES.register("battery_box", () => TileEntityType.Builder.of(() => new TileBatteryBox, batteryBoxBlock.get()).build(null))
    val batteryBoxItem = ITEMS.register("battery_box", () => new BlockItem(batteryBoxBlock.get(), new Item.Properties().tab(expansionItemGroup)))
    val batteryBoxContainer = CONTAINERS.register("battery_box", () => ICCLContainerType.create(ContainerBatteryBox))

    val chargingBenchBlock = BLOCKS.register("charging_bench", () => new BaseMachineBlock(() => new TileChargingBench))
    val chargingBenchTile = TILES.register("charging_bench", () => TileEntityType.Builder.of(() => new TileChargingBench, chargingBenchBlock.get).build(null))
    val chargingBenchItem = ITEMS.register("charging_bench", () => new BlockItem(chargingBenchBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val chargingBenchContainer = CONTAINERS.register("charging_bench", () => ICCLContainerType.create(ContainerChargingBench))

    val inductionFurnaceBlock = BLOCKS.register("induction_furnace", () => new BaseMachineBlock(() => new TileInductiveFurnace))
    val inductionFurnaceTile = TILES.register("induction_furnace", () => TileEntityType.Builder.of(() => new TileInductiveFurnace, inductionFurnaceBlock.get).build(null))
    val inductionFurnaceItem = ITEMS.register("induction_furnace", () => new BlockItem(inductionFurnaceBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val inductionFurnaceContainer = CONTAINERS.register("induction_furnace", () => ICCLContainerType.create(ContainerInductiveFurnace))

    val electrotineGeneratorBlock = BLOCKS.register("electrotine_generator", () => new BaseMachineBlock(() => new TileElectrotineGenerator))
    val electrotineGeneratorTile = TILES.register("electrotine_generator", () => TileEntityType.Builder.of(() => new TileElectrotineGenerator, electrotineGeneratorBlock.get).build(null))
    val electrotineGeneratorItem = ITEMS.register("electrotine_generator", () => new BlockItem(electrotineGeneratorBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val electrotineGeneratorContainer = CONTAINERS.register("electrotine_generator", () => ICCLContainerType.create(ContainerElectrotineGenerator))

    val autoCraftingBenchBlock = BLOCKS.register("auto_crafting_bench", () => new BaseMachineBlock(() => new TileAutoCrafter))
    val autoCraftingBenchTile = TILES.register("auto_crafting_bench", () => TileEntityType.Builder.of(() => new TileAutoCrafter, autoCraftingBenchBlock.get).build(null))
    val autoCraftingBenchItem = ITEMS.register("auto_crafting_bench", () => new BlockItem(autoCraftingBenchBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val autoCraftingBenchContainer = CONTAINERS.register("auto_crafting_bench", () => ICCLContainerType.create(ContainerAutoCrafter))

    val teleposerBlock = BLOCKS.register("teleposer", () => new BaseMachineBlock(() => new TileTeleposer))
    val teleposerTile = TILES.register("teleposer", () => TileEntityType.Builder.of(() => new TileTeleposer, teleposerBlock.get).build(null))
    val teleposerItem = ITEMS.register("teleposer", () => new BlockItem(teleposerBlock.get, new Item.Properties().tab(expansionItemGroup)))


    /** Items **/
    val batteryItem = ITEMS.register("battery", () => new BatteryItem)
    val emptyBatteryItem = ITEMS.register("empty_battery", () => new EmptyBatteryItem)
    val planItem = ITEMS.register("plan", () => new PlanItem)
    val electricScrewdriverItem = ITEMS.register("electric_screwdriver", () => new ElectricScrewdriverItem)
    val solarPanelItem = ITEMS.register("solar_panel", () => new ItemSolarPanel)
    val infusedEnderPearlItem = ITEMS.register("infused_ender_pearl", () => new InfusedEnderPearlItem)

    /** Parts **/
    val solarPanelPart = PARTS.register("solar_panel", () => new SimpleMultiPartType[TMultiPart]((client:Boolean) => new SolarPanelPart))

    def register(bus:IEventBus):Unit = {
        LOCK.lock()
        BLOCKS.register(bus)
        TILES.register(bus)
        ITEMS.register(bus)
        PARTS.register(bus)
        CONTAINERS.register(bus)

        bus.register(DataGen)
    }
}

private object DataGen {
    @SubscribeEvent
    def gatherDataGenerators(event: GatherDataEvent):Unit = {
        val gen = event.getGenerator
        val helper = event.getExistingFileHelper
        if (event.includeClient()) {
            gen.addProvider(new ItemModels(gen, helper))
            gen.addProvider(new BlockStates(gen, helper))
        }
        if (event.includeServer()) {
            //            gen.addProvider(new Recipes(gen))
        }
    }
}

private class ItemModels(gen: DataGenerator, fileHelper: ExistingFileHelper) extends ItemModelProvider(gen, ProjectRedExpansion.MOD_ID, fileHelper) {

    override def getName:String = "ProjectRed-Expansion Item Models."

    override protected def registerModels():Unit = {
        import ExpansionContent._

        simpleItemBlock(batteryBoxBlock.get)
        simpleItemBlock(chargingBenchBlock.get)
        simpleItemBlock(inductionFurnaceBlock.get)
        simpleItemBlock(electrotineGeneratorBlock.get)
        simpleItemBlock(projectBenchBlock.get)
        simpleItemBlock(autoCraftingBenchBlock.get)
        simpleItemBlock(teleposerBlock.get)

//        val solarModel = getExistingFile(new ResourceLocation(ProjectRedExpansion.MOD_ID, "item/solar_panel"))
//        getSimple(solarPanelItem).texture(null).parent(solarModel)

        generated(emptyBatteryItem)
        generated(batteryItem)
        generated(planItem)
        generated(electricScrewdriverItem)
        generated(infusedEnderPearlItem)
    }
}

private class BlockStates(gen:DataGenerator, fileHelper:ExistingFileHelper) extends BlockStateProvider(gen, ProjectRedExpansion.MOD_ID, fileHelper) {

    override def getName:String = "ProjectRed-Expansion Block Models"

    override protected def registerStatesAndModels():Unit = {
        import ExpansionContent._

        //Temp single-texture models
        makeTmpModel(projectBenchBlock.get, "project_bench/top")
        makeTmpModel(batteryBoxBlock.get, "battery_box/side8")
        makeTmpModel(chargingBenchBlock.get, "charging_bench/side1")
        makeTmpModel(inductionFurnaceBlock.get, "induction_furnace/side1")
        makeTmpModel(electrotineGeneratorBlock.get, "electrotine_generator/side2a")
        makeTmpModel(autoCraftingBenchBlock.get, "auto_crafting_bench/top")
        makeTmpModel(teleposerBlock.get, "teleposer/top1")
    }

    private def extend(rl:ResourceLocation, suffix:String) =
        new ResourceLocation(rl.getNamespace, rl.getPath + suffix)

    private def makeTmpModel(block:Block, tex:String):Unit = {

        val textureLocation = new ResourceLocation(ProjectRedExpansion.MOD_ID, s"block/$tex")
        val modelFile = models.cubeAll(block.getRegistryName.toString, textureLocation)

        getVariantBuilder(block)
                .forAllStates(state => ConfiguredModel.builder().modelFile(modelFile).build())
    }
}