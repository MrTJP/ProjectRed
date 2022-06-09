package mrtjp.projectred.expansion

import codechicken.lib.datagen.ItemModelProvider
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.inventory.container.ICCLContainerType
import codechicken.lib.util.CrashLock
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.api.{MultiPartType, SimpleMultiPartType}
import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.expansion.block.{AutoCraftingBenchBlock, BatteryBoxBlock, ChargingBenchBlock, ElectrotineGeneratorBlock, InductionFurnaceBlock, ProjectBenchBlock, TeleposerBlock}
import mrtjp.projectred.expansion.item._
import net.minecraft.block.{Block, Blocks}
import net.minecraft.data.DataGenerator
import net.minecraft.item.{BlockItem, Item, ItemStack}
import net.minecraft.tileentity.TileEntityType
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.generators.{BlockModelBuilder, BlockStateProvider, ConfiguredModel, ModelFile}
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

    val projectBenchBlock = BLOCKS.register("project_bench", () => new ProjectBenchBlock)
    val projectBenchTile = TILES.register("project_bench", () => TileEntityType.Builder.of(() => new TileProjectBench, projectBenchBlock.get()).build(null))
    val projectBenchItem = ITEMS.register("project_bench", () => new BlockItem(projectBenchBlock.get(), new Item.Properties().tab(expansionItemGroup)))
    val projectBenchContainer = CONTAINERS.register("project_bench", () => ICCLContainerType.create(ContainerProjectBench))

    val batteryBoxBlock = BLOCKS.register("battery_box", () => new BatteryBoxBlock)
    val batteryBoxTile = TILES.register("battery_box", () => TileEntityType.Builder.of(() => new TileBatteryBox, batteryBoxBlock.get()).build(null))
    val batteryBoxItem = ITEMS.register("battery_box", () => new BlockItem(batteryBoxBlock.get(), new Item.Properties().tab(expansionItemGroup)))
    val batteryBoxContainer = CONTAINERS.register("battery_box", () => ICCLContainerType.create(ContainerBatteryBox))

    val chargingBenchBlock = BLOCKS.register("charging_bench", () => new ChargingBenchBlock)
    val chargingBenchTile = TILES.register("charging_bench", () => TileEntityType.Builder.of(() => new TileChargingBench, chargingBenchBlock.get).build(null))
    val chargingBenchItem = ITEMS.register("charging_bench", () => new BlockItem(chargingBenchBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val chargingBenchContainer = CONTAINERS.register("charging_bench", () => ICCLContainerType.create(ContainerChargingBench))

    val inductionFurnaceBlock = BLOCKS.register("induction_furnace", () => new InductionFurnaceBlock)
    val inductionFurnaceTile = TILES.register("induction_furnace", () => TileEntityType.Builder.of(() => new TileInductiveFurnace, inductionFurnaceBlock.get).build(null))
    val inductionFurnaceItem = ITEMS.register("induction_furnace", () => new BlockItem(inductionFurnaceBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val inductionFurnaceContainer = CONTAINERS.register("induction_furnace", () => ICCLContainerType.create(ContainerInductiveFurnace))

    val electrotineGeneratorBlock = BLOCKS.register("electrotine_generator", () => new ElectrotineGeneratorBlock)
    val electrotineGeneratorTile = TILES.register("electrotine_generator", () => TileEntityType.Builder.of(() => new TileElectrotineGenerator, electrotineGeneratorBlock.get).build(null))
    val electrotineGeneratorItem = ITEMS.register("electrotine_generator", () => new BlockItem(electrotineGeneratorBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val electrotineGeneratorContainer = CONTAINERS.register("electrotine_generator", () => ICCLContainerType.create(ContainerElectrotineGenerator))

    val autoCraftingBenchBlock = BLOCKS.register("auto_crafting_bench", () => new AutoCraftingBenchBlock)
    val autoCraftingBenchTile = TILES.register("auto_crafting_bench", () => TileEntityType.Builder.of(() => new TileAutoCrafter, autoCraftingBenchBlock.get).build(null))
    val autoCraftingBenchItem = ITEMS.register("auto_crafting_bench", () => new BlockItem(autoCraftingBenchBlock.get, new Item.Properties().tab(expansionItemGroup)))
    val autoCraftingBenchContainer = CONTAINERS.register("auto_crafting_bench", () => ICCLContainerType.create(ContainerAutoCrafter))

    val teleposerBlock = BLOCKS.register("teleposer", () => new TeleposerBlock)
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

        rotatableOppositeMatchingFacesModel(projectBenchBlock.get)
        batteryBoxModel(batteryBoxBlock.get)
        chargableTableModel(chargingBenchBlock.get)
        triStateFrontFacedPoweredMachineModel(inductionFurnaceBlock.get)
        quadStateFrontFacedPoweredMachineModel(electrotineGeneratorBlock.get())
        rotatableOppositeMatchingFacesModel(autoCraftingBenchBlock.get)
        chargableTableModel(teleposerBlock.get)
    }

    private def extend(rl:ResourceLocation, suffix:String) =
        new ResourceLocation(rl.getNamespace, rl.getPath + suffix)

    private def rotatableOppositeMatchingFacesModel(block:Block):Unit = {
        addRotatableVariants(block, createOppositeMatchingFaceModel(block))
    }

    private def batteryBoxModel(block:Block):Unit = {
        getVariantBuilder(block).forAllStates(state => {
            val charge = state.getValue(BaseMachineBlock.CHARGE_LEVEL_PROPERTY)
            ConfiguredModel.builder().modelFile(createBatteryModel(block, charge)).build()
        })
    }

    private def chargableTableModel(block:Block):Unit = {
        getVariantBuilder(block).forAllStates(state => {
            val charged = state.getValue(BaseMachineBlock.CHARGED_PROPERTY)
            ConfiguredModel.builder().modelFile(createChargeableSideAndTopModel(block, charged)).build()
        })
    }

    private def quadStateFrontFacedPoweredMachineModel(block:Block):Unit = {
        addRotatablePoweredMachineVariants(block,
            createFrontFacedPoweredMachineModel(block, 0),
            createFrontFacedPoweredMachineModel(block, 1),
            createFrontFacedPoweredMachineModel(block, 2),
            createFrontFacedPoweredMachineModel(block, 3))
    }

    private def triStateFrontFacedPoweredMachineModel(block:Block):Unit = {
        val m0 = createFrontFacedPoweredMachineModel(block, 0)
        val m1 = createFrontFacedPoweredMachineModel(block, 1)
        val m3 = createFrontFacedPoweredMachineModel(block, 3)

        // m2 not possible here (cannot be working and not charged)
        addRotatablePoweredMachineVariants(block, m0, m1, m1, m3)
    }

    private def makeTmpModel(block:Block, tex:String):Unit = {

        val textureLocation = new ResourceLocation(ProjectRedExpansion.MOD_ID, s"block/$tex")
        val modelFile = models.cubeAll(block.getRegistryName.toString, textureLocation)

        getVariantBuilder(block)
                .forAllStates(state => ConfiguredModel.builder().modelFile(modelFile).build())
    }

    private def addRotatableVariants(block:Block, model:ModelFile):Unit = {
        for (r <- 0 until 4) {
            getVariantBuilder(block)
                .partialState().`with`(BaseMachineBlock.ROTATION_PROPERTY, Int.box(r))
                .addModels(ConfiguredModel.builder().modelFile(model).rotationY(90 * r).build():_*)
        }
    }

    private def addRotatablePoweredMachineVariants(block:Block, idleModel:ModelFile, chargedModel:ModelFile, workingModel:ModelFile, chargedWorkingModel:ModelFile):Unit = {
        getVariantBuilder(block).forAllStates { state =>
            val r = state.getValue(BaseMachineBlock.ROTATION_PROPERTY)
            val isWorking:Boolean = state.getValue(BaseMachineBlock.WORKING_PROPERTY)
            val isCharged:Boolean = state.getValue(BaseMachineBlock.CHARGED_PROPERTY)

            val model = (isWorking, isCharged) match {
                case (false, false) => idleModel
                case (false, true) => chargedModel
                case (true, false) => workingModel
                case (true, true) => chargedWorkingModel
            }

            ConfiguredModel.builder()
                .modelFile(model)
                .rotationY(r * 90)
                .build()
        }
    }

    private def createFrontFacedPoweredMachineModel(block:Block, state:Int):BlockModelBuilder = {
        val texture = block.getRegistryName.getPath
        // Minecraft convention: default model name == block name
        val modelName = texture + (if (state > 0) "_state" + state else "")
        models().orientableWithBottom(modelName,
            modLoc("block/" + texture + "_side"),
            modLoc("block/" + texture + "_front_" + state),
            modLoc("block/" + texture + "_bottom"),
            modLoc("block/" + texture + "_top"))
    }

    private def createOppositeMatchingFaceModel(block:Block):BlockModelBuilder = {
        val texture = block.getRegistryName.getPath
        models().cube(texture,
            modLoc("block/" + texture + "_bottom"),
            modLoc("block/" + texture + "_top"),
            modLoc("block/" + texture + "_side_0"),
            modLoc("block/" + texture + "_side_0"),
            modLoc("block/" + texture + "_side_1"),
            modLoc("block/" + texture + "_side_1"))
            .texture("particle", modLoc("block/" + texture + "_side_0"))
    }

    private def createBatteryModel(block:Block, charge:Int):BlockModelBuilder = {
        val texture = block.getRegistryName.getPath
        val modelName = texture + (if (charge > 0) "_charge" + charge else "")
        models().cubeBottomTop(modelName,
            modLoc("block/" + texture + "_side_" + charge),
            modLoc("block/" + texture + "_bottom"),
            modLoc("block/" + texture + "_top"))
    }

    private def createChargeableSideAndTopModel(block:Block, charged:Boolean):BlockModelBuilder = {
        val texture = block.getRegistryName.getPath
        val modelName = texture + (if (charged) "_charged" else "")
        models().cubeBottomTop(modelName,
            modLoc("block/" + texture + "_side_" + (if (charged) "1" else "0")),
            modLoc("block/" + texture + "_bottom"),
            modLoc("block/" + texture + "_top_" + (if (charged) "1" else "0")))
    }
}