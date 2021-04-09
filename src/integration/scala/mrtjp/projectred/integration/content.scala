package mrtjp.projectred.integration

import codechicken.lib.datagen.ItemModelProvider
import codechicken.lib.datagen.recipe.RecipeProvider
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.util.CrashLock
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.api.{MultiPartType, SimpleMultiPartType}
import mrtjp.projectred.ProjectRedIntegration
import net.minecraft.block.Blocks
import net.minecraft.data.DataGenerator
import net.minecraft.util.ResourceLocation
import net.minecraftforge.client.model.generators.ExistingFileHelper
import net.minecraftforge.common.Tags
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

import java.util.function.Supplier

object IntegrationContent
{
    private val LOCK = new CrashLock("Already Initialized.")
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, ProjectRedIntegration.MOD_ID)
    private val PARTS = DeferredRegister.create(classOf[MultiPartType[_]], ProjectRedIntegration.MOD_ID)

    val integrationItemGroup = new SimpleItemGroup(ProjectRedIntegration.MOD_ID, () => GateType.TIMER.makeStack())

    val itemOrGate = ITEMS.register("or_gate", newItemPartGateSupplierForType(GateType.OR))
    val itemNorGate = ITEMS.register("nor_gate", newItemPartGateSupplierForType(GateType.NOR))
    val itemNotGate = ITEMS.register("not_gate", newItemPartGateSupplierForType(GateType.NOT))
    val itemAndGate = ITEMS.register("and_gate", newItemPartGateSupplierForType(GateType.AND))
    val itemNandGate = ITEMS.register("nand_gate", newItemPartGateSupplierForType(GateType.NAND))
    val itemXorGate = ITEMS.register("xor_gate", newItemPartGateSupplierForType(GateType.XOR))
    val itemXnorGate = ITEMS.register("xnor_gate", newItemPartGateSupplierForType(GateType.XNOR))
    val itemBufferGate = ITEMS.register("buffer_gate", newItemPartGateSupplierForType(GateType.BUFFER))
    val itemMultiplexerGate = ITEMS.register("multiplexer_gate", newItemPartGateSupplierForType(GateType.MULTIPLEXER))
    val itemPulseGate = ITEMS.register("pulse_gate", newItemPartGateSupplierForType(GateType.PULSE))
    val itemRepeaterGate = ITEMS.register("repeater_gate", newItemPartGateSupplierForType(GateType.REPEATER))
    val itemRandomizerGate = ITEMS.register("randomizer_gate", newItemPartGateSupplierForType(GateType.RANDOMIZER))
    val itemSrLatchGate = ITEMS.register("sr_latch_gate", newItemPartGateSupplierForType(GateType.SR_LATCH))
    val itemToggleLatchGate = ITEMS.register("toggle_latch_gate", newItemPartGateSupplierForType(GateType.TOGGLE_LATCH))
    val itemTransparentLatchGate = ITEMS.register("transparent_latch_gate", newItemPartGateSupplierForType(GateType.TRANSPARENT_LATCH))
    val itemLightSensorGate = ITEMS.register("light_sensor_gate", newItemPartGateSupplierForType(GateType.LIGHT_SENSOR))
    val itemRainSensorGate = ITEMS.register("rain_sensor_gate", newItemPartGateSupplierForType(GateType.RAIN_SENSOR))
    val itemTimerGate = ITEMS.register("timer_gate", newItemPartGateSupplierForType(GateType.TIMER))
    val itemSequencerGate = ITEMS.register("sequencer_gate", newItemPartGateSupplierForType(GateType.SEQUENCER))
    val itemCounterGate = ITEMS.register("counter_gate", newItemPartGateSupplierForType(GateType.COUNTER))
    val itemStateCellGate = ITEMS.register("state_cell_gate", newItemPartGateSupplierForType(GateType.STATE_CELL))
    val itemSynchronizerGate = ITEMS.register("synchronizer_gate", newItemPartGateSupplierForType(GateType.SYNCHRONIZER))
    val itemBusTransceiverGate = ITEMS.register("bus_transceiver_gate", newItemPartGateSupplierForType(GateType.BUS_TRANSCEIVER))
    val itemNullCellGate = ITEMS.register("null_cell_gate", newItemPartGateSupplierForType(GateType.NULL_CELL))
    val itemInvertCellGate = ITEMS.register("invert_cell_gate", newItemPartGateSupplierForType(GateType.INVERT_CELL))
    val itemBufferCellGate = ITEMS.register("buffer_cell_gate", newItemPartGateSupplierForType(GateType.BUFFER_CELL))
    val itemComparatorGate = ITEMS.register("comparator_gate", newItemPartGateSupplierForType(GateType.COMPARATOR))
    val itemAndCellGate = ITEMS.register("and_cell_gate", newItemPartGateSupplierForType(GateType.AND_CELL))
    val itemBusRandomizerGate = ITEMS.register("bus_randomizer_gate", newItemPartGateSupplierForType(GateType.BUS_RANDOMIZER))
    val itemBusConverterGate = ITEMS.register("bus_converter_gate", newItemPartGateSupplierForType(GateType.BUS_CONVERTER))
    val itemBusInputPanelGate = ITEMS.register("bus_input_panel_gate", newItemPartGateSupplierForType(GateType.BUS_INPUT_PANEL))
    val itemStackingLatchGate = ITEMS.register("stacking_latch_gate", newItemPartGateSupplierForType(GateType.STACKING_LATCH))
    val itemSegmentDisplayGate = ITEMS.register("segment_display_gate", newItemPartGateSupplierForType(GateType.SEGMENT_DISPLAY))
    val itemDecRandomizerGate = ITEMS.register("dec_randomizer_gate", newItemPartGateSupplierForType(GateType.DEC_RANDOMIZER))

    val partOrGate = PARTS.register("or_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.OR)))
    val partNorGate = PARTS.register("nor_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.NOR)))
    val partNotGate = PARTS.register("not_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.NOT)))
    val partAndGate = PARTS.register("and_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.AND)))
    val partNandGate = PARTS.register("nand_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.NAND)))
    val partXorGate = PARTS.register("xor_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.XOR)))
    val partXnorGate = PARTS.register("xnor_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.XNOR)))
    val partBufferGate = PARTS.register("buffer_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.BUFFER)))
    val partMultiplexerGate = PARTS.register("multiplexer_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.MULTIPLEXER)))
    val partPulseGate = PARTS.register("pulse_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.PULSE)))
    val partRepeaterGate = PARTS.register("repeater_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.REPEATER)))
    val partRandomizerGate = PARTS.register("randomizer_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.RANDOMIZER)))
    val partSrLatchGate = PARTS.register("sr_latch_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.SR_LATCH)))
    val partToggleLatchGate = PARTS.register("toggle_latch_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.TOGGLE_LATCH)))
    val partTransparentLatchGate = PARTS.register("transparent_latch_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.TRANSPARENT_LATCH)))
    val partLightSensorGate = PARTS.register("light_sensor_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.LIGHT_SENSOR)))
    val partRainSensorGate = PARTS.register("rain_sensor_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.RAIN_SENSOR)))
    val partTimerGate = PARTS.register("timer_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.TIMER)))
    val partSequencerGate = PARTS.register("sequencer_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.SEQUENCER)))
    val partCounterGate = PARTS.register("counter_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.COUNTER)))
    val partStateCellGate = PARTS.register("state_cell_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.STATE_CELL)))
    val partSynchronizerGate = PARTS.register("synchronizer_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.SYNCHRONIZER)))
    val partBusTransceiverGate = PARTS.register("bus_transceiver_gate", newPartTypeSupplierForFactory(() => new BundledGatePart(GateType.BUS_TRANSCEIVER)))
    val partNullCellGate = PARTS.register("null_cell_gate", newPartTypeSupplierForFactory(() => new ArrayGatePart(GateType.NULL_CELL)))
    val partInvertCellGate = PARTS.register("invert_cell_gate", newPartTypeSupplierForFactory(() => new ArrayGatePart(GateType.INVERT_CELL)))
    val partBufferCellGate = PARTS.register("buffer_cell_gate", newPartTypeSupplierForFactory(() => new ArrayGatePart(GateType.BUFFER_CELL)))
    val partComparatorGate = PARTS.register("comparator_gate", newPartTypeSupplierForFactory(() => new SequentialGatePart(GateType.COMPARATOR)))
    val partAndCellGate = PARTS.register("and_cell_gate", newPartTypeSupplierForFactory(() => new ArrayGatePart(GateType.AND_CELL)))
    val partBusRandomizerGate = PARTS.register("bus_randomizer_gate", newPartTypeSupplierForFactory(() => new BundledGatePart(GateType.BUS_RANDOMIZER)))
    val partBusConverterGate = PARTS.register("bus_converter_gate", newPartTypeSupplierForFactory(() => new BundledGatePart(GateType.BUS_CONVERTER)))
    val partBusInputPanelGate = PARTS.register("bus_input_panel_gate", newPartTypeSupplierForFactory(() => new BundledGatePart(GateType.BUS_INPUT_PANEL)))
    val partStackingLatchGate = PARTS.register("stacking_latch_gate", newPartTypeSupplierForFactory(() => new ArrayGatePart(GateType.STACKING_LATCH)))
    val partSegmentDisplayGate = PARTS.register("segment_display_gate", newPartTypeSupplierForFactory(() => new BundledGatePart(GateType.SEGMENT_DISPLAY)))
    val partDecRandomizerGate = PARTS.register("dec_randomizer_gate", newPartTypeSupplierForFactory(() => new ComboGatePart(GateType.DEC_RANDOMIZER)))

    def newItemPartGateSupplierForType(gateType:GateType):Supplier[ItemPartGate] =
        () => new ItemPartGate(gateType)

    def newPartTypeSupplierForFactory(factory: () => TMultiPart):Supplier[MultiPartType[_]] =
        () => new SimpleMultiPartType[TMultiPart]((client:Boolean) => factory.apply())

    def register(bus:IEventBus):Unit = {
        LOCK.lock()
        ITEMS.register(bus)
        PARTS.register(bus)
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
        }
        if (event.includeServer()) {
            gen.addProvider(new Recipes(gen))
        }
    }
}

private class ItemModels(gen: DataGenerator, fileHelper: ExistingFileHelper) extends ItemModelProvider(gen, ProjectRedIntegration.MOD_ID, fileHelper) {

    override def getName = "ProjectRed-Integration Item Models."

    override protected def registerModels():Unit = {
        val gate = getExistingFile(new ResourceLocation(ProjectRedIntegration.MOD_ID, "item/gate"))

        import IntegrationContent._

        getSimple(itemOrGate).texture(null).parent(gate)
        getSimple(itemNorGate).texture(null).parent(gate)
        getSimple(itemNotGate).texture(null).parent(gate)
        getSimple(itemAndGate).texture(null).parent(gate)
        getSimple(itemNandGate).texture(null).parent(gate)
        getSimple(itemXorGate).texture(null).parent(gate)
        getSimple(itemXnorGate).texture(null).parent(gate)
        getSimple(itemBufferGate).texture(null).parent(gate)
        getSimple(itemMultiplexerGate).texture(null).parent(gate)
        getSimple(itemPulseGate).texture(null).parent(gate)
        getSimple(itemRepeaterGate).texture(null).parent(gate)
        getSimple(itemRandomizerGate).texture(null).parent(gate)
        getSimple(itemSrLatchGate).texture(null).parent(gate)
        getSimple(itemToggleLatchGate).texture(null).parent(gate)
        getSimple(itemTransparentLatchGate).texture(null).parent(gate)
        getSimple(itemLightSensorGate).texture(null).parent(gate)
        getSimple(itemRainSensorGate).texture(null).parent(gate)
        getSimple(itemTimerGate).texture(null).parent(gate)
        getSimple(itemSequencerGate).texture(null).parent(gate)
        getSimple(itemCounterGate).texture(null).parent(gate)
        getSimple(itemStateCellGate).texture(null).parent(gate)
        getSimple(itemSynchronizerGate).texture(null).parent(gate)
        getSimple(itemBusTransceiverGate).texture(null).parent(gate)
        getSimple(itemNullCellGate).texture(null).parent(gate)
        getSimple(itemInvertCellGate).texture(null).parent(gate)
        getSimple(itemBufferCellGate).texture(null).parent(gate)
        getSimple(itemComparatorGate).texture(null).parent(gate)
        getSimple(itemAndCellGate).texture(null).parent(gate)
        getSimple(itemBusRandomizerGate).texture(null).parent(gate)
        getSimple(itemBusConverterGate).texture(null).parent(gate)
        getSimple(itemBusInputPanelGate).texture(null).parent(gate)
        getSimple(itemStackingLatchGate).texture(null).parent(gate)
        getSimple(itemSegmentDisplayGate).texture(null).parent(gate)
        getSimple(itemDecRandomizerGate).texture(null).parent(gate)
    }
}

private class Recipes(gen:DataGenerator) extends RecipeProvider(gen)
{
    override def getName = "ProjectRed-Integration Recipes."

    override protected def registerRecipes():Unit = {
        import IntegrationContent._
        import mrtjp.projectred.core.CoreContent._

        //OR gate
        shapedRecipe(itemOrGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("PCP")
                .patternLine("WCW")
                .patternLine("PWP")

        //NOR gate
        shapedRecipe(itemNorGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("PAP")
                .patternLine("WCW")
                .patternLine("PWP")

        //NOT gate
        shapedRecipe(itemNotGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("PCP")
                .patternLine("CAC")
                .patternLine("PWP")

        // AND gate
        shapedRecipe(itemAndGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACA")
                .patternLine("CCC")
                .patternLine("PWP")
        //NAND gate
        shapedRecipe(itemNandGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("AAA")
                .patternLine("CCC")
                .patternLine("PWP")

        //XOR gate
        shapedRecipe(itemXorGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('W', itemConductivePlate)
                .patternLine("AWA")
                .patternLine("CAC")
                .patternLine("WCW")

        //XNOR gate
        shapedRecipe(itemXnorGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('W', itemConductivePlate)
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WCW")

        //Buffer gate
        shapedRecipe(itemBufferGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACA")
                .patternLine("WCW")
                .patternLine("PWP")

        //Multiplexer gate
        shapedRecipe(itemMultiplexerGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACA")
                .patternLine("CPC")
                .patternLine("ACW")

        //Pulse gate
        shapedRecipe(itemPulseGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACA")
                .patternLine("CAC")
                .patternLine("WWP")

        //Repeater gate
        shapedRecipe(itemRepeaterGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("PCA")
                .patternLine("ACP")
                .patternLine("PWP")

        //Randomizer gate
        shapedRecipe(itemRandomizerGate, 1)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('E', itemEnergizedSiliconChip)
                .patternLine("PEP")
                .patternLine("WWW")
                .patternLine("EWE")

        //SR latch gate
        shapedRecipe(itemSrLatchGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACW")
                .patternLine("WPW")
                .patternLine("WCA")

        //Toggle latch gate
        shapedRecipe(itemToggleLatchGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('L', Blocks.LEVER)
                .patternLine("CPP")
                .patternLine("WLW")
                .patternLine("CPP")

        //Transparent latch gate
        shapedRecipe(itemTransparentLatchGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .patternLine("ACW")
                .patternLine("CCC")
                .patternLine("CWP")

        //Light sensor gate
        shapedRecipe(itemLightSensorGate, 1)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('B', Tags.Items.DYES_BLUE)
                .patternLine("PPP")
                .patternLine("BBB")
                .patternLine("PWP")

        //Rain sensor gate
        shapedRecipe(itemRainSensorGate, 1)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('S', Tags.Items.SLIMEBALLS)
                .patternLine("PPP")
                .patternLine("SSS")
                .patternLine("PWP")

        //Timer gate
        shapedRecipe(itemTimerGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('T', itemPointer)
                .patternLine("ACA")
                .patternLine("WTW")
                .patternLine("PWP")

        //Sequencer gate
        shapedRecipe(itemSequencerGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('T', itemPointer)
                .patternLine("PCP")
                .patternLine("CTC")
                .patternLine("PCP")

        //Counter gate
        shapedRecipe(itemCounterGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('T', itemPointer)
                .patternLine("PCP")
                .patternLine("WWT")
                .patternLine("PCP")

        //State cell gate
        shapedRecipe(itemStateCellGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('T', itemPointer)
                .key('S', itemSiliconChip)
                .patternLine("PAC")
                .patternLine("WST")
                .patternLine("PWP")

        //Synchronizer gate
        shapedRecipe(itemSynchronizerGate, 1)
                .key('A', itemAnode)
                .key('C', itemCathode)
                .key('W', itemConductivePlate)
                .key('S', itemSiliconChip)
                .patternLine("WCW")
                .patternLine("SAS")
                .patternLine("WWW")

        //Bus tranceiver gate
        shapedRecipe(itemBusTransceiverGate, 1)
                .key('P', itemPlate)
                .key('B', itemBundledPlate)
                .key('S', itemSiliconChip)
                .patternLine("BBB")
                .patternLine("SPS")
                .patternLine("BBB")

        //Null cell gate
        shapedRecipe(itemNullCellGate, 1)
                .key('P', itemPlate)
                .key('W', itemWiredPlate)
                .key('F', itemPlatformedPlate)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PWP")

        //Invert cell gate
        shapedRecipe(itemInvertCellGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemWiredPlate)
                .key('F', itemPlatformedPlate)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCP")

        //Buffer cell gate
        shapedRecipe(itemBufferCellGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemWiredPlate)
                .key('F', itemPlatformedPlate)
                .patternLine("PWP")
                .patternLine("WFW")
                .patternLine("PCC")

        //Comparator gate
        shapedRecipe(itemComparatorGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('W', itemConductivePlate)
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .patternLine("WCW")
                .patternLine("QWQ")
                .patternLine("PWP")

        //AND cell gate
        shapedRecipe(itemAndCellGate, 1)
                .key('C', itemCathode)
                .key('P', itemPlate)
                .key('w', itemConductivePlate)
                .key('W', itemWiredPlate)
                .key('S', itemPlatformedPlate)
                .patternLine("CwC")
                .patternLine("WSW")
                .patternLine("PwC")

        //Bus randomizer gate
        shapedRecipe(itemBusRandomizerGate, 1)
                .key('R', itemConductivePlate)
                .key('B', itemBundledPlate)
                .key('G', itemEnergizedSiliconChip)
                .patternLine("BBB")
                .patternLine("RGR")
                .patternLine("BBB")

        //Bus converter gate
        shapedRecipe(itemBusConverterGate, 1)
                .key('P', itemPlate)
                .key('B', itemBundledPlate)
                .key('R', itemConductivePlate)
                .key('S', itemSiliconChip)
                .patternLine("PBP")
                .patternLine("RSR")
                .patternLine("PRP")

        //Bus input panel gate
        shapedRecipe(itemBusInputPanelGate, 1)
                .key('B', itemBundledPlate)
                .key('R', itemConductivePlate)
                .key('I', tagIllumars)
                .patternLine("BRB")
                .patternLine("BIB")
                .patternLine("BBB")

        //Stacking latch gate
        shapedRecipe(itemStackingLatchGate, 1)
                .key('P', itemPlate)
                .key('C', itemCathode)
                .key('R', itemWiredPlate)
                .patternLine("PCP")
                .patternLine("RCR")
                .patternLine("PCC")

        //Segment display gate
        shapedRecipe(itemSegmentDisplayGate, 1)
                .key('P', itemPlate)
                .key('B', itemBundledPlate)
                .key('Q', Tags.Items.GEMS_QUARTZ)
                .key('S', itemSiliconChip)
                .patternLine("PBP")
                .patternLine("QSQ")
                .patternLine("PQP")


        //Dec randomizer gate
        shapedRecipe(itemDecRandomizerGate, 1)
                .key('S', itemSiliconChip)
                .key('C', itemCathode)
                .key('A', itemAnode)
                .key('E', itemEnergizedSiliconChip)
                .key('W', itemConductivePlate)
                .patternLine("SCA")
                .patternLine("CCC")
                .patternLine("EWE")
    }
}