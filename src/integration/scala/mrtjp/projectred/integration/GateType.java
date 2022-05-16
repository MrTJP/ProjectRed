package mrtjp.projectred.integration;

import codechicken.multipart.api.MultiPartType;
import net.minecraft.item.ItemStack;

import java.util.function.Supplier;

public enum GateType
{
    OR(IntegrationContent::itemOrGate, IntegrationContent::partOrGate),
    NOR(IntegrationContent::itemNorGate, IntegrationContent::partNorGate),
    NOT(IntegrationContent::itemNotGate, IntegrationContent::partNotGate),
    AND(IntegrationContent::itemAndGate, IntegrationContent::partAndGate),
    NAND(IntegrationContent::itemNandGate, IntegrationContent::partNandGate),
    XOR(IntegrationContent::itemXorGate, IntegrationContent::partXorGate),
    XNOR(IntegrationContent::itemXnorGate, IntegrationContent::partXnorGate),
    BUFFER(IntegrationContent::itemBufferGate, IntegrationContent::partBufferGate),
    MULTIPLEXER(IntegrationContent::itemMultiplexerGate, IntegrationContent::partMultiplexerGate),
    PULSE(IntegrationContent::itemPulseGate, IntegrationContent::partPulseGate),
    REPEATER(IntegrationContent::itemRepeaterGate, IntegrationContent::partRepeaterGate),
    RANDOMIZER(IntegrationContent::itemRandomizerGate, IntegrationContent::partRandomizerGate),
    SR_LATCH(IntegrationContent::itemSrLatchGate, IntegrationContent::partSrLatchGate),
    TOGGLE_LATCH(IntegrationContent::itemToggleLatchGate, IntegrationContent::partToggleLatchGate),
    TRANSPARENT_LATCH(IntegrationContent::itemTransparentLatchGate, IntegrationContent::partTransparentLatchGate),
    LIGHT_SENSOR(IntegrationContent::itemLightSensorGate, IntegrationContent::partLightSensorGate),
    RAIN_SENSOR(IntegrationContent::itemRainSensorGate, IntegrationContent::partRainSensorGate),
    TIMER(IntegrationContent::itemTimerGate, IntegrationContent::partTimerGate),
    SEQUENCER(IntegrationContent::itemSequencerGate, IntegrationContent::partSequencerGate),
    COUNTER(IntegrationContent::itemCounterGate, IntegrationContent::partCounterGate),
    STATE_CELL(IntegrationContent::itemStateCellGate, IntegrationContent::partStateCellGate),
    SYNCHRONIZER(IntegrationContent::itemSynchronizerGate, IntegrationContent::partSynchronizerGate),
    BUS_TRANSCEIVER(IntegrationContent::itemBusTransceiverGate, IntegrationContent::partBusTransceiverGate),
    NULL_CELL(IntegrationContent::itemNullCellGate, IntegrationContent::partNullCellGate),
    INVERT_CELL(IntegrationContent::itemInvertCellGate, IntegrationContent::partInvertCellGate),
    BUFFER_CELL(IntegrationContent::itemBufferCellGate, IntegrationContent::partBufferCellGate),
    COMPARATOR(IntegrationContent::itemComparatorGate, IntegrationContent::partComparatorGate),
    AND_CELL(IntegrationContent::itemAndCellGate, IntegrationContent::partAndCellGate),
    BUS_RANDOMIZER(IntegrationContent::itemBusRandomizerGate, IntegrationContent::partBusRandomizerGate),
    BUS_CONVERTER(IntegrationContent::itemBusConverterGate, IntegrationContent::partBusConverterGate),
    BUS_INPUT_PANEL(IntegrationContent::itemBusInputPanelGate, IntegrationContent::partBusInputPanelGate),
    STACKING_LATCH(IntegrationContent::itemStackingLatchGate, IntegrationContent::partStackingLatchGate),
    SEGMENT_DISPLAY(IntegrationContent::itemSegmentDisplayGate, IntegrationContent::partSegmentDisplayGate),
    DEC_RANDOMIZER(IntegrationContent::itemDecRandomizerGate, IntegrationContent::partDecRandomizerGate),
    FABRICATED_GATE(null, null); // Filled out if Fabrication is installed

    private Supplier<Supplier<? extends ItemPartGate>> itemSupplier;
    private Supplier<Supplier<MultiPartType<?>>> partSupplier;

    private ItemPartGate item;
    private MultiPartType<?> partType;

    GateType(Supplier<Supplier<? extends ItemPartGate>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier) {
        this.itemSupplier = itemSupplier;
        this.partSupplier = partSupplier;
    }

    public MultiPartType<?> getPartType() {
        if (partType == null) {
            partType = partSupplier.get().get();
        }
        return partType;
    }

    public ItemPartGate getItem() {
        if (item == null) {
            item = itemSupplier.get().get();
        }
        return item;
    }

    public ItemStack makeStack() {
        return new ItemStack(getItem());
    }

    // TODO: Add proper gate registering mechanism
    public void inject(Supplier<Supplier<? extends ItemPartGate>> itemSupplier, Supplier<Supplier<MultiPartType<?>>> partSupplier) {
        if (this.itemSupplier != null || this.partSupplier != null) {
            throw new RuntimeException("GateType " + name() + " already registered!");
        }
        this.itemSupplier = itemSupplier;
        this.partSupplier = partSupplier;
    }
}
