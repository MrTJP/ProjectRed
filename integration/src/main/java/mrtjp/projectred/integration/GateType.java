package mrtjp.projectred.integration;

import codechicken.multipart.api.MultipartType;
import codechicken.multipart.api.SimpleMultipartType;
import mrtjp.projectred.integration.item.GatePartItem;
import mrtjp.projectred.integration.part.*;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.RegistryObject;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.function.Function;

import static mrtjp.projectred.integration.init.IntegrationParts.*;

public enum GateType
{
    OR                 (ID_OR,                  SimpleGatePart.OR::new),
    NOR                (ID_NOR,                 SimpleGatePart.NOR::new),
    NOT                (ID_NOT,                 SimpleGatePart.NOT::new),
    AND                (ID_AND,                 SimpleGatePart.AND::new),
    NAND               (ID_NAND,                SimpleGatePart.NAND::new),
    XOR                (ID_XOR,                 SimpleGatePart.XOR::new),
    XNOR               (ID_XNOR,                SimpleGatePart.XNOR::new),
    BUFFER             (ID_BUFFER,              SimpleGatePart.Buffer::new),
    MULTIPLEXER        (ID_MULTIPLEXER,         SimpleGatePart.Multiplexer::new),
    PULSE              (ID_PULSE,               SimpleGatePart.Pulse::new),
    REPEATER           (ID_REPEATER,            SimpleGatePart.Repeater::new),
    RANDOMIZER         (ID_RANDOMIZER,          SimpleGatePart.Randomizer::new),
    SR_LATCH           (ID_SR_LATCH,            ComplexGatePart.SRLatch::new),
    TOGGLE_LATCH       (ID_TOGGLE_LATCH,        ComplexGatePart.ToggleLatch::new),
    TRANSPARENT_LATCH  (ID_TRANSPARENT_LATCH,   SimpleGatePart.TransparentLatch::new),
    LIGHT_SENSOR       (ID_LIGHT_SENSOR,        SimpleGatePart.LightSensor::new),
    RAIN_SENSOR        (ID_RAIN_SENSOR,         SimpleGatePart.RainSensor::new),
    TIMER              (ID_TIMER,               ComplexGatePart.Timer::new),
    SEQUENCER          (ID_SEQUENCER,           ComplexGatePart.Sequencer::new),
    COUNTER            (ID_COUNTER,             ComplexGatePart.Counter::new),
    STATE_CELL         (ID_STATE_CELL,          ComplexGatePart.StateCell::new),
    SYNCHRONIZER       (ID_SYNCHRONIZER,        ComplexGatePart.Synchronizer::new),
    BUS_TRANSCEIVER    (ID_BUS_TRANSCEIVER,     BundledGatePart.BusTransceiver::new),
    NULL_CELL          (ID_NULL_CELL,           ArrayGatePart.NullCell::new),
    INVERT_CELL        (ID_INVERT_CELL,         ArrayGatePart.InvertCell::new),
    BUFFER_CELL        (ID_BUFFER_CELL,         ArrayGatePart.BufferCell::new),
    COMPARATOR         (ID_COMPARATOR,          ComplexGatePart.Comparator::new),
    AND_CELL           (ID_AND_CELL,            ArrayGatePart.ANDCell::new),
    BUS_RANDOMIZER     (ID_BUS_RANDOMIZER,      BundledGatePart.BusRandomizer::new),
    BUS_CONVERTER      (ID_BUS_CONVERTER,       BundledGatePart.BusConverter::new),
    BUS_INPUT_PANEL    (ID_BUS_INPUT_PANEL,     BundledGatePart.BusInputPanel::new),
    TRANSPARENT_LATCH_CELL(ID_STACKING_LATCH,      ArrayGatePart.TransparentLatchCell::new),
    SEGMENT_DISPLAY    (ID_SEGMENT_DISPLAY,     BundledGatePart.SegmentDisplay::new),
    DEC_RANDOMIZER     (ID_DEC_RANDOMIZER,      SimpleGatePart.DecodingRandomizer::new),
    FABRICATED_GATE    (null, null), // Will be injected if applicable
    ;

    private @Nullable String unlocalName;
    private @Nullable Function<GateType, GatePart> partFactory;

    private @Nullable RegistryObject<? extends Item> itemSupplier;
    private @Nullable RegistryObject<MultipartType<GatePart>> partSupplier;

    private boolean isExternalGate;

    GateType(@Nullable String unlocalName, @Nullable Function<GateType, GatePart> partFactory) {
        this.unlocalName = unlocalName;
        this.partFactory = partFactory;
        this.isExternalGate = partFactory == null;
    }

    public boolean isExternalGate() {
        return isExternalGate;
    }

    public String getUnlocalizedName() {
        assert unlocalName != null;
        return unlocalName;
    }

    public RegistryObject<? extends Item> getItemRegistryObject() {
        assert itemSupplier != null;
        return itemSupplier;
    }

    public Item getItem() {
        assert itemSupplier != null;
        return itemSupplier.get();
    }

    public ItemStack makeStack() {
        return new ItemStack(getItem());
    }

    public MultipartType<GatePart> getPartType() {
        assert partSupplier != null;
        return partSupplier.get();
    }

    public GatePart newPart() {
        assert partFactory != null;
        return partFactory.apply(this);
    }

    public void registerParts(DeferredRegister<MultipartType<?>> partRegistry, DeferredRegister<Item> itemRegistry) {
        itemSupplier = itemRegistry.register(unlocalName, () -> new GatePartItem(this));
        partSupplier = partRegistry.register(unlocalName, () -> new SimpleMultipartType<>(isClient -> Objects.requireNonNull(partFactory).apply(this)));
    }

    // TODO: Add proper gate registering mechanism
    public void inject(String unlocalName, Function<GateType, GatePart> partFactory, RegistryObject<? extends Item> itemSupplier, RegistryObject<MultipartType<GatePart>> partSupplier) {
        if (this.itemSupplier != null || this.partSupplier != null) {
            throw new RuntimeException("GateType " + name() + " already registered!");
        }
        this.unlocalName = unlocalName;
        this.partFactory = partFactory;
        this.itemSupplier = itemSupplier;
        this.partSupplier = partSupplier;
    }
}
