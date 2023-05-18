package mrtjp.projectred.integration.data;

import mrtjp.projectred.integration.GateType;
import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.integration.ProjectRedIntegration.MOD_ID;

public class IntegrationLanguageProvider extends LanguageProvider {

    public IntegrationLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Integration Language: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Integration");

        // Gates from GateType enum in order
        add(GateType.OR.getItem(), "OR Gate");
        add(GateType.NOR.getItem(), "NOR Gate");
        add(GateType.NOT.getItem(), "NOT Gate");
        add(GateType.AND.getItem(), "AND Gate");
        add(GateType.NAND.getItem(), "NAND Gate");
        add(GateType.XOR.getItem(), "XOR Gate");
        add(GateType.XNOR.getItem(), "XNOR Gate");
        add(GateType.BUFFER.getItem(), "Buffer Gate");
        add(GateType.MULTIPLEXER.getItem(), "Multiplexer");
        add(GateType.PULSE.getItem(), "Pulse Former");
        add(GateType.REPEATER.getItem(), "Repeater");
        add(GateType.RANDOMIZER.getItem(), "Randomizer");
        add(GateType.SR_LATCH.getItem(), "RS Latch");
        add(GateType.TOGGLE_LATCH.getItem(), "Toggle Latch");
        add(GateType.TRANSPARENT_LATCH.getItem(), "Transparent Latch");
        add(GateType.LIGHT_SENSOR.getItem(), "Light Sensor");
        add(GateType.RAIN_SENSOR.getItem(), "Rain Sensor");
        add(GateType.TIMER.getItem(), "Timer");
        add(GateType.SEQUENCER.getItem(), "Sequencer");
        add(GateType.COUNTER.getItem(), "Counter");
        add(GateType.STATE_CELL.getItem(), "State Cell");
        add(GateType.SYNCHRONIZER.getItem(), "Synchronizer");
        add(GateType.BUS_TRANSCEIVER.getItem(), "Bus Transceiver");
        add(GateType.NULL_CELL.getItem(), "Null Cell");
        add(GateType.INVERT_CELL.getItem(), "Invert Cell");
        add(GateType.BUFFER_CELL.getItem(), "Buffer Cell");
        add(GateType.COMPARATOR.getItem(), "Comparator");
        add(GateType.AND_CELL.getItem(), "AND Cell");
        add(GateType.BUS_RANDOMIZER.getItem(), "Bus Randomizer");
        add(GateType.BUS_CONVERTER.getItem(), "Bus Converter");
        add(GateType.BUS_INPUT_PANEL.makeStack(), "Bus Input Panel");
        add(GateType.TRANSPARENT_LATCH_CELL.makeStack(), "Transparent Latch Cell");
        add(GateType.SEGMENT_DISPLAY.makeStack(), "Segment Display");
        add(GateType.DEC_RANDOMIZER.makeStack(), "Dec Randomizer");
    }
}
