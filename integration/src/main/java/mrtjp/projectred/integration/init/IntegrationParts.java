package mrtjp.projectred.integration.init;

import mrtjp.projectred.integration.GateType;

import static mrtjp.projectred.integration.ProjectRedIntegration.ITEMS;
import static mrtjp.projectred.integration.ProjectRedIntegration.PARTS;

public class IntegrationParts {

    public static final String ID_OR                = "or_gate";
    public static final String ID_NOR               = "nor_gate";
    public static final String ID_NOT               = "not_gate";
    public static final String ID_AND               = "and_gate";
    public static final String ID_NAND              = "nand_gate";
    public static final String ID_XOR               = "xor_gate";
    public static final String ID_XNOR              = "xnor_gate";
    public static final String ID_BUFFER            = "buffer_gate";
    public static final String ID_MULTIPLEXER       = "multiplexer_gate";
    public static final String ID_PULSE             = "pulse_gate";
    public static final String ID_REPEATER          = "repeater_gate";
    public static final String ID_RANDOMIZER        = "randomizer_gate";
    public static final String ID_SR_LATCH          = "sr_latch_gate";
    public static final String ID_TOGGLE_LATCH      = "toggle_latch_gate";
    public static final String ID_TRANSPARENT_LATCH = "transparent_latch_gate";
    public static final String ID_LIGHT_SENSOR      = "light_sensor_gate";
    public static final String ID_RAIN_SENSOR       = "rain_sensor_gate";
    public static final String ID_TIMER             = "timer_gate";
    public static final String ID_SEQUENCER         = "sequencer_gate";
    public static final String ID_COUNTER           = "counter_gate";
    public static final String ID_STATE_CELL        = "state_cell_gate";
    public static final String ID_SYNCHRONIZER      = "synchronizer_gate";
    public static final String ID_BUS_TRANSCEIVER   = "bus_transceiver_gate";
    public static final String ID_NULL_CELL         = "null_cell_gate";
    public static final String ID_INVERT_CELL       = "invert_cell_gate";
    public static final String ID_BUFFER_CELL       = "buffer_cell_gate";
    public static final String ID_COMPARATOR        = "comparator_gate";
    public static final String ID_AND_CELL          = "and_cell_gate";
    public static final String ID_BUS_RANDOMIZER    = "bus_randomizer_gate";
    public static final String ID_BUS_CONVERTER     = "bus_converter_gate";
    public static final String ID_BUS_INPUT_PANEL   = "bus_input_panel_gate";
    public static final String ID_STACKING_LATCH    = "stacking_latch_gate";
    public static final String ID_SEGMENT_DISPLAY   = "segment_display_gate";
    public static final String ID_DEC_RANDOMIZER    = "dec_randomizer_gate";

    public static void register() {
        for (GateType type : GateType.values()) {
            if (!type.isEnabled()) continue;
            type.registerParts(PARTS, ITEMS);
        }
    }
}
