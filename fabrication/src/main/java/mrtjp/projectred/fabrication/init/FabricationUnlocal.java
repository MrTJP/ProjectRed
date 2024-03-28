package mrtjp.projectred.fabrication.init;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;

public class FabricationUnlocal {

    private static final String PREFIX = MOD_ID + ".";

    // ICWorkbench tabs
    public static final String UL_TAB_INFO      = PREFIX + "tab.info";
    public static final String UL_TAB_EDIT      = PREFIX + "tab.edit";
    public static final String UL_TAB_COMPILE   = PREFIX + "tab.compile";
    public static final String UL_TAB_STACK     = PREFIX + "tab.stack";
    public static final String UL_TAB_TREE      = PREFIX + "tab.tree";
    public static final String UL_TAB_PROBLEMS  = PREFIX + "tab.problems";

    // Workbench tools
    public static final String UL_ERASER_TOOL       = PREFIX + "tool.eraser";
    public static final String UL_INTERACT_TOOL     = PREFIX + "tool.interact";
    public static final String UL_WIRE_TOOL         = PREFIX + "tool.wires";
    public static final String UL_GATE_TOOL         = PREFIX + "tool.gates";

    // Tile interactions
    public static final String UL_TOGGLE_STATE      = PREFIX + "interact.toggle_state";
    public static final String UL_TOGGLE_IO_MODE    = PREFIX + "interact.toggle_io_mode";
    public static final String UL_IO_MODE_INPUT     = PREFIX + "interact.io_mode.input";
    public static final String UL_IO_MODE_OUTPUT    = PREFIX + "interact.io_mode.output";
    public static final String UL_TOGGLE_COLOUR     = PREFIX + "interact.toggle_colour";
    public static final String UL_TOGGLE_DELAY      = PREFIX + "interact.toggle_delay";
    public static final String UL_SIDE_ENABLED      = PREFIX + "interact.side_enabled";
    public static final String UL_SIDE_DISABLED     = PREFIX + "interact.side_disabled";

    // Problems
    public static final String UL_MULTIPLE_DRIVERS_TITLE =  PREFIX + "problems.multiple_drivers.title";
    public static final String UL_MULTIPLE_DRIVERS_DESC  =  PREFIX + "problems.multiple_drivers.desc";
    public static final String UL_DEAD_WIRE_TITLE        =  PREFIX + "problems.dead_wire.title";
    public static final String UL_DEAD_WIRE_DESC         =  PREFIX + "problems.dead_wire.desc";
    public static final String UL_DEAD_GATE_TITLE        =  PREFIX + "problems.dead_gate.title";
    public static final String UL_DEAD_GATE_DESC         =  PREFIX + "problems.dead_gate.desc";
    public static final String UL_IO_DIR_MISMATCH_TITLE  =  PREFIX + "problems.io_dir_mismatch.title";
    public static final String UL_IO_DIR_MISMATCH_DESC   =  PREFIX + "problems.io_dir_mismatch.desc";
    public static final String UL_NO_INPUTS_TITLE        =  PREFIX + "problems.no_inputs.title";
    public static final String UL_NO_INPUTS_DESC         =  PREFIX + "problems.no_inputs.desc";
    public static final String UL_NO_OUTPUTS_TITLE       =  PREFIX + "problems.no_outputs.title";
    public static final String UL_NO_OUTPUTS_DESC        =  PREFIX + "problems.no_outputs.desc";
    public static final String UL_NO_ERRORS              =  PREFIX + "problems.no_errors";
    public static final String UL_NO_WARNINGS            =  PREFIX + "problems.no_warnings";

    // General workbench UI
    public static final String UL_PLACE_BLUEPRINT           = PREFIX + "ui.place_blueprint";
    public static final String UL_BLUEPRINT_INFO            = PREFIX + "ui.blueprint_info";
    public static final String UL_BLUEPRINT_NAME            = PREFIX + "ui.blueprint_name";
    public static final String UL_BLUEPRINT_OWNER           = PREFIX + "ui.blueprint_owner";
    public static final String UL_BLUEPRINT_DIM             = PREFIX + "ui.blueprint_dim";
    public static final String UL_BLUEPRINT_LAYERS          = PREFIX + "ui.blueprint_layers";
    public static final String UL_COMPILE                   = PREFIX + "ui.compile";
    public static final String UL_COMPILE_PROGRESS          = PREFIX + "ui.compile_progress";
    public static final String UL_COMPILE_DONE              = PREFIX + "ui.compile_done";
    public static final String UL_COMPILE_READY             = PREFIX + "ui.compile_ready";
    public static final String UL_COMPILE_FAILED            = PREFIX + "ui.compile_failed";
    public static final String UL_AUTO_COMPILE              = PREFIX + "ui.auto_compile";
    public static final String UL_AUTO_COMPILE_DESCRIPTION  = PREFIX + "ui.auto_compile_description";
    public static final String UL_AUTO_COMPILE_DISABLED     = PREFIX + "ui.auto_compile_disabled";
    public static final String UL_SIM_RUNNING               = PREFIX + "ui.sim_running";

    public static final String UL_YIELD_CALCULATOR      = PREFIX + "ui.yield_calculator";
    public static final String UL_LITHOGRAPHY_PIPELINE  = PREFIX + "ui.lithography_pipeline";
    public static final String UL_PROCESS_NODE          = PREFIX + "ui.process_node";
    public static final String UL_WAFER_TYPE            = PREFIX + "ui.wafer_type";
    public static final String UL_DIE_SIZE              = PREFIX + "ui.die_size";
    public static final String UL_WAFER_SIZE            = PREFIX + "ui.wafer_size";
    public static final String UL_DIES_PER_WAFER        = PREFIX + "ui.dies_per_wafer";
    public static final String UL_SINGLE_LAYER_YIELD    = PREFIX + "ui.single_layer_yield";
    public static final String UL_YIELD                 = PREFIX + "ui.yield";

    // IC Tiles
    public static final String UL_IO_GATE_TILE       = PREFIX + "tiles.io_gate";

    // Tile Groups
    public static final String UL_TILEGROUP_REDWIRE = PREFIX + "tilegroup.redwire";
    public static final String UL_TILEGROUP_BUNDLED = PREFIX + "tilegroup.bundled";
    public static final String UL_TILEGROUP_IO      = PREFIX + "tilegroup.io";
    public static final String UL_TILEGROUP_BASIC   = PREFIX + "tilegroup.basic";
    public static final String UL_TILEGROUP_TIMING  = PREFIX + "tilegroup.timing";
    public static final String UL_TILEGROUP_MEMORY  = PREFIX + "tilegroup.memory";

    // Item tooltips
    public static final String UL_SIZE               = PREFIX + "tooltip.size";
    public static final String UL_DEFECT_CHANCE      = PREFIX + "tooltip.defect_chance";
    public static final String UL_CORRUPTED_DISCARD  = PREFIX + "tooltip.corrupted_discard"; //"Corrupted NBT data, please discard"
    public static final String UL_NAME               = PREFIX + "tooltip.name";
    public static final String UL_TILE_COUNT         = PREFIX + "tooltip.tile_count";
    public static final String UL_IO_TYPES           = PREFIX + "tooltip.io_types";
    public static final String UL_INPUT_MASK         = PREFIX + "tooltip.input_mask";
    public static final String UL_OUTPUT_MASK        = PREFIX + "tooltip.output_mask";
    public static final String UL_TOP                = PREFIX + "tooltip.top";
    public static final String UL_RIGHT              = PREFIX + "tooltip.right";
    public static final String UL_BOTTOM             = PREFIX + "tooltip.bottom";
    public static final String UL_LEFT               = PREFIX + "tooltip.left";
    public static final String UL_BUNDLED_INPUT      = PREFIX + "tooltip.bundled_input";
    public static final String UL_BUNDLED_OUTPUT     = PREFIX + "tooltip.bundled_output";
    public static final String UL_IO_NONE            = PREFIX + "tooltip.io_none";
    public static final String UL_FAB_ERR_NOT_COMPILED      = PREFIX + "tooltip.fab_err.not_compiled";
    public static final String UL_FAB_ERR_COMPILE_FORMAT    = PREFIX + "tooltip.fab_err.compile_format";
    public static final String UL_FAB_ERR_GENERIC           = PREFIX + "tooltip.fab_err.generic";

    // Interfaces
    public static final String UL_INTERFACE_NC       = PREFIX + "interface.nc";
    public static final String UL_INTERFACE_REDSTONE = PREFIX + "interface.redstone";
    public static final String UL_INTERFACE_BUNDLED  = PREFIX + "interface.bundled";

    // Numbers and measurements
    public static final String UL_UNIT_WARNINGS          = PREFIX + "unit.warnings";
    public static final String UL_UNIT_ERRORS            = PREFIX + "unit.errors";
    public static final String UL_UNIT_TICKS             = PREFIX + "unit.ticks";
    public static final String UL_DIMENSIONS_NM          = PREFIX + "dimensions.nm";
    public static final String UL_DIMENSIONS_TILES       = PREFIX + "dimensions.tiles";
    public static final String UL_DIMENSIONS_DIES        = PREFIX + "dimensions.dies";
    public static final String UL_DIMENSIONS_NM_TOTAL    = PREFIX + "dimensions.nm_total";
    public static final String UL_DIMENSIONS_TILES_TOTAL = PREFIX + "dimensions.tiles_total";
    public static final String UL_DIMENSIONS_DIES_TOTAL  = PREFIX + "dimensions.dies_total";
}
