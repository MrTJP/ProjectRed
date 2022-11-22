package mrtjp.projectred.fabrication.init;

public class FabricationUnlocal {

    private static final String PREFIX = "projectred-fabrication.";

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

    // General workbench UI
    public static final String UL_PLACE_BLUEPRINT   = PREFIX + "ui.place_blueprint";
    public static final String UL_BLUEPRINT_INFO    = PREFIX + "ui.blueprint_info";
    public static final String UL_BLUEPRINT_NAME    = PREFIX + "ui.blueprint_name";
    public static final String UL_BLUEPRINT_OWNER   = PREFIX + "ui.blueprint_owner";
    public static final String UL_BLUEPRINT_DIM     = PREFIX + "ui.blueprint_dim";
    public static final String UL_BLUEPRINT_LAYERS  = PREFIX + "ui.blueprint_layers";

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
    public static final String UL_TOP                = PREFIX + "tooltip.top";
    public static final String UL_RIGHT              = PREFIX + "tooltip.right";
    public static final String UL_BOTTOM             = PREFIX + "tooltip.bottom";
    public static final String UL_LEFT               = PREFIX + "tooltip.left";
    public static final String UL_BUNDLED_INPUT      = PREFIX + "tooltip.bundled_input";
    public static final String UL_BUNDLED_OUTPUT     = PREFIX + "tooltip.bundled_output";
    public static final String UL_IO_NONE            = PREFIX + "tooltip.io_none";

    // Numbers and measurements
    public static final String UL_UNIT_TICKS             = PREFIX + "unit.ticks";
    public static final String UL_DIMENSIONS_NM          = PREFIX + "dimensions.nm";
    public static final String UL_DIMENSIONS_TILES       = PREFIX + "dimensions.tiles";
    public static final String UL_DIMENSIONS_DIES        = PREFIX + "dimensions.dies";
    public static final String UL_DIMENSIONS_NM_TOTAL    = PREFIX + "dimensions.nm_total";
    public static final String UL_DIMENSIONS_TILES_TOTAL = PREFIX + "dimensions.tiles_total";
    public static final String UL_DIMENSIONS_DIES_TOTAL  = PREFIX + "dimensions.dies_total";
}
