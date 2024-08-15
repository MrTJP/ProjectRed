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
    public static final String UL_ROTATE            = PREFIX + "interact.rotate";
    public static final String UL_REFLECT           = PREFIX + "interact.reflect";
    public static final String UL_IO_RS_INPUT       = PREFIX + "interact.io_rs_input";
    public static final String UL_IO_RS_OUTPUT      = PREFIX + "interact.io_rs_output";
    public static final String UL_IO_COLORED_INPUT  = PREFIX + "interact.io_colored_input";
    public static final String UL_IO_COLORED_OUTPUT = PREFIX + "interact.io_colored_output";
    public static final String UL_IO_BUNDLED_INPUT  = PREFIX + "interact.io_bundled_input";
    public static final String UL_IO_BUNDLED_OUTPUT = PREFIX + "interact.io_bundled_output";
    public static final String UL_IO_ANALOG_INPUT   = PREFIX + "interact.io_analog_input";
    public static final String UL_IO_ANALOG_OUTPUT  = PREFIX + "interact.io_analog_output";
    public static final String UL_IO_LEVEL_HIGH     = PREFIX + "interact.io_level_high";
    public static final String UL_IO_LEVEL_LOW      = PREFIX + "interact.io_level_low";
    public static final String UL_IO_BUS_TOGGLE     = PREFIX + "interact.io_bus_toggle";
    public static final String UL_IO_DIRECTION      = PREFIX + "interact.io_direction";
    public static final String UL_IO_DIR_INPUT      = PREFIX + "interact.io_direction.input";
    public static final String UL_IO_DIR_OUTPUT     = PREFIX + "interact.io_direction.output";
    public static final String UL_IO_BUNDLED_COLOUR = PREFIX + "interact.io_bundled_color";
    public static final String UL_IO_ANALOG_LEVEL   = PREFIX + "interact.io_analog_level";
    public static final String UL_SIDE_ENABLED      = PREFIX + "interact.side_enabled";
    public static final String UL_SIDE_DISABLED     = PREFIX + "interact.side_disabled";
    public static final String UL_TIME_DELAY        = PREFIX + "interact.delay";
    public static final String UL_DEFAULT_STATE     = PREFIX + "interact.default_state";

    // Compile
    public static final String UL_COMPILE_TREE                = PREFIX + "compile.tree";
    public static final String UL_COMPILE_STACK               = PREFIX + "compile.stack";
    public static final String UL_COMPILE_CHECK_TILE_MAP      = PREFIX + "compile.step.check_tile_maps";
    public static final String UL_COMPILE_CHECK_FLAT_MAP      = PREFIX + "compile.step.check_flat_maps";
    public static final String UL_COMPILE_MERGE_TILE_MAP      = PREFIX + "compile.step.merge_tile_map";
    public static final String UL_COMPILE_MERGE_FLAT_MAP      = PREFIX + "compile.step.merge_flat_map";
    public static final String UL_COMPILE_MERGE_TILE_MAP_PRE  = PREFIX + "compile.step.merge_tile_map_pre";
    public static final String UL_COMPILE_PHASE_1             = PREFIX + "compile.step.merge_tile_map_phase1";
    public static final String UL_COMPILE_ALLOC               = PREFIX + "compile.step.merge_tile_map_alloc";
    public static final String UL_COMPILE_PHASE_2             = PREFIX + "compile.step.merge_tile_map_phase2";
    public static final String UL_COMPILE_PATHFIND            = PREFIX + "compile.step.merge_tile_map_pathfind";
    public static final String UL_COMPILE_PHASE_3             = PREFIX + "compile.step.merge_tile_map_phase3";
    public static final String UL_COMPILE_PF_MANIFEST         = PREFIX + "compile.step.merge_tile_map_manifest_search";
    public static final String UL_COMPILE_PHASE_4             = PREFIX + "compile.step.merge_tile_map_phase4";
    public static final String UL_COMPILE_ADD_REMAPS          = PREFIX + "compile.step.merge_tile_map_add_remaps";
    public static final String UL_COMPILE_PHASE_5             = PREFIX + "compile.step.merge_tile_map_phase5";
    public static final String UL_COMPILE_REMAP               = PREFIX + "compile.step.merge_tile_map_consume_remaps";
    public static final String UL_COMPILE_PHASE_6             = PREFIX + "compile.step.merge_tile_map_phase6";
    public static final String UL_COMPILE_COLLECT             = PREFIX + "compile.step.merge_tile_map_collect";
    public static final String UL_COMPILE_MERGE_TILE_MAP_POST = PREFIX + "compile.step.merge_tile_map_post";

    public static final String UL_COMPILE_CHECK_TILE_MAP_DESC      = PREFIX + "compile.step.check_tile_maps.desc";
    public static final String UL_COMPILE_CHECK_FLAT_MAP_DESC      = PREFIX + "compile.step.check_flat_maps.desc";
    public static final String UL_COMPILE_MERGE_TILE_MAP_DESC      = PREFIX + "compile.step.merge_tile_map.desc";
    public static final String UL_COMPILE_MERGE_FLAT_MAP_DESC      = PREFIX + "compile.step.merge_flat_map.desc";
    public static final String UL_COMPILE_MERGE_TILE_MAP_PRE_DESC  = PREFIX + "compile.step.merge_tile_map_pre.desc";
    public static final String UL_COMPILE_PHASE_1_DESC             = PREFIX + "compile.step.merge_tile_map_phase1.desc";
    public static final String UL_COMPILE_ALLOC_DESC               = PREFIX + "compile.step.merge_tile_map_alloc.desc";
    public static final String UL_COMPILE_PHASE_2_DESC             = PREFIX + "compile.step.merge_tile_map_phase2.desc";
    public static final String UL_COMPILE_PATHFIND_DESC            = PREFIX + "compile.step.merge_tile_map_pathfind.desc";
    public static final String UL_COMPILE_PHASE_3_DESC             = PREFIX + "compile.step.merge_tile_map_phase3.desc";
    public static final String UL_COMPILE_PF_MANIFEST_DESC         = PREFIX + "compile.step.merge_tile_map_manifest_search.desc";
    public static final String UL_COMPILE_PHASE_4_DESC             = PREFIX + "compile.step.merge_tile_map_phase4.desc";
    public static final String UL_COMPILE_ADD_REMAPS_DESC          = PREFIX + "compile.step.merge_tile_map_add_remaps.desc";
    public static final String UL_COMPILE_PHASE_5_DESC             = PREFIX + "compile.step.merge_tile_map_phase5.desc";
    public static final String UL_COMPILE_REMAP_DESC               = PREFIX + "compile.step.merge_tile_map_consume_remaps.desc";
    public static final String UL_COMPILE_PHASE_6_DESC             = PREFIX + "compile.step.merge_tile_map_phase6.desc";
    public static final String UL_COMPILE_COLLECT_DESC             = PREFIX + "compile.step.merge_tile_map_collect.desc";
    public static final String UL_COMPILE_MERGE_TILE_MAP_POST_DESC = PREFIX + "compile.step.merge_tile_map_post.desc";


    // Problems
    public static final String UL_MULTIPLE_DRIVERS_TITLE =  PREFIX + "problems.multiple_drivers.title";
    public static final String UL_MULTIPLE_DRIVERS_DESC  =  PREFIX + "problems.multiple_drivers.desc";
    public static final String UL_DEAD_WIRE_TITLE        =  PREFIX + "problems.dead_wire.title";
    public static final String UL_DEAD_WIRE_DESC         =  PREFIX + "problems.dead_wire.desc";
    public static final String UL_DEAD_GATE_TITLE        =  PREFIX + "problems.dead_gate.title";
    public static final String UL_DEAD_GATE_DESC         =  PREFIX + "problems.dead_gate.desc";
    public static final String UL_IO_DIR_MISMATCH_TITLE  =  PREFIX + "problems.io_dir_mismatch.title";
    public static final String UL_IO_DIR_MISMATCH_DESC   =  PREFIX + "problems.io_dir_mismatch.desc";
    public static final String UL_IO_TYPE_MISMATCH_TITLE  =  PREFIX + "problems.io_type_mismatch.title";
    public static final String UL_IO_TYPE_MISMATCH_DESC   =  PREFIX + "problems.io_type_mismatch.desc";
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
    public static final String UL_REDSTONE_IO_GATE_TILE = PREFIX + "tiles.redstone_io_gate";
    public static final String UL_BUNDLED_COLOR_IO_GATE_TILE = PREFIX + "tiles.bundled_color_io_gate";
    public static final String UL_BUNDLED_BUS_IO_GATE_TILE = PREFIX + "tiles.bundled_bus_io_gate";
    public static final String UL_ANALOG_IO_GATE_TILE = PREFIX + "tiles.analog_io_gate";
    public static final String UL_TIMER_INTERVAL     = PREFIX + "tiles.timer.interval";
    public static final String UL_COUNTER_VALUE      = PREFIX + "tiles.counter.value";
    public static final String UL_COUNTER_MAX        = PREFIX + "tiles.counter.max";
    public static final String UL_COUNTER_INCR       = PREFIX + "tiles.counter.incr";
    public static final String UL_COUNTER_DECR       = PREFIX + "tiles.counter.decr";

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
    public static final String UL_INTERFACE_ANALOG   = PREFIX + "interface.analog";

    // Numbers and measurements //TODO drop this formattable ones and just do it in code
    public static final String UL_UNIT_WARNINGS          = PREFIX + "f.unit.warnings";
    public static final String UL_UNIT_ERRORS            = PREFIX + "f.unit.errors";
    public static final String UL_UNIT_TICKS             = PREFIX + "f.unit.ticks";
    public static final String UL_DIMENSIONS_NM          = PREFIX + "f.dimensions.nm";
    public static final String UL_DIMENSIONS_TILES       = PREFIX + "f.dimensions.tiles";
    public static final String UL_DIMENSIONS_DIES        = PREFIX + "f.dimensions.dies";
    public static final String UL_DIMENSIONS_NM_TOTAL    = PREFIX + "f.dimensions.nm_total";
    public static final String UL_DIMENSIONS_TILES_TOTAL = PREFIX + "f.dimensions.tiles_total";
    public static final String UL_DIMENSIONS_DIES_TOTAL  = PREFIX + "f.dimensions.dies_total";

    public static final String UL_UNIT_ONLY_TICKS        = PREFIX + "unit.ticks";
    public static final String UL_UNIT_ONLY_REGISTERS    = PREFIX + "unit.registers";
    public static final String UL_UNIT_ONLY_GATES        = PREFIX + "unit.gates";
    public static final String UL_UNIT_ONLY_REMAPS       = PREFIX + "unit.remaps";
    public static final String UL_UNIT_ONLY_NONE         = PREFIX + "unit.none";
}
