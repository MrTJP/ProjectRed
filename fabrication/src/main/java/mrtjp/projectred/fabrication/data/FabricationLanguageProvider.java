package mrtjp.projectred.fabrication.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;
import static mrtjp.projectred.fabrication.init.FabricationItems.*;
import static mrtjp.projectred.fabrication.init.FabricationParts.FABRICATED_GATE_ITEM;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class FabricationLanguageProvider extends LanguageProvider {

    public FabricationLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Fabrication Languages: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Fabrication");

        add(IC_WORKBENCH_BLOCK.get(), "IC Workbench");
        add(PLOTTING_TABLE_BLOCK.get(), "Plotting Table");
        add(LITHOGRAPHY_TABLE_BLOCK.get(), "Lithography Table");
        add(PACKAGING_TABLE_BLOCK.get(), "Packaging Table");

        add(IC_BLUEPRINT_ITEM.get(), "IC Blueprint");
        add(BLANK_PHOTOMASK_ITEM.get(), "Blank Photomask");
        add(PHOTOMASK_SET_ITEM.get(), "Photomask Set");
        add(ROUGH_SILICON_WAFER_ITEM.get(), "Rough Silicon Wafer");
        add(ETCHED_SILICON_WAFER_ITEM.get(), "Etched Silicon Wafer");
        add(VALID_DIE_ITEM.get(), "Valid Die");
        add(INVALID_DIE_ITEM.get(), "Invalid Die");

        add(FABRICATED_GATE_ITEM.get(), "Fabricated Gate");

        add(UL_TAB_INFO, "Info");
        add(UL_TAB_EDIT, "Edit");
        add(UL_TAB_COMPILE, "Compile");
        add(UL_TAB_STACK, "Stack");
        add(UL_TAB_TREE, "Tree");
        add(UL_TAB_PROBLEMS, "Problems");

        add(UL_ERASER_TOOL, "Erase");
        add(UL_INTERACT_TOOL, "Interact");
        add(UL_WIRE_TOOL, "Wires");
        add(UL_GATE_TOOL, "Gates");

        add(UL_ROTATE, "Rotate");
        add(UL_REFLECT, "Reflect");
        add(UL_SIM_INPUT, "Simulation input");
        add(UL_SIM_OUTPUT, "Simulation output");
        add(UL_IO_DIRECTION, "IO direction");
        add(UL_IO_DIR_INPUT, "Input");
        add(UL_IO_DIR_OUTPUT, "Output");
        add(UL_SIGNAL_COLOUR, "Signal colour");
        add(UL_SIDE_ENABLED, "Side enabled");
        add(UL_SIDE_DISABLED, "Side disabled");
        add(UL_TIME_DELAY, "Delay");
        add(UL_DEFAULT_STATE, "Default state");

        add(UL_COMPILE_TREE, "Tree");
        add(UL_COMPILE_STACK, "Stack");
        add(UL_COMPILE_CHECK_TILE_MAP, "Check tile maps");
        add(UL_COMPILE_CHECK_FLAT_MAP, "Check flat maps");
        add(UL_COMPILE_MERGE_TILE_MAP, "Compile tile map");
        add(UL_COMPILE_MERGE_FLAT_MAP, "Compile flat map");
        add(UL_COMPILE_MERGE_TILE_MAP_PRE, "Pre-compile");
        add(UL_COMPILE_PHASE_1, "Allocation phase");
        add(UL_COMPILE_ALLOC, "Alloc");
        add(UL_COMPILE_PHASE_2, "Pathfind phase");
        add(UL_COMPILE_PATHFIND, "Pathfind");
        add(UL_COMPILE_PHASE_3, "Lookup phase");
        add(UL_COMPILE_PF_MANIFEST, "Lookup");
        add(UL_COMPILE_PHASE_4, "Add remaps phase");
        add(UL_COMPILE_ADD_REMAPS, "Add remaps");
        add(UL_COMPILE_PHASE_5, "Remap phase");
        add(UL_COMPILE_REMAP, "Remap");
        add(UL_COMPILE_PHASE_6, "Collection phase");
        add(UL_COMPILE_COLLECT, "Collect");
        add(UL_COMPILE_MERGE_TILE_MAP_POST, "Post-compile");

        add(UL_COMPILE_CHECK_TILE_MAP_DESC, "Check for tile maps to compile");
        add(UL_COMPILE_CHECK_FLAT_MAP_DESC, "Check for flat maps to merge");
        add(UL_COMPILE_MERGE_TILE_MAP_DESC, "Compile tile map");
        add(UL_COMPILE_MERGE_FLAT_MAP_DESC, "Merge flat map");
        add(UL_COMPILE_MERGE_TILE_MAP_PRE_DESC, "Pre-compile");
        add(UL_COMPILE_PHASE_1_DESC, "Allocate memory for all tiles");
        add(UL_COMPILE_ALLOC_DESC, "Output registers and gate allocation");
        add(UL_COMPILE_PHASE_2_DESC, "Search for connected registers");
        add(UL_COMPILE_PATHFIND_DESC, "Input register search");
        add(UL_COMPILE_PHASE_3_DESC, "Look up passing signals");
        add(UL_COMPILE_PF_MANIFEST_DESC, "Passing signals lookup");
        add(UL_COMPILE_PHASE_4_DESC, "Declare register remaps");
        add(UL_COMPILE_ADD_REMAPS_DESC, "Declare remaps");
        add(UL_COMPILE_PHASE_5_DESC, "Apply register remaps");
        add(UL_COMPILE_REMAP_DESC, "Apply remaps");
        add(UL_COMPILE_PHASE_6_DESC, "Collect gates and registers");
        add(UL_COMPILE_COLLECT_DESC, "Gates and registers collected");
        add(UL_COMPILE_MERGE_TILE_MAP_POST_DESC, "Post-compile");

        add(UL_MULTIPLE_DRIVERS_TITLE, "Multiple drivers");
        add(UL_MULTIPLE_DRIVERS_DESC, "Multiple registers connected to an input:");
        add(UL_DEAD_WIRE_TITLE, "Dead wire");
        add(UL_DEAD_WIRE_DESC, "No signals pass through this wire");
        add(UL_DEAD_GATE_TITLE, "Dead gate");
        add(UL_DEAD_GATE_DESC, "No signals drive this gate");
        add(UL_IO_DIR_MISMATCH_TITLE, "IO direction mismatch");
        add(UL_IO_DIR_MISMATCH_DESC, "Side has IO gates with conflicting directions");
        add(UL_NO_INPUTS_TITLE, "No inputs");
        add(UL_NO_INPUTS_DESC, "Design has no inputs");
        add(UL_NO_OUTPUTS_TITLE, "No outputs");
        add(UL_NO_OUTPUTS_DESC, "Design has no outputs");
        add(UL_NO_ERRORS, "No errors");
        add(UL_NO_WARNINGS, "No warnings");

        add(UL_PLACE_BLUEPRINT, "Place blueprint");
        add(UL_BLUEPRINT_INFO, "Blueprint Info");
        add(UL_BLUEPRINT_NAME, "Name");
        add(UL_BLUEPRINT_OWNER, "Owner");
        add(UL_BLUEPRINT_DIM, "Dimensions");
        add(UL_BLUEPRINT_LAYERS, "Layers");
        add(UL_COMPILE, "Compile");
        add(UL_COMPILE_PROGRESS, "Compiling (%d/%d)");
        add(UL_COMPILE_DONE, "Done (%d/%d)");
        add(UL_COMPILE_READY, "Ready to compile");
        add(UL_COMPILE_FAILED, "Compile failed");
        add(UL_AUTO_COMPILE, "Auto-compile");
        add(UL_AUTO_COMPILE_DESCRIPTION, "Enable to auto-compile design on change");
        add(UL_AUTO_COMPILE_DISABLED, "Disabled. Design is too complex");
        add(UL_SIM_RUNNING, "Simulation running");

        add(UL_YIELD_CALCULATOR, "Yield Calculator");
        add(UL_LITHOGRAPHY_PIPELINE, "Lithography pipeline");
        add(UL_PROCESS_NODE, "Process node");
        add(UL_WAFER_TYPE, "Wafer type");
        add(UL_DIE_SIZE, "Die size");
        add(UL_WAFER_SIZE, "Wafer size");
        add(UL_DIES_PER_WAFER, "Dies per wafer");
        add(UL_SINGLE_LAYER_YIELD, "Single layer yield");
        add(UL_YIELD, "Yield");

        add(UL_SIZE, "Size");
        add(UL_DEFECT_CHANCE, "Defect chance");
        add(UL_CORRUPTED_DISCARD, "Corrupted NBT data, please discard");
        add(UL_NAME, "Name");
        add(UL_TILE_COUNT, "Tile count");
        add(UL_IO_TYPES, "IO types");
        add(UL_INPUT_MASK, "Input mask");
        add(UL_OUTPUT_MASK, "Output mask");
        add(UL_TOP, "Top");
        add(UL_RIGHT, "Right");
        add(UL_BOTTOM, "Bottom");
        add(UL_LEFT, "Left");
        add(UL_BUNDLED_INPUT, "Bundled input");
        add(UL_BUNDLED_OUTPUT, "Bundled output");
        add(UL_IO_NONE, "None");
        add(UL_FAB_ERR_NOT_COMPILED, "Cannot fabricate: Not compiled");
        add(UL_FAB_ERR_COMPILE_FORMAT, "Cannot fabricate: Re-compile required");
        add(UL_FAB_ERR_GENERIC, "Cannot fabricate");

        add(UL_IO_GATE_TILE, "IO Gate");
        add(UL_TIMER_INTERVAL, "Interval");
        add(UL_COUNTER_VALUE, "Value");
        add(UL_COUNTER_MAX, "Max");
        add(UL_COUNTER_INCR, "Increment");
        add(UL_COUNTER_DECR, "Decrement");

        add(UL_TILEGROUP_REDWIRE, "Redwire");
        add(UL_TILEGROUP_BUNDLED, "Bundled");
        add(UL_TILEGROUP_IO, "IO");
        add(UL_TILEGROUP_BASIC, "Basic");
        add(UL_TILEGROUP_TIMING, "Timing");
        add(UL_TILEGROUP_MEMORY, "Memory");

        add(UL_INTERFACE_NC, "Not connected");
        add(UL_INTERFACE_REDSTONE, "Redstone");
        add(UL_INTERFACE_BUNDLED, "Bundled");

        add(UL_UNIT_WARNINGS, "%d warnings");
        add(UL_UNIT_ERRORS, "%d errors");
        add(UL_UNIT_TICKS, "%d ticks");
        add(UL_DIMENSIONS_NM, "%d nm x %d nm");
        add(UL_DIMENSIONS_TILES, "%d tiles x %d tiles");
        add(UL_DIMENSIONS_DIES, "%d dies x %d dies");
        add(UL_DIMENSIONS_NM_TOTAL, "%d nm x %d nm (%d nm^2)");
        add(UL_DIMENSIONS_TILES_TOTAL, "%d tiles x %d tiles (%d tiles^2)");
        add(UL_DIMENSIONS_DIES_TOTAL, "%d dies x %d dies (%d dies^2)");

        add(UL_UNIT_ONLY_TICKS, "ticks");
        add(UL_UNIT_ONLY_REGISTERS, "registers");
        add(UL_UNIT_ONLY_GATES, "gates");
        add(UL_UNIT_ONLY_REMAPS, "remaps");
        add(UL_UNIT_ONLY_NONE, "none");
    }
}
