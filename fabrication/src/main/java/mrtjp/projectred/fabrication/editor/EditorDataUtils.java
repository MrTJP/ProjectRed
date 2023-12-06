package mrtjp.projectred.fabrication.editor;

import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.engine.InterfaceSpec;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;

import javax.annotation.Nullable;
import java.util.List;

public class EditorDataUtils {

    // ICEditor
    public static final String KEY_FORMAT = "format"; // int
    public static final String KEY_ACTIVE = "active"; // boolean
    public static final String KEY_IC_NAME = "ic_name"; // String
    public static final String KEY_TILE_MAP = "tile_map"; // CompoundTag
    public static final String KEY_TILE_COUNT = "tile_count"; // int
    public static final String KEY_IS_BUILT = "is_built"; // boolean
    public static final String KEY_IO_SPEC = "io_spec";

    // ICEditorStateMachine
    public static final String KEY_COMP_STATE = "state"; // byte
    public static final String KEY_FLAT_MAP = "flat_map"; // String
    public static final String KEY_SIMULATION = "sim_cont"; // CompoundTag
    public static final String KEY_COMPILER_LOG = "compiler_log"; // CompoundTag

    // ICIssuesLog
    public static final String KEY_COMPLETED_STEPS = "completed_steps"; // int
    public static final String KEY_CURRENT_PATH = "current_path"; // int array
    public static final String KEY_PROBLEMS_LIST = "problems"; // ListTag
    public static final String KEY_ERROR_COUNT = "error_count"; // int
    public static final String KEY_WARNING_COUNT = "warning_count"; // int

    public static int getFormat(CompoundTag tag) {
        return tag.getInt(KEY_FORMAT);
    }

    public static boolean hasEditorData(@Nullable CompoundTag tag) {
        return tag != null &&
                tag.contains(KEY_FORMAT) &&
                tag.contains(KEY_ACTIVE) &&
                tag.contains(KEY_TILE_MAP);
    }

    // Minimum subset of data required to fabricate gate (i.e. create photomask)
    public static boolean hasFabricationTarget(@Nullable CompoundTag tag) {
        return tag != null &&
                tag.contains(KEY_IS_BUILT) &&
                tag.contains(KEY_FLAT_MAP);
    }

    public static boolean canFabricate(@Nullable CompoundTag tag) {
        return hasFabricationTarget(tag) && tag.getBoolean(KEY_IS_BUILT) && getErrorCount(tag) == 0;
    }

    public static int getErrorCount(CompoundTag tag) {
        return tag.getCompound(KEY_COMPILER_LOG).getInt(KEY_ERROR_COUNT);
    }

    public static int getWarningCount(CompoundTag tag) {
        return tag.getCompound(KEY_COMPILER_LOG).getInt(KEY_WARNING_COUNT);
    }

    // Creates copy of editor tag with only the data required to fabricate a gate
    public static CompoundTag createFabricationCopy(CompoundTag editorTag) {
        CompoundTag copy = new CompoundTag();
        copy.putBoolean(KEY_IS_BUILT, editorTag.getBoolean(KEY_IS_BUILT));
        copy.putString(KEY_IC_NAME, editorTag.getString(KEY_IC_NAME));
        copy.putInt(KEY_TILE_COUNT, editorTag.getInt(KEY_TILE_COUNT));
        copy.put(KEY_IO_SPEC, editorTag.getCompound(KEY_IO_SPEC));
        copy.putString(KEY_FLAT_MAP, editorTag.getString(KEY_FLAT_MAP));
        return copy;
    }

    public static InterfaceSpec getInterfaceSpec(CompoundTag tag) {
        return InterfaceSpec.createFrom(tag, KEY_IO_SPEC);
    }

    public static void saveTileCoord(CompoundTag tag, String key, TileCoord coord) {
        tag.putByte(key + "_x", (byte) coord.x);
        tag.putByte(key + "_y", (byte) coord.y);
        tag.putByte(key + "_z", (byte) coord.z);
    }

    public static TileCoord loadTileCoord(CompoundTag tag, String key) {
        return new TileCoord(
                tag.getByte(key + "_x"),
                tag.getByte(key + "_y"),
                tag.getByte(key + "_z")
        );
    }

    public static void saveTileCoordList(CompoundTag tag, String key, Iterable<TileCoord> coordList) {
        ListTag list = new ListTag();
        for (TileCoord coord : coordList) {
            list.add(tileCoordToNBT(coord));
        }
        tag.put(key, list);
    }

    public static void loadTileCoordList(CompoundTag tag, String key, List<TileCoord> coordList) {
        ListTag list = tag.getList(key, Tag.TAG_COMPOUND);
        for (int i = 0; i < list.size(); i++) {
            coordList.add(tileCoordFromNBT(list.getCompound(i)));
        }
    }

    public static CompoundTag tileCoordToNBT(TileCoord coord) {
        CompoundTag tag = new CompoundTag();
        saveTileCoord(tag, "", coord);
        return tag;
    }

    public static TileCoord tileCoordFromNBT(CompoundTag tag) {
        return loadTileCoord(tag, "");
    }
}
