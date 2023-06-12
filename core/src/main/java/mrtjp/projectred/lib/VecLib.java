package mrtjp.projectred.lib;

import codechicken.lib.vec.*;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;

import java.util.*;

public class VecLib {

    public static Cuboid6[] buildCubeArray(int xSize, int zSize, Cuboid6 box, Vector3 expand) {

        box.min.multiply(1 / 16D);
        box.max.multiply(1 / 16D);
        expand.multiply(1 / 16D);

        Cuboid6[] cuboids = new Cuboid6[xSize * zSize];
        for (int i = 0; i < cuboids.length; i++) {

            int x = i % xSize;
            int z = i / zSize;

            double dx = (box.max.x - box.min.x) / xSize;
            double dz = (box.max.z - box.min.z) / zSize;
            Vector3 min1 = new Vector3(box.min.x + dx * x, box.min.y, box.min.z + dz * z);
            Vector3 max1 = new Vector3(min1.x + dx, box.max.y, min1.z + dz);
            cuboids[i] = new Cuboid6(min1, max1).expand(expand);
        }
        return cuboids;
    }

    public static Transformation orientT(int orient) {
        Transformation t = Rotation.sideOrientation(orient % 24 >> 2, orient & 3);
        if (orient >= 24) t = new Scale(-1, 1, 1).with(t);
        return t.at(Vector3.CENTER);
    }

    public static int rejectComponent(BlockPos pos, int dir) {
        return rejectComponent(pos.getX(), pos.getY(), pos.getZ(), dir);
    }

    public static int rejectComponent(int x, int y, int z, int dir) {
        return switch (dir) {
            case 0, 1 -> y;
            case 2, 3 -> z;
            case 4, 5 -> x;
            default -> throw new IllegalArgumentException("Unexpected value: " + dir);
        };
    }

    public static int sign(int dir) {
        return (dir & 1) == 0 ? -1 : 1;
    }

    public static BlockPos projectDir(BlockPos pos, int dir) {
        return switch (dir) {
            case 0, 1 -> new BlockPos(pos.getX(), 0, pos.getZ());
            case 2, 3 -> new BlockPos(pos.getX(), pos.getY(), 0);
            case 4, 5 -> new BlockPos(0, pos.getY(), pos.getZ());
            default -> throw new IllegalArgumentException("Unexpected value: " + dir);
        };
    }

    public static BlockPos rejectDir(BlockPos pos, int dir) {
        return switch (dir) {
            case 0, 1 -> new BlockPos(0, pos.getY(), 0);
            case 2, 3 -> new BlockPos(0, 0, pos.getZ());
            case 4, 5 -> new BlockPos(pos.getX(), 0, 0);
            default -> throw new IllegalArgumentException("Unexpected value: " + dir);
        };
    }

    public static List<BlockPos> sortTowardsDir(Set<BlockPos> posSet, int dir) {
        List<BlockPos> posList = new ArrayList<>(posSet);
        posList.sort((a, b) -> {
            int ba = rejectComponent(a, dir);
            int bb = rejectComponent(b, dir);
            return sign(dir) * Integer.compare(ba, bb);
        });
        return posList;
    }

    /**
     * Splits a list of positions all in one row extending towards dir into lists of contiguous
     * rows without any gaps.
     *
     * @param rowList List of BlockPos all in one row along the axis towards dir
     * @param dir Direction of the row (i.e. last position is towards this dir from the first)
     * @return List of row lists, with head being the first position, and tail being some
     *         amount of offsets of head towards dir
     */
    public static List<List<BlockPos>> splitContinuousRows(List<BlockPos> rowList, int dir) {
        List<List<BlockPos>> rows = new LinkedList<>();

        LinkedList<BlockPos> row = new LinkedList<>();
        for (BlockPos next : rowList) {
            // Begin a new row
            if (row.isEmpty()) {
                row.addLast(next);
                continue;
            }
            // If next pos continues the sequence, add it to the row
            if (row.getLast().relative(Direction.values()[dir]).equals(next)) {
                row.addLast(next);

            } else { // Otherwise, there was a gap. End current row and begin a new one
                rows.add(row);
                row = new LinkedList<>();
                row.addLast(next);
            }
        }

        // Add the last row
        if (!row.isEmpty()) {
            rows.add(row);
        }

        return rows;
    }

    /**
     * Given a set of positions, this will resolve all contiguous rows of blocks towards
     * the given direction.
     *
     * @param posSet Set of positions
     * @param dir Direction of each row (i.e. last pos of each row will be towards this dir from the first)
     * @return A set of rows, each row being a list of positions
     */
    public static Set<List<BlockPos>> resolveRows(Set<BlockPos> posSet, int dir) {

        // 1. Project every position onto the plane perpendicular to the given direction
        HashMap<BlockPos, HashSet<BlockPos>> projected = new HashMap<>();
        for (BlockPos pos : posSet) {
            BlockPos proj = projectDir(pos, dir);
            projected.computeIfAbsent(proj, k -> new HashSet<>()).add(pos);
        }

        // 2. Sort each row towards given dir
        Set<List<BlockPos>> rows = new HashSet<>();
        for (HashSet<BlockPos> rowSet : projected.values()) {
            List<BlockPos> row = sortTowardsDir(rowSet, dir);
            rows.add(row);
        }

        // 3. Split each row into contiguous sub-rows
        Set<List<BlockPos>> resolvedRows = new HashSet<>();
        for (List<BlockPos> row : rows) {
            resolvedRows.addAll(splitContinuousRows(row, dir));
        }

        return resolvedRows;
    }
}
