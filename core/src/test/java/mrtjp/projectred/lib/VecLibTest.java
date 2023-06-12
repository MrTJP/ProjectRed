package mrtjp.projectred.lib;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import org.junit.jupiter.api.Test;

import java.util.*;
import java.util.function.Consumer;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class VecLibTest {

    private static void iterateAround(BlockPos center, int radius, Consumer<BlockPos> consumer) {
        for (int x = center.getX() - radius; x <= center.getX() + radius; x++) {
            for (int y = center.getY() - radius; y <= center.getY() + radius; y++) {
                for (int z = center.getZ() - radius; z <= center.getZ() + radius; z++) {
                    consumer.accept(new BlockPos(x, y, z));
                }
            }
        }
    }

    private static List<BlockPos> createRow(BlockPos head, int dir, int count) {
        List<BlockPos> list = new ArrayList<>(count);
        list.add(head);
        for (int i = 1; i < count; i++) {
            list.add(head.relative(Direction.values()[dir], i));
        }
        return list;
    }

    @Test
    public void testProjectReject() {
        iterateAround(BlockPos.ZERO, 5, pos -> {
            for (int i = 0; i < 6; i++) {
                BlockPos p = VecLib.projectDir(pos, i);
                BlockPos r = VecLib.rejectDir(pos, i);
                assertEquals(pos, p.offset(r));
            }
        });
    }

    @Test
    public void testSortTowardsDir() {
        int len = 15;
        for (int dir = 0; dir < 6; dir++) {
            // Start towards other dir so origin is somewhere in middle of row
            BlockPos start = BlockPos.ZERO.relative(Direction.values()[dir ^ 1], len / 2);

            // Create set of blocks in row
            Set<BlockPos> rowSet = new HashSet<>(createRow(start, dir, len));

            // Sort row
            List<BlockPos> sortedRow = VecLib.sortTowardsDir(rowSet, dir);

            // Check sorting
            for (int i = 0; i < len; i++) {
                assertEquals(start.relative(Direction.values()[dir], i), sortedRow.get(i));
            }
        }
    }

    @Test
    public void testSplitRow() {
        iterateAround(BlockPos.ZERO, 3, p -> {
            for (int dir = 0; dir < 6; dir++) {
                for (int gapLen = 1; gapLen < 3; gapLen++) {
                    testSplitRows(1, 5, p, dir, gapLen);
                }
            }
        });
    }

    private void testSplitRows(int minLen, int maxLen, BlockPos start, int dir, int gapLen) {
        List<BlockPos> posList = new LinkedList<>();

        // Create rows of various sizes
        BlockPos head = start;
        for (int len = minLen; len <= maxLen; len++) {
            // Create entire row
            for (int i = 0; i < len; i++) {
                posList.add(head); // Add current pos
                head = head.relative(Direction.values()[dir], 1); // Offset towards dir
            }
            // Add a gap
            head = head.relative(Direction.values()[dir], gapLen);
        }

        // Ensure all blocks are in a directional row
        BlockPos p = VecLib.projectDir(start, dir);
        for (BlockPos pos : posList) {
            assertEquals(p, VecLib.projectDir(pos, dir));
        }

        // Split row by any gaps
        List<List<BlockPos>> rows = VecLib.splitContinuousRows(posList, dir);

        // Check expected number of rows
        assertEquals(maxLen - minLen + 1, rows.size());

        // Assert size of each row
        for (int i = 0; i < rows.size(); i++) {
            assertEquals(minLen + i, rows.get(i).size());
        }

        // Use sets to verify all positions are accounted for
        Set<BlockPos> initial = new HashSet<>(posList);
        Set<BlockPos> result = new HashSet<>();
        for (List<BlockPos> row : rows) {
            result.addAll(row);
        }
        assertEquals(posList.size(), initial.size());
        assertEquals(initial, result);
    }
}
