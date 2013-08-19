package mrtjp.projectred.exploration;

import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.world.World;

public class GeneratorMetamorphicCave extends GeneratorOre {

    private LinkedList openList = new LinkedList();
    private HashSet closedList = new HashSet();

    private int maxCaveDiameter = 96;

    public GeneratorMetamorphicCave(int id, int meta, int veinSize) {
        super(id, meta, veinSize);
    }

    private void addBlockForConversion(int x, int y, int z, int sides) {
        List blockCoords = Arrays.asList(new Integer[] { x, y, z });
        if (this.closedList.contains(blockCoords)) {
            return;
        }
        this.openList.addLast(Arrays.asList(new Integer[] { x, y, z, sides }));
        this.closedList.add(blockCoords);
    }

    private void queueSurroundingBlocks(World world, int x, int y, int z, int sides) {
        if ((world.getBlockId(x - 1, y, z) == 0) || (world.getBlockId(x + 1, y, z) == 0) || (world.getBlockId(x, y - 1, z) == 0) || (world.getBlockId(x, y + 1, z) == 0) || (world.getBlockId(x, y, z - 1) == 0) || (world.getBlockId(x, y, z + 1) == 0)) {
            sides = 6;
        }
        addBlockForConversion(x - 1, y, z, sides);
        addBlockForConversion(x + 1, y, z, sides);
        addBlockForConversion(x, y - 1, z, sides);
        addBlockForConversion(x, y + 1, z, sides);
        addBlockForConversion(x, y, z - 1, sides);
        addBlockForConversion(x, y, z + 1, sides);
    }

    public void swapStoneBlock(World world, int x, int y, int z, int sides) {
        if (world.getBlockId(x, y, z) == Block.stone.blockID) {
            world.setBlock(x, y, z, id, meta, 2);
            if (sides > 0) {
                queueSurroundingBlocks(world, x, y, z, sides - 1);
            }
            this.veinSize -= 1;
        }
    }

    public boolean generate(World world, Random random, int x, int y, int z) {
        if (world.getBlockId(x, y, z) != 0) {
            return false;
        }
        int shiftedY = y;

        while (world.getBlockId(x, shiftedY, z) != Block.stone.blockID) {
            if (shiftedY > maxCaveDiameter) {
                return false;
            }
            shiftedY++;
        }
        addBlockForConversion(x, shiftedY, z, 6);

        while ((this.openList.size() > 0) && (this.veinSize > 0)) {
            List remainingCoords = (List) this.openList.removeFirst();
            Integer[] rc = (Integer[]) remainingCoords.toArray();
            swapStoneBlock(world, rc[0].intValue(), rc[1].intValue(), rc[2].intValue(), rc[3].intValue());
        }
        return true;
    }

}
