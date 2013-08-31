package mrtjp.projectred.exploration;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockFlower;
import net.minecraft.world.World;

public class GeneratorVolcano extends GeneratorOre {

    LinkedList openList = new LinkedList();
    HashMap closedList = new HashMap();

    public GeneratorVolcano(int id, int meta, int veinSize) {
        super(id, meta, veinSize);
    }

    @Override
    public boolean generate(World world, Random random, int x, int y, int z) {
        if (world.getBlockId(x, y, z) != Block.lavaStill.blockID)
            return false;

        int grassHeight = world.getHeightValue(x, z);
        int lavaid = Block.lavaMoving.blockID;

        // Make a tube of volcano from the underground lake to grass level.
        while (canReplaceId(world.getBlockId(x, grassHeight - 1, z)))
            grassHeight--;

        for (int i = y; i < grassHeight; i++) {
            world.setBlock(x, i, z, lavaid);
            world.setBlock(x - 1, i, z, this.id, meta, 3);
            world.setBlock(x + 1, i, z, this.id, meta, 3);
            world.setBlock(x, i, z - 1, this.id, meta, 3);
            world.setBlock(x, i, z + 1, this.id, meta, 3);
        }

        // Start at the bottom layer above the grass. Each time we go up, make
        // the layer smaller.
        int head = 3;
        int spread = random.nextInt(1);
        int currentYIndex = grassHeight;

        while (this.veinSize > 0) {
            boolean breakOut = false;
            while (this.openList.size() == 0) {
                world.setBlock(x, currentYIndex, z, lavaid);
                this.closedList.clear();
                queueNeighboringBlocks(x, currentYIndex, z, head, random);
                currentYIndex++;
                if (currentYIndex > 125) {
                    breakOut = true;
                    break;
                }
            }
            if (breakOut)
                break;

            Integer[] coord = (Integer[]) ((List) this.openList.removeFirst()).toArray();

            world.getBlockId(coord[0].intValue(), 64, coord[2].intValue());
            if (world.blockExists(coord[0].intValue(), 64, coord[2].intValue())) {
                int pow = ((Integer) this.closedList.get(Arrays.asList(new Integer[] { coord[0], coord[2] }))).intValue();
                int currentLevel = world.getHeightValue(coord[0].intValue(), coord[2].intValue()) + 1;
                while ((currentLevel > 0) && (canReplaceId(world.getBlockId(coord[0].intValue(), currentLevel - 1, coord[2].intValue())))) {
                    currentLevel--;
                }
                if (currentLevel <= coord[1].intValue()) {
                    int nextBlock = world.getBlockId(coord[0].intValue(), currentLevel, coord[2].intValue());
                    if (canReplaceId(nextBlock)) {
                        destroyTree(world, coord[0].intValue(), currentLevel, coord[2].intValue());
                        world.setBlock(coord[0].intValue(), currentLevel, coord[2].intValue(), this.id, this.meta, 3);

                        if (coord[1].intValue() > currentLevel)
                            pow = Math.max(pow, spread);

                        queueNeighboringBlocks(coord[0].intValue(), currentLevel, coord[2].intValue(), pow, random);
                        this.veinSize -= 1;
                    }
                }
            }
        }

        world.setBlock(x, currentYIndex, z, lavaid);

        while ((currentYIndex > grassHeight) && (world.getBlockId(x, currentYIndex, z) == lavaid)) {
            world.markBlockForUpdate(x, currentYIndex, z);
            world.notifyBlocksOfNeighborChange(x, currentYIndex, z, lavaid);
            world.scheduledUpdatesAreImmediate = true;
            Block.blocksList[lavaid].updateTick(world, x, currentYIndex, z, random);
            world.scheduledUpdatesAreImmediate = false;
            currentYIndex--;
        }

        return true;
    }

    private void queueBlock(int x, int y, int z, int sides) {
        if (sides <= 0)
            return;

        List sb = Arrays.asList(new Integer[] { x, z });
        Integer o = (Integer) this.closedList.get(sb);

        if ((o != null) && (sides <= o.intValue()))
            return;

        this.openList.addLast(Arrays.asList(new Integer[] { x, y, z }));
        this.closedList.put(sb, sides);
    }

    private void queueNeighboringBlocks(int x, int y, int z, int sides, Random random) {
        queueBlock(x - 1, y, z, random.nextInt(2) > 0 ? sides - 1 : sides);
        queueBlock(x + 1, y, z, random.nextInt(2) > 0 ? sides - 1 : sides);
        queueBlock(x, y, z - 1, random.nextInt(2) > 0 ? sides - 1 : sides);
        queueBlock(x, y, z + 1, random.nextInt(2) > 0 ? sides - 1 : sides);
    }

    public void destroyTree(World world, int x, int y, int z) {
        int bid = world.getBlockId(x, y, z);
        if (bid == Block.snow.blockID) {
            world.setBlock(x, y, z, 0);
            return;
        }
        if ((bid != Block.wood.blockID) && (bid != Block.leaves.blockID) && (bid != Block.vine.blockID))
            return;

        world.setBlock(x, y, z, 0);
        destroyTree(world, x, y + 1, z);
    }

    private boolean canReplaceId(int id) {
        if (id == 0)
            return true;

        if ((id == Block.waterMoving.blockID) || (id == Block.waterStill.blockID) || (id == Block.wood.blockID) || (id == Block.leaves.blockID) || (id == Block.vine.blockID) || (id == Block.snow.blockID) || (id == Block.ice.blockID))
            return true;

        if ((Block.blocksList[id] instanceof BlockFlower))
            return true;

        return false;
    }

}
