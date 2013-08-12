package mrtjp.projectred.exploration;

import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockFlower;
import net.minecraft.util.MathHelper;
import net.minecraft.world.World;

public class GeneratorVolcano extends GeneratorOre {

    LinkedList queuedList = new LinkedList();
    HashMap queuedTestList = new HashMap();

    public GeneratorVolcano(int id, int meta, int veinSize) {
        super(id, meta, veinSize);
    }

    @Override
    public boolean generate(World world, Random random, int i, int j, int k) {
        if (world.getBlockId(i, j, k) != Block.lavaStill.blockID) {
            return false;
        }
        int grassHeight = world.getHeightValue(i, k);
        int lavaid = Block.lavaMoving.blockID;

        // Make a tube of volcano from the underground lake to grass level.
        while (canReplaceId(world.getBlockId(i, grassHeight - 1, k))) {
            grassHeight--;
        }
        for (int n = j; n < grassHeight; n++) {
            world.setBlock(i, n, k, lavaid);
            world.setBlock(i - 1, n, k, this.id, meta, 3);
            world.setBlock(i + 1, n, k, this.id, meta, 3);
            world.setBlock(i, n, k - 1, this.id, meta, 3);
            world.setBlock(i, n, k + 1, this.id, meta, 3);
        }

        // Start at the bottom layer above the grass. Each time we go up, make the layer smaller.
        int head = 3;
        int spread = random.nextInt(1);
        int currentYIndex = grassHeight;

        while (this.veinSize > 0) {
            boolean breakOut = false;
            while (this.queuedList.size() == 0) {
                world.setBlock(i, currentYIndex, k, lavaid);
                this.queuedTestList.clear();
                queueNeighboringBlocks(i, currentYIndex, k, head, random);
                currentYIndex++;
                if (currentYIndex > 125) {
                    breakOut = true;
                    break;
                }
            }
            if (breakOut) {
                break;
            }

            Integer[] coord = (Integer[]) ((List) this.queuedList.removeFirst()).toArray();

            world.getBlockId(coord[0].intValue(), 64, coord[2].intValue());
            if (world.blockExists(coord[0].intValue(), 64, coord[2].intValue())) {
                int pow = ((Integer) this.queuedTestList.get(Arrays.asList(new Integer[] { coord[0], coord[2] }))).intValue();
                int currentLevel = world.getHeightValue(coord[0].intValue(), coord[2].intValue()) + 1;
                while ((currentLevel > 0) && (canReplaceId(world.getBlockId(coord[0].intValue(), currentLevel - 1, coord[2].intValue())))) {
                    currentLevel--;
                }
                if (currentLevel <= coord[1].intValue()) {
                    int bid = world.getBlockId(coord[0].intValue(), currentLevel, coord[2].intValue());
                    if (canReplaceId(bid)) {
                        destroyTree(world, coord[0].intValue(), currentLevel, coord[2].intValue());
                        world.setBlock(coord[0].intValue(), currentLevel, coord[2].intValue(), this.id, this.meta, 3);

                        if (coord[1].intValue() > currentLevel) {
                            pow = Math.max(pow, spread);
                        }
                        queueNeighboringBlocks(coord[0].intValue(), currentLevel, coord[2].intValue(), pow, random);
                        this.veinSize -= 1;
                    }
                }
            }
        }

        world.setBlock(i, currentYIndex, k, lavaid);

        while ((currentYIndex > grassHeight) && (world.getBlockId(i, currentYIndex, k) == lavaid)) {
            world.markBlockForUpdate(i, currentYIndex, k);
            world.notifyBlocksOfNeighborChange(i, currentYIndex, k, lavaid);
            world.scheduledUpdatesAreImmediate = true;
            Block.blocksList[lavaid].updateTick(world, i, currentYIndex, k, random);
            world.scheduledUpdatesAreImmediate = false;
            currentYIndex--;
        }

        return true;
    }

    private void queueBlock(int i, int j, int k, int p) {
        if (p <= 0) {
            return;
        }
        List sb = Arrays.asList(new Integer[] { i, k });
        Integer o = (Integer) this.queuedTestList.get(sb);

        if ((o != null) && (p <= o.intValue())) {
            return;
        }
        this.queuedList.addLast(Arrays.asList(new Integer[] { i, j, k }));
        this.queuedTestList.put(sb, p);
    }

    private void queueNeighboringBlocks(int i, int j, int k, int p, Random random) {
        //int rp = random.nextInt(32);
        queueBlock(i - 1, j, k, random.nextInt(2) > 0 ? p - 1 : p);
        queueBlock(i + 1, j, k, random.nextInt(2) > 0 ? p - 1 : p);
        queueBlock(i, j, k - 1, random.nextInt(2) > 0 ? p - 1 : p);
        queueBlock(i, j, k + 1, random.nextInt(2) > 0 ? p - 1 : p);
    }

    public void destroyTree(World world, int i, int j, int k) {
        int bid = world.getBlockId(i, j, k);
        if (bid == Block.snow.blockID) {
            world.setBlock(i, j, k, 0);
            return;
        }
        if ((bid != Block.wood.blockID) && (bid != Block.leaves.blockID) && (bid != Block.vine.blockID)) {
            return;
        }
        world.setBlock(i, j, k, 0);

        destroyTree(world, i, j + 1, k);
    }

    private boolean canReplaceId(int bid) {
        if (bid == 0) {
            return true;
        }
        if ((bid == Block.waterMoving.blockID) || (bid == Block.waterStill.blockID) || (bid == Block.wood.blockID) || (bid == Block.leaves.blockID) || (bid == Block.vine.blockID) || (bid == Block.snow.blockID) || (bid == Block.ice.blockID)) {
            return true;
        }
        if ((Block.blocksList[bid] instanceof BlockFlower)) {
            return true;
        }
        return false;
    }

}
