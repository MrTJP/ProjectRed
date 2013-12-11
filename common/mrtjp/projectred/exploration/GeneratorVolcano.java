package mrtjp.projectred.exploration;

import java.util.LinkedList;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockFlower;
import net.minecraft.world.World;

public class GeneratorVolcano extends GeneratorOre
{
    LinkedList<Evaluation> openList = new LinkedList<Evaluation>();
    LinkedList<Evaluation> closedList = new LinkedList<Evaluation>();

    public GeneratorVolcano(int id, int meta, int veinSize)
    {
        super(id, meta, veinSize);
    }

    @Override
    public boolean generate(World w, Random rand, int x, int y, int z)
    {
        if (w.getBlockId(x, y, z) != Block.lavaStill.blockID)
            return false;

        int grass = makeLavaTube(w, x, y, z);

        int head = 3;
        int spread = rand.nextInt(1);
        int yIndex = grass;

        while (veinSize > 0)
        {
            boolean reachedTop = false;
            while (this.openList.size() == 0)
            {
                w.setBlock(x, yIndex, z, Block.lavaMoving.blockID);
                closedList.clear();
                evaluateNeighbors(x, yIndex, z, head, rand);
                yIndex++;
                if (yIndex > 125)
                {
                    reachedTop = true;
                    break;
                }
            }
            if (reachedTop)
                break;

            Evaluation nextEval = openList.removeFirst();

            if (w.blockExists(nextEval.x, 64, nextEval.z))
            {
                int pow = getClosedEval(nextEval.x, nextEval.z).sides;
                int evalLevel = w.getHeightValue(nextEval.x, nextEval.z);
                while (evalLevel > 0 && isUnimportant(w.getBlockId(nextEval.x, evalLevel - 1, nextEval.z)))
                    evalLevel--;

                if (evalLevel <= nextEval.y)
                    if (isUnimportant(w.getBlockId(nextEval.x, evalLevel, nextEval.z)))
                    {
                        purgeArea(w, nextEval.x, evalLevel, nextEval.z);
                        w.setBlock(nextEval.x, evalLevel, nextEval.z, this.id, this.meta, 3);
                        if (nextEval.y > evalLevel)
                            pow = Math.max(pow, spread);

                        evaluateNeighbors(nextEval.x, evalLevel, nextEval.z, pow, rand);
                        this.veinSize -= 1;
                    }
            }
        }

        // Make everything flow
        w.setBlock(x, yIndex, z, Block.lavaStill.blockID);
        while (yIndex > grass && w.getBlockId(x, yIndex, z) == Block.lavaStill.blockID)
        {
            w.markBlockForUpdate(x, yIndex, z);
            w.notifyBlocksOfNeighborChange(x, yIndex, z, Block.lavaStill.blockID);
            w.scheduledUpdatesAreImmediate = true;
            Block.lavaStill.updateTick(w, x, yIndex, z, rand);
            w.scheduledUpdatesAreImmediate = false;
            yIndex--;
        }
        return true;
    }

    public void purgeArea(World world, int x, int y, int z)
    {
        int center = world.getBlockId(x, y, z);
        if (center == 0)
            return;
        for (int i = -1; i <= 1; i++)
            for (int j = -1; j <= 1; j++)
            {
                int block = world.getBlockId(x + i, y, z + j);
                if (block == Block.snow.blockID)
                {
                    world.setBlock(x + i, y, z + j, 0);
                    continue;
                }
                if (block != Block.wood.blockID && block != Block.leaves.blockID && block != Block.vine.blockID)
                    continue;
                world.setBlock(x + i, y, z + j, 0);
            }
        purgeArea(world, x, y + 1, z);
    }

    private Evaluation getClosedEval(int x, int z)
    {
        for (Evaluation e : closedList)
            if (e.x == x && e.z == z)
                return e;
        return null;
    }

    /**
     * Add block to the A* open list and closed list, with the number of future
     * sides to evaluate.
     */
    private void addBlockForEvaluation(int x, int y, int z, int sides)
    {
        if (sides <= 0)
            return;

        Evaluation eval = getClosedEval(x, z);
        if (eval != null && sides <= eval.sides)
            return;
        Evaluation newEval = new Evaluation(x, y, z, sides);
        openList.addLast(newEval);
        closedList.add(newEval);
    }

    /**
     * Queue all surrounding blocks to the A* open list.
     */
    private void evaluateNeighbors(int x, int y, int z, int sides, Random random)
    {
        addBlockForEvaluation(x - 1, y, z, random.nextInt(2) > 0 ? sides - 1 : sides);
        addBlockForEvaluation(x + 1, y, z, random.nextInt(2) > 0 ? sides - 1 : sides);
        addBlockForEvaluation(x, y, z - 1, random.nextInt(2) > 0 ? sides - 1 : sides);
        addBlockForEvaluation(x, y, z + 1, random.nextInt(2) > 0 ? sides - 1 : sides);
    }

    /**
     * Makes a tube of lava from the underground lake to predicted grass level
     * of the area. Returns the y of the very top of the tube (grass level).
     */
    private int makeLavaTube(World w, int x, int y, int z)
    {
        int grassHeight = w.getHeightValue(x, z);
        int lavaid = Block.lavaMoving.blockID;
        while (isUnimportant(w.getBlockId(x, grassHeight - 1, z)))
            grassHeight--;

        for (int i = y; i < grassHeight; i++)
        {
            w.setBlock(x, i, z, lavaid);
            w.setBlock(x - 1, i, z, this.id, meta, 3);
            w.setBlock(x + 1, i, z, this.id, meta, 3);
            w.setBlock(x, i, z - 1, this.id, meta, 3);
            w.setBlock(x, i, z + 1, this.id, meta, 3);
        }
        return grassHeight;
    }

    /**
     * Define what blocks to eat up.
     */
    private boolean isUnimportant(int id)
    {
        if (id == 0)
            return true;

        if (id == Block.waterMoving.blockID || id == Block.waterStill.blockID || id == Block.wood.blockID || id == Block.leaves.blockID || id == Block.vine.blockID || id == Block.snow.blockID || id == Block.ice.blockID)
            return true;

        if (Block.blocksList[id] instanceof BlockFlower)
            return true;

        return false;
    }

}
