package mrtjp.projectred.exploration;

import java.util.LinkedList;
import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.world.World;
import codechicken.lib.vec.BlockCoord;

public class GeneratorMetamorphicCave extends GeneratorOre {

    LinkedList<Evaluation> openList = new LinkedList<Evaluation>();
    LinkedList<Evaluation> closedList = new LinkedList<Evaluation>();

    public static int MAX_DIAMETER = 128;

    public GeneratorMetamorphicCave(int id, int meta, int veinSize) {
        super(id, meta, veinSize);
    }

    @Override
    public boolean generate(World world, Random random, int x, int y, int z) {
        if (world.getBlockId(x, y, z) != 0)
            return false;

        int yIndex = y;

        while (yIndex <= MAX_DIAMETER && world.getBlockId(x, yIndex, z) != Block.stone.blockID)
            yIndex++;

        addBlockForEvaluation(x, yIndex, z, 6);

        while ((openList.size() > 0) && (veinSize > 0)) {
            Evaluation eval = openList.removeFirst();
            checkStoneBlock(world, eval.x, eval.y, eval.z, eval.sides);
        }
        return true;
    }

    private void checkStoneBlock(World world, int x, int y, int z, int sides) {
        if (world.getBlockId(x, y, z) == Block.stone.blockID) {
            world.setBlock(x, y, z, id, meta, 2);
            if (sides > 0)
                evaluateNeighbors(world, x, y, z, sides - 1);
            
            this.veinSize -= 1;
        }
    }

    private boolean isBlockTouchingAir(World w, BlockCoord b) {
        for (int i = 0; i < 6; i++) {
            BlockCoord bc = b.copy().offset(i);
            if (w.getBlockId(bc.x, bc.y, bc.z) == 0)
                return true;
        }
        return false;
    }

    private void evaluateNeighbors(World w, int x, int y, int z, int sides) {
        BlockCoord b = new BlockCoord(x, y, z);
        if (isBlockTouchingAir(w, b))
            sides = 6;
        for (int i = 0; i < 6; i++) {
            BlockCoord bc = b.copy().offset(i);
            addBlockForEvaluation(bc.x, bc.y, bc.z, sides);
        }
    }

    private void addBlockForEvaluation(int x, int y, int z, int sides) {
        Evaluation eval = new Evaluation(x, y, z, sides);
        if (closedList.contains(eval))
            return;
        openList.add(eval);
        closedList.add(eval);
    }
}
