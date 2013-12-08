package mrtjp.projectred.exploration;

import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.world.World;
import net.minecraft.world.gen.feature.WorldGenerator;
import codechicken.lib.math.MathHelper;

/**
 * Generic world ore generator, pretty much direct copy from WorldGenMinable
 */
public class GeneratorOre extends WorldGenerator {

    protected int id;
    protected int meta;
    protected int veinSize;

    public GeneratorOre(int id, int meta, int veinSize) {
        this.id = id;
        this.meta = meta;
        this.veinSize = veinSize;
    }

    public void swapStoneBlock(World world, Random random, int x, int y, int z) {
        if (world.getBlockId(x, y, z) == Block.stone.blockID)
            world.setBlock(x, y, z, id, meta, 2);
    }

    @Override
    public boolean generate(World world, Random random, int x, int y, int z) {
        // Randomly space out blocks in vein
        float f = (float) (random.nextFloat() * Math.PI);
        double d0 = x + 8 + MathHelper.sin(f) * this.veinSize / 8.0F;
        double d1 = x + 8 - MathHelper.sin(f) * this.veinSize / 8.0F;
        double d2 = z + 8 + MathHelper.cos(f) * this.veinSize / 8.0F;
        double d3 = z + 8 - MathHelper.cos(f) * this.veinSize / 8.0F;
        double d4 = y + random.nextInt(3) - 2;
        double d5 = y + random.nextInt(3) - 2;

        // Do this once for every block of ore we need to generate.
        for (int l = 0; l <= this.veinSize; l++) {
            double d6 = d0 + (d1 - d0) * l / this.veinSize;
            double d7 = d4 + (d5 - d4) * l / this.veinSize;
            double d8 = d2 + (d3 - d2) * l / this.veinSize;
            double d9 = random.nextDouble() * this.veinSize / 16.0D;
            double d10 = (MathHelper.sin(l * 3.141593F / this.veinSize) + 1.0F) * d9 + 1.0D;
            double d11 = (MathHelper.sin(l * 3.141593F / this.veinSize) + 1.0F) * d9 + 1.0D;
            int i1 = MathHelper.floor_double(d6 - d10 / 2.0D);
            int j1 = MathHelper.floor_double(d7 - d11 / 2.0D);
            int k1 = MathHelper.floor_double(d8 - d10 / 2.0D);
            int l1 = MathHelper.floor_double(d6 + d10 / 2.0D);
            int i2 = MathHelper.floor_double(d7 + d11 / 2.0D);
            int j2 = MathHelper.floor_double(d8 + d10 / 2.0D);
            for (int k2 = i1; k2 <= l1; k2++) {
                double d12 = (k2 + 0.5D - d6) / (d10 / 2.0D);
                if (d12 * d12 < 1.0D)
                    for (int l2 = j1; l2 <= i2; l2++) {
                        double d13 = (l2 + 0.5D - d7) / (d11 / 2.0D);
                        if (d12 * d12 + d13 * d13 < 1.0D)
                            for (int i3 = k1; i3 <= j2; i3++) {
                                double d14 = (i3 + 0.5D - d8) / (d10 / 2.0D);
                                if (d12 * d12 + d13 * d13 + d14 * d14 < 1.0D)
                                    swapStoneBlock(world, random, k2, l2, i3);
                            }
                    }
            }
        }
        return true;
    }

    protected static class Evaluation {
        final int x, y, z;
        int sides;
        public Evaluation(int x, int y, int z, int sides) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.sides = sides;
        }

        public void evaluateSide() {
            sides--;
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof Evaluation) {
                Evaluation e = (Evaluation)o;
                return x == e.x && y == e.y && z == e.z && sides == e.sides;
            }
            return false;
        }
    }

}
