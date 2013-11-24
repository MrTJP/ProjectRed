package mrtjp.projectred.exploration;

import java.util.Random;

import net.minecraft.block.Block;
import net.minecraft.block.BlockSapling;
import net.minecraft.util.Direction;
import net.minecraft.world.World;
import net.minecraft.world.gen.feature.WorldGenerator;
import net.minecraftforge.common.ForgeDirection;

/**
 * Minecraft's tree generator with a few tweaks
 *
 */
public class GeneratorCustomTree extends WorldGenerator {
    private final int minTreeHeight;

    private final boolean vinesGrow;
    private final int vineID;
    private final int vineMeta;

    private final int woodID;
    private final int woodMeta;

    private final int leafID;
    private final int leafMeta;

    public GeneratorCustomTree(boolean growFromSapling, int minHeight, int woodID, int woodMeta, int leafID, int leafMeta, int vineID, int vineMeta) {
        super(growFromSapling);
        minTreeHeight = minHeight;

        this.woodID = woodID;
        this.woodMeta = woodMeta;

        this.leafID = leafID;
        this.leafMeta = leafMeta;

        this.vineID = vineID;
        this.vineMeta = vineMeta;
        vinesGrow = vineID > 0;
    }

    @Override
    public boolean generate(World w, Random r, int x, int y, int z) {
        int l = r.nextInt(3) + this.minTreeHeight;
        boolean flag = true;

        if (y >= 1 && y + l + 1 <= 256) {
            int i1;
            byte b0;
            int j1;
            int k1;

            for (i1 = y; i1 <= y + 1 + l; ++i1) {
                b0 = 1;

                if (i1 == y)
                    b0 = 0;

                if (i1 >= y + 1 + l - 2)
                    b0 = 2;

                for (int l1 = x - b0; l1 <= x + b0 && flag; ++l1)
                    for (j1 = z - b0; j1 <= z + b0 && flag; ++j1)
                        if (i1 >= 0 && i1 < 256) {
                            k1 = w.getBlockId(l1, i1, j1);

                            Block block = Block.blocksList[k1];
                            boolean isAir = w.isAirBlock(l1, i1, j1);

                            if (!isAir && !block.isLeaves(w, l1, i1, j1) && k1 != Block.grass.blockID && k1 != Block.dirt.blockID && !block.isWood(w, l1, i1, j1))
                                flag = false;
                        } else
                            flag = false;
            }

            if (!flag)
                return false;
            else {
                i1 = w.getBlockId(x, y - 1, z);
                Block soil = Block.blocksList[i1];
                boolean isSoil = soil != null && soil.canSustainPlant(w, x, y - 1, z, ForgeDirection.UP, (BlockSapling) Block.sapling);

                if (isSoil && y < 256 - l - 1) {
                    soil.onPlantGrow(w, x, y - 1, z, x, y, z);
                    b0 = 3;
                    byte b1 = 0;
                    int i2;
                    int j2;
                    int k2;

                    for (j1 = y - b0 + l; j1 <= y + l; ++j1) {
                        k1 = j1 - (y + l);
                        i2 = b1 + 1 - k1 / 2;

                        for (j2 = x - i2; j2 <= x + i2; ++j2) {
                            k2 = j2 - x;

                            for (int l2 = z - i2; l2 <= z + i2; ++l2) {
                                int i3 = l2 - z;

                                if (Math.abs(k2) != i2 || Math.abs(i3) != i2 || r.nextInt(2) != 0 && k1 != 0) {
                                    int j3 = w.getBlockId(j2, j1, l2);
                                    Block block = Block.blocksList[j3];

                                    if (block == null || block.canBeReplacedByLeaves( w, j2, j1, l2))
                                        this.setBlockAndMetadata(w, j2, j1, l2, leafID, this.leafMeta);
                                }
                            }
                        }
                    }

                    for (j1 = 0; j1 < l; ++j1) {
                        k1 = w.getBlockId(x, y + j1, z);

                        Block block = Block.blocksList[k1];

                        if (k1 == 0 || block == null || block.isLeaves(w, x, y + j1, z)) {
                            this.setBlockAndMetadata(w, x, y + j1, z, woodID, woodMeta);

                            if (this.vinesGrow && j1 > 0) {
                                if (r.nextInt(3) > 0 && w.isAirBlock(x - 1, y + j1, z))
                                    this.setBlockAndMetadata(w, x - 1, y + j1, z, vineID, 8);

                                if (r.nextInt(3) > 0 && w.isAirBlock(x + 1, y + j1, z))
                                    this.setBlockAndMetadata(w, x + 1, y + j1, z, vineID, 2);

                                if (r.nextInt(3) > 0 && w.isAirBlock(x, y + j1, z - 1))
                                    this.setBlockAndMetadata(w, x, y + j1, z - 1, vineID, 1);

                                if (r.nextInt(3) > 0 && w.isAirBlock(x, y + j1, z + 1))
                                    this.setBlockAndMetadata(w, x, y + j1, z + 1, vineID, 4);
                            }
                        }
                    }

                    if (this.vinesGrow) {
                        for (j1 = y - 3 + l; j1 <= y + l; ++j1) {
                            k1 = j1 - (y + l);
                            i2 = 2 - k1 / 2;

                            for (j2 = x - i2; j2 <= x + i2; ++j2)
                                for (k2 = z - i2; k2 <= z + i2; ++k2) {
                                    Block block = Block.blocksList[w.getBlockId(j2, j1, k2)];
                                    if (block != null && block.isLeaves(w, j2, j1, k2)) {
                                        if (r.nextInt(4) == 0 && w.isAirBlock(j2 - 1, j1, k2))
                                            this.growVines(w, j2 - 1, j1, k2, 8);

                                        if (r.nextInt(4) == 0 && w.isAirBlock(j2 + 1, j1, k2))
                                            this.growVines(w, j2 + 1, j1, k2, 2);

                                        if (r.nextInt(4) == 0 && w.isAirBlock(j2, j1, k2 - 1))
                                            this.growVines(w, j2, j1, k2 - 1, 1);

                                        if (r.nextInt(4) == 0 && w.isAirBlock(j2, j1, k2 + 1))
                                            this.growVines(w, j2, j1, k2 + 1, 4);
                                    }
                                }
                        }

                        if (r.nextInt(5) == 0 && l > 5)
                            for (j1 = 0; j1 < 2; ++j1)
                                for (k1 = 0; k1 < 4; ++k1)
                                    if (r.nextInt(4 - j1) == 0) {
                                        i2 = r.nextInt(3);
                                        this.setBlockAndMetadata(w, x + Direction.offsetX[Direction.rotateOpposite[k1]], y + l - 5 + j1, z + Direction.offsetZ[Direction.rotateOpposite[k1]], Block.cocoaPlant.blockID, i2 << 2 | k1);
                                    }
                    }

                    return true;
                } else
                    return false;
            }
        } else
            return false;
    }

    /**
     * Grows vines downward from the given block for a given length. Args:
     * World, x, starty, z, vine-length
     */
    private void growVines(World par1World, int par2, int par3, int par4, int par5) {
        this.setBlockAndMetadata(par1World, par2, par3, par4, vineID, par5);
        int i1 = 4;

        while (true) {
            --par3;

            if (!par1World.isAirBlock(par2, par3, par4) || i1 <= 0)
                return;

            this.setBlockAndMetadata(par1World, par2, par3, par4, vineID, par5);
            --i1;
        }
    }

}
