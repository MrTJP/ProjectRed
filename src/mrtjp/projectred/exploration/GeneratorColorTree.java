package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PRColors;
import net.minecraft.block.Block;
import net.minecraft.block.BlockFluid;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.minecraft.world.biome.BiomeGenBase;
import net.minecraftforge.common.IPlantable;

import java.util.Random;

public class GeneratorColorTree
{
    public static final int LUMPY_CHANCE = 10;
    public static final int TALL_CHANCE = 30;

    private final int leafID;


    private final Random rand = new Random();

    public GeneratorColorTree(int leafID)
    {
        this.leafID = leafID;
    }

    public void generateTreeAnyType(World world, int x, int y, int z, PRColors color)
    {
        int val = rand.nextInt(100);
        int chance = LUMPY_CHANCE;
        if (val < chance)
        {
            this.generateTreeWithNodes(world, x, y, z, color);
            return;
        }
        chance += TALL_CHANCE;
        if (val < chance)
        {
            this.generateTreeTall(world, x, y, z, color);
            return;
        }
        this.generateTree(world, x, y, z, color);
    }

    public ItemStack getLogType()
    {
        return new ItemStack(Block.wood, 1, rand.nextInt(4));
    }

    public void generateTree(World world, int x, int y, int z, PRColors color)
    {
        if (canGenerateTree(world, x, z))
        {
            int meta = color.ordinal();
            ItemStack log = this.getLogType();
            int w = 2;
            int h = 5+rand.nextInt(3);

            for (int i = 0; i < h; i++)
            {
                world.setBlock(x, y+i, z, log.itemID, log.getItemDamage(), 3);
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-3, z+j))
                    {
                        world.setBlock(x+i, y+h-3, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-2, z+j))
                    {
                        world.setBlock(x+i, y+h-2, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-1, z+j))
                    {
                        world.setBlock(x+i, y+h-1, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (i*j == 0)
                    {
                        if (this.canGenerateLeavesAt(world, x+i, y+h, z+j))
                        {
                            world.setBlock(x+i, y+h, z+j, leafID, meta, 3);
                        }
                    }
                }
            }
        }
    }

    public void generateTreeTall(World world, int x, int y, int z, PRColors color)
    {
        if (canGenerateTree(world, x, z))
        {
            int h = 10+rand.nextInt(3);
            ItemStack log = this.getLogType();
            int meta = color.ordinal();
            int w = 2;

            for (int i = 0; i < h; i++)
            {
                world.setBlock(x, y+i, z, log.itemID, log.getItemDamage(), 3);
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (i*j == 0)
                        if (this.canGenerateLeavesAt(world, x+i, y+h-8, z+j))
                        {
                            world.setBlock(x+i, y+h-8, z+j, leafID, meta, 3);
                        }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-7, z+j))
                    {
                        world.setBlock(x+i, y+h-7, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (i*j != w*w && i*j != -w*w)
                        if (this.canGenerateLeavesAt(world, x+i, y+h-6, z+j))
                        {
                            world.setBlock(x+i, y+h-6, z+j, leafID, meta, 3);
                        }
                }
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (i*j != w*w && i*j != -w*w)
                        if (this.canGenerateLeavesAt(world, x+i, y+h-5, z+j))
                        {
                            world.setBlock(x+i, y+h-5, z+j, leafID, meta, 3);
                        }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-4, z+j))
                    {
                        world.setBlock(x+i, y+h-4, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (i*j != w*w && i*j != -w*w)
                        if (this.canGenerateLeavesAt(world, x+i, y+h-3, z+j))
                        {
                            world.setBlock(x+i, y+h-3, z+j, leafID, meta, 3);
                        }
                }
            }
            for (int i = -w; i <= w; i++)
            {
                for (int j = -w; j <= w; j++)
                {
                    if (i*j != w*w && i*j != -w*w)
                        if (this.canGenerateLeavesAt(world, x+i, y+h-2, z+j))
                        {
                            world.setBlock(x+i, y+h-2, z+j, leafID, meta, 3);
                        }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (this.canGenerateLeavesAt(world, x+i, y+h-1, z+j))
                    {
                        world.setBlock(x+i, y+h-1, z+j, leafID, meta, 3);
                    }
                }
            }
            for (int i = -1; i <= 1; i++)
            {
                for (int j = -1; j <= 1; j++)
                {
                    if (i*j == 0)
                    {
                        if (this.canGenerateLeavesAt(world, x+i, y+h, z+j))
                        {
                            world.setBlock(x+i, y+h, z+j, leafID, meta, 3);
                        }
                    }
                }
            }
        }
    }

    public void generateTreeWithNodes(World world, int x, int y, int z, PRColors color)
    {
        if (canGenerateTree(world, x, z))
        {
            int h = 8+rand.nextInt(4);
            ItemStack log = this.getLogType();
            int meta = color.ordinal();

            for (int i = 0; i < h; i++)
            {
                world.setBlock(x, y+i, z, log.itemID, log.getItemDamage(), 3);
            }

            for (int i = 1; i < 2; i++)
            {
                int dx = x+i;
                int dy = y+h-2;
                int dz = z;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x-i;
                dz = z;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x;
                dz = z-i;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x;
                dz = z+i;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x+i;
                dy = y+h-6;
                dz = z;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x-i;
                dz = z;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x;
                dz = z-i;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }

                dx = x;
                dz = z+i;
                world.setBlock(dx, dy, dz, log.itemID, log.getItemDamage(), 3);
                for (int j = -1; j <= 1; j++)
                {
                    for (int k = -1; k <= 1; k++)
                    {
                        for (int m = -1; m <= 1; m++)
                        {
                            if (j*k*m == 0 && this.canGenerateLeavesAt(world, dx+j, dy+k, dz+m))
                            {
                                world.setBlock(dx+j, dy+k, dz+m, leafID, meta, 3);
                            }
                        }
                    }
                }
            }

            int dy = y+h-4;
            for (int i = 1; i < 2; i++)
            {
                if (this.canGenerateLeavesAt(world, x+i, dy, z))
                {
                    world.setBlock(x+i, dy, z, leafID, meta, 3);
                }
                if (this.canGenerateLeavesAt(world, x-i, dy, z))
                {
                    world.setBlock(x-i, dy, z, leafID, meta, 3);
                }
                if (this.canGenerateLeavesAt(world, x, dy, z+i))
                {
                    world.setBlock(x, dy, z+i, leafID, meta, 3);
                }
                if (this.canGenerateLeavesAt(world, x, dy, z-i))
                {
                    world.setBlock(x, dy, z-i, leafID, meta, 3);
                }
            }

            dy = y+h;
            for (int k = -1; k <= 1; k++)
            {
                for (int m = -1; m <= 1; m++)
                {
                    if (k*m == 0 && this.canGenerateLeavesAt(world, x+k, dy, z+m))
                    {
                        world.setBlock(x+k, dy, z+m, leafID, meta, 3);
                    }
                }
            }
        }
    }

    private boolean canGenerateLeavesAt(World world, int x, int y, int z)
    {
        return isSoftBlock(world, x, y, z) || world.getBlockId(x, y, z) == Block.leaves.blockID;
    }

    public static boolean canGenerateTree(World world, int x, int z)
    {
        if (world.isRemote)
            return false;

        if (Math.abs(world.provider.dimensionId) == 1)
            return false;

        BiomeGenBase biome = world.getBiomeGenForCoords(x, z);
        if (biome == BiomeGenBase.ocean || biome == BiomeGenBase.frozenOcean)
            return false;

        if (biome == BiomeGenBase.desert || biome == BiomeGenBase.desertHills)
            ;//return false;

        if (biome == BiomeGenBase.mushroomIsland || biome == BiomeGenBase.mushroomIslandShore)
            return false;

        int y = world.getTopSolidOrLiquidBlock(x, z);
        return canGrowAt(world, x, y, z);
    }

    public static boolean isSoftBlock(World w, int x, int y, int z)
    {
        Block b = Block.blocksList[w.getBlockId(x, y, z)];
        return (w.isAirBlock(x, y, z) || b.canBeReplacedByLeaves(w, x, y, z) || b instanceof IPlantable);
    }

    public static boolean canGrowAt(World world, int x, int y, int z)
    {
        int id = world.getBlockId(x, y, z);
        Block b = Block.blocksList[id];

        int id1 = world.getBlockId(x, y-1, z);

        if (id1 != Block.dirt.blockID && id1 != Block.grass.blockID)
            return false;

        if (b instanceof BlockFluid)
            return false;

        for (int i = 0; i < 6; i++)
        {
            if (!GeneratorColorTree.isSoftBlock(world, x, y+i, z) && !(i == 0 && id == ProjectRedExploration.blockStainedSapling().blockID))
                return false;
        }
        return true;
    }
}
