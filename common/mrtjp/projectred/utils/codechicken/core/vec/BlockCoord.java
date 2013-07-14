package mrtjp.projectred.utils.codechicken.core.vec;

import net.minecraft.tileentity.TileEntity;
import codechicken.core.alg.MathHelper;

public class BlockCoord implements Comparable<BlockCoord>
{
    public int x;
    public int y;
    public int z;
    
    public BlockCoord(int x, int y, int z)
    {
        this.x = x;
        this.y = y;
        this.z = z;
    }
    
    public BlockCoord(Vector3 v)
    {
        this(MathHelper.floor_double(v.x), MathHelper.floor_double(v.y), MathHelper.floor_double(v.z));
    }
    
    public BlockCoord(TileEntity tile)
    {
        this(tile.xCoord, tile.yCoord, tile.zCoord);
    }

    public BlockCoord(int[] ia)
    {
        this(ia[0], ia[1], ia[2]);
    }
    
    public BlockCoord()
    {
    }

    @Override
    public boolean equals(Object obj)
    {
        if(!(obj instanceof BlockCoord))
            return false;
        BlockCoord o2 = (BlockCoord)obj;
        return x == o2.x && y == o2.y && z == o2.z;
    }
    
    @Override
    public int hashCode()
    {
        return (x^z)*31 + y;
    }

    public int compareTo(BlockCoord o)
    {
        if(x != o.x)return x < o.x ? 1 : -1;
        if(y != o.y)return y < o.y ? 1 : -1;
        if(z != o.z)return z < o.z ? 1 : -1;
        return 0;
    }

    public Vector3 toVector3Centered()
    {
        return new Vector3(x+0.5, y+0.5, z+0.5);
    }

    public BlockCoord multiply(int i)
    {
        x*=i;
        y*=i;
        z*=i;
        return this;
    }

    public double mag_()
    {
        return Math.sqrt(x*x+y*y+z*z);
    }
    
    public int mag2()
    {
        return x*x+y*y+z*z;
    }

    public boolean isZero()
    {
        return x == 0 && y == 0 && z == 0;
    }

    public boolean isAxial()
    {
        return x == 0 ? (y == 0 || z == 0) : (y == 0 && z == 0);
    }
    
    public BlockCoord add(BlockCoord coord2)
    {
        x+=coord2.x;
        y+=coord2.y;
        z+=coord2.z;
        return this;
    }

    public BlockCoord add(int i, int j, int k)
    {
        x+=i;
        y+=j;
        z+=k;
        return this;
    }
    
    public BlockCoord sub(BlockCoord coord2)
    {
        x-=coord2.x;
        y-=coord2.y;
        z-=coord2.z;
        return this;
    }

    public BlockCoord sub(int i, int j, int k)
    {
        x-=i;
        y-=j;
        z-=k;
        return this;
    }
    
    public BlockCoord offset(int side)
    {
        return offset(side, 1);
    }

    public BlockCoord offset(int side, int amount)
    {
        BlockCoord offset = sideOffsets[side];
        x+=offset.x*amount;
        y+=offset.y*amount;
        z+=offset.z*amount;
        return this;
    }
    
    public BlockCoord inset(int side)
    {
        return inset(side, 1);
    }

    public BlockCoord inset(int side, int amount)
    {
        return offset(side, -amount);
    }
    
    public static final BlockCoord[] sideOffsets = new BlockCoord[]{
        new BlockCoord( 0,-1, 0),
        new BlockCoord( 0, 1, 0),
        new BlockCoord( 0, 0,-1),
        new BlockCoord( 0, 0, 1),
        new BlockCoord(-1, 0, 0),
        new BlockCoord( 1, 0, 0)};

    public int[] intArray()
    {
        return new int[]{x, y, z};
    }

    public BlockCoord copy()
    {
        return new BlockCoord(x, y, z);
    }

    public BlockCoord set(int i, int j, int k)
    {
        x = i;
        y = j;
        z = k;
        return this;
    }

    public BlockCoord set(BlockCoord coord)
    {
        return set(coord.x, coord.y, coord.z);
    }
}
