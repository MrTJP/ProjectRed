package mrtjp.projectred.api;

import net.minecraft.world.World;

/**
 * Central API class for ProjectRed
 * If ProjectRed is installed, the instance field will contain an implementor of these methods.
 * <br><br>
 * It is recommended that mods access this class within a soft dependancy class.
 */
public abstract class ProjectRedAPI
{
    public static ProjectRedAPI instance;

    /**
     * Queries the block on side of this block for the bundled signal being emmitted to this block.
     * 
     * @param world The world containing the block
     * @param x The x coordinate of the block/tile querying signal
     * @param y The y coordinate of the block/tile querying signal
     * @param z The z coordinate of the block/tile querying signal
     * @param side The side of the block
     * @return A bundled signal {@link IBundledEmitter}
     */
    public abstract byte[] getBundledInput(World world, int x, int y, int z, int side);
}
