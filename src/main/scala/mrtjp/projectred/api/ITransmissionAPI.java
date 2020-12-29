package mrtjp.projectred.api;

import net.minecraft.util.EnumFacing;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

public interface ITransmissionAPI
{
    /**
     * Used to register a IBundledTileInteraction to allow PR wiring to use
     * your block's signals without having to implement an interface on your
     * tile entity.
     *
     * @param interaction The interaction class you want to register
     */
    void registerBundledTileInteraction(IBundledTileInteraction interaction);

    /**
     * Queries the block on side of this block for the bundled signal being
     * emitted to this block.
     *
     * @param world The world containing the block
     * @param pos The coordinates of the block/tile querying signal
     * @param side The side of the block
     * @return A bundled signal {@link IBundledEmitter}
     */
    byte[] getBundledInput(World world, BlockPos pos, EnumFacing side);

    /**
     * Checks to see if the position specified contains a bundled cable.
     *
     * @param world The world containing the block
     * @param pos The coordinates of the block to check
     * @param side The side to check
     * @return If the given position contains a bundled cable on the given side.
     */
    boolean containsBundledCable(World world, BlockPos pos, EnumFacing side);

    /**
     * Checks to see if there is a framed wire at this coordinate.
     *
     * @param world The world containing the block
     * @param pos The coordinates of the block/tile querying signal
     * @return True if there is a framed wire within the block
     */
    boolean containsFramedWire(World world, BlockPos pos);

    /**
     * Retreives the connection mask of the framed wire at the blockspace
     * if there is one or -1 if there isnt.
     *
     * @param world The world containing the block
     * @param pos The coordinates of the block/tile querying signal
     * @return The connmask within 0x3F for the wire if exists, else -1
     */
    int getFramedWireConnectionMask(World world, BlockPos pos);
}