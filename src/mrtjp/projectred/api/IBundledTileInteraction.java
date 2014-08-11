package mrtjp.projectred.api;

import net.minecraft.world.World;

/**
 * Class used instead of implementing IBundledTile, where it would be very,
 * very inconvienient (such as when PR adds this functionality to other mods).
 * This class externally handles passing signal info from the tile that
 * *should* have implemented IBundledTile to the requesting device.
 *
 * This should be a standalone class that you have to create on the side,
 * you probably dont want this to be implemented on your tile.
 */
public interface IBundledTileInteraction
{
    /**
     * Checks if this interaction can run at the given position.
     * (ie, if the position contains the tile this interaction is meant for)
     *
     * @param world The World
     * @param x The x coordinate
     * @param y The y coordinate
     * @param z The z coordinate
     * @return True if this interaction should be run at the given location
     */
    public boolean isValidInteractionFor(World world, int x, int y, int z);

    /**
     * Checks if the block at the given position can be connected to.
     *
     * @param world The World
     * @param x The x coordinate
     * @param y The y coordinate
     * @param z The z coordinate
     * @param side The side the wire is trying to connect to.
     * @return True if the wire should be allowed to connect to the side.
     */
    public boolean canConnectBundled(World world, int x, int y, int z, int side);

    /**
     * Gets the bundled signal from the tile on the specified side.
     *
     * @param world The World
     * @param x The x coordinate
     * @param y The y coordinate
     * @param z The z coordinate
     * @param side The side we want the signal for.
     * @return The byte array of all the signals.
     */
    public byte[] getBundledSignal(World world, int x, int y, int z, int side);
}