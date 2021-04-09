package mrtjp.projectred.api;

/**
 * Interface for tile entities that emit/receive bundled signal.
 *
 * Also defines a mask for specific areas that a cable can connect to instead of just any
 * edge on the entire face of a side of the receiving block.
 */
public interface IMaskedBundledTile extends IBundledTile
{
    /**
     * A connection mask definition for each side (edges and center)
     *
     * All connection masks are a 5 bit map.
     * The lowest 4 bits correspond to the connection toward the face specified
     * Rotation.rotateSide(side&6, b) where b is the bit index from lowest to highest.
     * Bit 5 corresponds to a center connection.
     *
     * Looking directly towards any face has the following values:
     *
     * 0x01 - Top edge
     * 0x02 - Right edge
     * 0x04 - Bottom edge
     * 0x08 - Left edge
     * 0x10 - Center connection
     *
     * @param side A side of this block.
     * @return The connection mask for the given side.
     */
    int getConnectionMask(int side);
}