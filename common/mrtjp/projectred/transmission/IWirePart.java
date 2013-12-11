package mrtjp.projectred.transmission;

import net.minecraft.world.World;
import codechicken.multipart.TMultiPart;

public interface IWirePart
{
    /**
     * Standard operation procedure, no special propogation rules. The
     * propogator signal may not have increased.
     */
    public static int RISING = 0;
    /**
     * Used when the propogator signal dropped (to 0). Propogation should
     * continue until a rising or constant change is encountered at which point
     * a RISING should be propogated back to this wire.
     */
    public static int DROPPING = 1;
    /**
     * Used when a wire's connection state has changed. Even if the signal
     * remains the same, new connections still need to be recalculated
     */
    public static int FORCE = 2;
    /**
     * Used when the propogator did not change signal, but a new connection may
     * have been established and signal needs recalculating
     */
    public static int FORCED = 3;

    /**
     * Recalculates the signal of this wire and calls the appropriate
     * propogation methods in WirePropogator. DO NOT CALL THIS YOURSELF. Use
     * WirePropogator.propogateTo
     * 
     * @param prev The part which called this propogation (should be connected)
     *            may be null.
     * @param mode One of RISING, DROPPING, FORCE and FORCED specified above
     */
    public void updateAndPropogate(TMultiPart prev, int mode);

    /**
     * Called at the end of a propogation run for partChanged events. Marks the
     * end of a state change for this part.
     */
    public void onSignalUpdate();

    /**
     * @param side The side of this part to test for wire connection. For face
     *            parts, a rotation, or -1 for center. For center parts, a
     *            forgedirection. The special value Integer.MAX_VALUE should
     *            always return true and is used for return signals
     * @return true if the specified side of this block is connected to a 'wire'
     *         where signal should decrease by one.
     */
    public boolean isWireSide(int side);

    /**
     * The world in which this part resides
     * 
     * @return
     */
    public World world();
}
