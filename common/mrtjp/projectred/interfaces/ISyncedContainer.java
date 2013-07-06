
package mrtjp.projectred.interfaces;


/**
 * Used to check if container supports button packets.
 */
public interface ISyncedContainer {
	
	/**
	 * Called when a button-press packet is received. It's like an action packet
	 * that carries a single int, for convenience (you don't have to make a
	 * packet class to wrap just one int)
	 */
	public void onButtonPressed(int id);
}
