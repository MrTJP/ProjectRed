package mrtjp.projectred.multipart.wiring.gates;

/**
 * Implemented by GateLogics which can be used with the timer GUI.
 * Allows access to the timer interval (which is measured in ticks)
 */
public interface GateLogicTimed {
	public int getInterval();
	public void setInterval(int i);
}
