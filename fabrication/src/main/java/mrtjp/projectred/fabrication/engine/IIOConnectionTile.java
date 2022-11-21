package mrtjp.projectred.fabrication.engine;

public interface IIOConnectionTile {

    /**
     * @return True if this tile is an input
     */
    boolean isInputIOMode();

    /**
     * @return Rotation index (0-3) representing the edge through which this IO signal is passing
     */
    int getIOSide();

    ICConnectionType getConnectionType();
}
