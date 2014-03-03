package mrtjp.projectred.transportation;

/**
 * Interface implemented by things to connect to various types of pipes.
 */
public interface IPipeConnectable
{
    /**
     * Called to check whether a tube part can connect to this. If a part
     * returns true it is expected to immediately reflect the fact that it is
     * now connected to pipe.
     *
     * @param pipe The part asking for connection.
     * @param fromAbsDir The absolute direction to connect from.
     * @return True to allow the tube connection.
     */
    public boolean connect(IPipeConnectable pipe, int fromAbsDir);

    public boolean canConnectTo(IPipeConnectable pipe);
}
