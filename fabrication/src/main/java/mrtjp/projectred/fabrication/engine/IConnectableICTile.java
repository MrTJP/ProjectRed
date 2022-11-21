package mrtjp.projectred.fabrication.engine;

public interface IConnectableICTile {

    int getConnMask();

    void setConnMask(int connMask);

    IConnectableICTile getTileTowardsDir(int dir);

    default int dirFrom(int dir) {
        return dir^1;
    }

    default boolean maskConnectsToDir(int dir) {
        return (getConnMask() & 1 << dir) != 0;
    }

    default boolean updateConns() {
        int newConn = 0;
        for (int s = 0; s < 6; s++) {
            if (discoverAndConnect(s)) newConn |= 1 << s;
        }
        if (newConn != getConnMask()) {
            setConnMask(newConn);
            onMaskChanged();
            return true;
        }
        return false;
    }

    default boolean discoverAndConnect(int dir) {
        IConnectableICTile tileTowardsDir = getTileTowardsDir(dir);
        if (tileTowardsDir == null) return false;

        return canConnectTo(tileTowardsDir, dir) && tileTowardsDir.connect(this, dirFrom(dir));
    }

    default boolean connect(IConnectableICTile target, int towardsDir) {
        if (canConnectTo(target, towardsDir)) {
            int newConn = getConnMask() | 1 << towardsDir;
            if (newConn != getConnMask()) {
                setConnMask(newConn);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }

    boolean canConnectTo(IConnectableICTile target, int towardsDir);

    void onMaskChanged();
}
