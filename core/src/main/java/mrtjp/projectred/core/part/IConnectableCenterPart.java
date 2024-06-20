package mrtjp.projectred.core.part;

import codechicken.multipart.api.part.MultiPart;
import codechicken.multipart.block.BlockMultipart;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;

import javax.annotation.Nullable;

public interface IConnectableCenterPart extends MultiPart, IConnectablePart {

    //region Neighbor Positions
    default BlockPos posOfStraight(int s) {
        return pos().relative(Direction.values()[s]);
    }
    //endregion

    //region Connectable Neighbor Acquisitions
    default @Nullable IConnectable getStraight(int s) {
        BlockPos pos = pos().relative(Direction.values()[s]);
        MultiPart part = BlockMultipart.getPart(level(), pos, 6);
        return part instanceof IConnectable ? (IConnectable) part : null;
    }

    default @Nullable IConnectable getInternal(int s) {
        MultiPart part = tile().getSlottedPart(s);
        return part instanceof IConnectable ? (IConnectable) part : null;
    }
    //endregion

    boolean canConnectPart(IConnectable part, int dir);

    //region Mask checks
    default boolean maskOpen(int s) {
        return (getConnMap() & 0x1000 << s) != 0;
    }

    default boolean maskConnects(int s) {
        return (getConnMap() & 0x41 << s) != 0;
    }

    default boolean maskConnectsOut(int s) {
        return (getConnMap() & 0x01 << s) != 0;
    }

    default boolean maskConnectsIn(int s) {
        return (getConnMap() & 0x40 << s) != 0;
    }
    //endregion

    //region IConnectable implementations

    @Override
    default boolean connectStraight(IConnectable part, int s, int edgeRot) {
        if (canConnectPart(part, s) && maskOpen(s)) {
            int connMap = getConnMap();
            connMap |= 0x01 << s;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                maskChangeEvent(false, true);
            }
            return true;
        }
        return false;
    }

    @Override
    default boolean connectInternal(IConnectable part, int s) {
        if (canConnectPart(part, s)) {
            int connMap = getConnMap();
            connMap |= 0x40 << s;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                maskChangeEvent(true, false);
            }
            return true;
        }
        return false;
    }

    @Override
    default boolean connectCorner(IConnectable part, int s, int edgeRot) {
        return false;
    }

    @Override
    default boolean canConnectCorner(int r) {
        return false;
    }
    //endregion

    /**
     * Returns true if nothing is blocking a connection to an external part on side s
     */
    boolean discoverOpen(int s);

    default boolean discoverStraight(int s) {
        IConnectable c = getStraight(s);
        if (c != null) {
            return canConnectPart(c, s) && c.connectStraight(this, s ^ 1, -1);
        }
        return discoverStraightOverride(s);
    }

    default boolean discoverInternal(int s) {
        IConnectable c = getInternal(s);
        if (c != null) {
            return canConnectPart(c, s) && c.connectInternal(this, -1);
        }
        return discoverInternalOverride(s);
    }

    default boolean discoverStraightOverride(int s) {
        return false;
    }

    default boolean discoverInternalOverride(int s) {
        return false;
    }

    @Override
    default boolean updateOpenConns() {
        int connMap = getConnMap() & ~0x3F000;
        for (int s = 0; s < 6; s++) {
            if (discoverOpen(s)) {
                connMap |= 0x1000 << s;
            }
        }
        if (connMap != getConnMap()) {
            setConnMap(connMap);
            return true;
        }
        return false;
    }

    @Override
    default boolean updateExternalConns() {
        int connMap = getConnMap() & ~0x3F;
        for (int s = 0; s < 6; s++) {
            if (maskOpen(s) && discoverStraight(s)) {
                connMap |= 0x01 << s;
            }
        }
        if (connMap != getConnMap()) {
            setConnMap(connMap);
            return true;
        }
        return false;
    }

    @Override
    default boolean updateInternalConns() {
        int connMap = getConnMap() & ~0xFC0;
        for (int s = 0; s < 6; s++) {
            if (discoverInternal(s)) {
                connMap |= 0x40 << s;
            }
        }
        if (connMap != getConnMap()) {
            setConnMap(connMap);
            return true;
        }
        return false;
    }

    @Override
    default void notifyAllExternals() {
        notifyExternals(0x3F);
    }

    default void notifyExternals(int mask) {
        for (int s = 0; s < 6; s++) {
            if ((mask & 1 << s) != 0) {
                if (maskConnectsOut(s)) {
                    notifyStraight(s);
                }
            }
        }
    }

    default void notifyStraight(int r) {
        BlockPos pos = posOfStraight(r);
        level().neighborChanged(pos, tile().getBlockState().getBlock(), pos);
    }
}
