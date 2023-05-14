package mrtjp.projectred.core.part;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.block.BlockMultiPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartMap;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * Default implementation of Connectable face part. Manages connection mask and facilitates connections with
 * other IConnectables.
 * <p>
 * Connection map split into 6 nybbles: (from lowest)
 * <p>
 * 0000 0000 FFFF 000E DDDD CCCC BBBB AAAA
 * <p>
 * A = Corner connections (this wire should connect around a corner to something external)
 * <p>
 * B = Straight connections (this wire should connect to something external)
 * <p>
 * C = Internal connections (this wire should connect to something internal)
 * <p>
 * D = Open sides (this wire is not blocked by a cover/edge part and *could* connect through side)
 * <p>
 * E = connection to the centerpart
 * <p>
 * F = Render corner connections. Like corner connections but set to low if the other wire part is smaller than this (they render to us not us to them)
 */
public interface IConnectableFacePart extends IConnectablePart, IOrientableFacePart {

    //region Trait fields
    BlockPos getPos();

    World getLevel();

    TileMultiPart getTile();
    //endregion

    //region Neighbor Positions
    default BlockPos posOfCorner(int r) {
        return getPos()
                .relative(Direction.values()[absoluteDir(r)])
                .relative(Direction.values()[getSide()]);
    }

    default BlockPos posOfStraight(int r) {
        return getPos().relative(Direction.values()[absoluteDir(r)]);
    }

    default int rotFromCorner(int r) {
        return Rotation.rotationTo(absoluteDir(r) ^ 1, getSide() ^ 1);
    }

    default int rotFromStraight(int r) {
        return (r + 2) % 4;
    }

    default int rotFromInternal(int r) {
        return Rotation.rotationTo(absoluteDir(r), getSide());
    }
    //endregion

    //region Connectable Neighbor Acquisitions
    default IConnectable getCorner(int r) {
        int absDir = absoluteDir(r);
        BlockPos pos = getPos()
                .relative(Direction.values()[absDir])
                .relative(Direction.values()[getSide()]);

        TMultiPart part = BlockMultiPart.getPart(getLevel(), pos, absDir ^ 1);
        return part instanceof IConnectable ? (IConnectable) part : null;
    }

    default IConnectable getStraight(int r) {
        BlockPos pos = getPos().relative(Direction.values()[absoluteDir(r)]);
        TMultiPart part = BlockMultiPart.getPart(getLevel(), pos, getSide());
        return part instanceof IConnectable ? (IConnectable) part : null;
    }

    default IConnectable getInternal(int r) {
        TMultiPart part = getTile().getSlottedPart(absoluteDir(r));
        return part instanceof IConnectable ? (IConnectable) part : null;
    }

    default IConnectable getCenter() {
        TMultiPart part = getTile().getSlottedPart(6);
        return part instanceof IConnectable ? (IConnectable) part : null;
    }
    //endregion

    boolean canConnectPart(IConnectable part, int dir);

    //region Mask checks

    /**
     * Should always return true if this is a logic part for example, where
     * mask is always open, because edges are all the way on the side,
     * so strips cant block conns.
     */
    default boolean maskOpen(int r) {
        return (getConnMap() & 0x1000 << r) != 0;
    }

    default boolean maskConnects(int r) {
        return (getConnMap() & 0x111 << r) != 0;
    }

    default boolean maskConnectsCorner(int r) {
        return (getConnMap() & 1 << r) != 0;
    }

    default boolean maskConnectsStraight(int r) {
        return (getConnMap() & 0x10 << r) != 0;
    }

    default boolean maskConnectsInside(int r) {
        return (getConnMap() & 0x100 << r) != 0;
    }

    default boolean maskConnectsCenter() {
        return (getConnMap() & 0x10000) != 0;
    }
    //endregion

    //region IConnectable implementations
    @Override
    default boolean connectCorner(IConnectable part, int r, int edgeRot) {
        if (canConnectPart(part, r) && maskOpen(r)) {
            int connMap = getConnMap();
            connMap |= 1 << r;
            if (setRenderFlag(part)) connMap |= 0x100000 << r;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }

    @Override
    default boolean connectStraight(IConnectable part, int r, int edgeRot) {
        if (canConnectPart(part, r) && maskOpen(r)) {
            int connMap = getConnMap();
            connMap |= 0x10 << r;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }

    @Override
    default boolean connectInternal(IConnectable part, int r) {
        if (canConnectPart(part, r)) {
            int connMap = getConnMap();
            connMap |= 0x100 << r;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }
    //endregion

    /**
     * If this is a wire, should return true if this wire is smaller
     * than that wire. This is used for corner rendering. (This wire renders
     * to that wire, not that wire to this wire). Always false if this is not a wire.
     * <p>
     * Only one of the parts (either this or that) must return true.
     *
     * @param part The part to connect to
     * @return true if this should render instead of that
     */
    boolean setRenderFlag(IConnectable part);

    default boolean outsideCornerEdgeOpen(int r) {
        int absDir = absoluteDir(r);
        BlockPos pos = getPos().relative(Direction.values()[absDir]);
        if (getLevel().isEmptyBlock(pos)) return true;

        int side1 = absDir ^ 1;
        int side2 = getSide();

        TileMultiPart t = BlockMultiPart.getTile(getLevel(), pos);
        if (t == null) return false; // Non-multipart block is here. We cant go through it.

        // Tile may have parts, but at least all slots that take up this edge must be empty.
        return t.getSlottedPart(side1) == null && t.getSlottedPart(side2) == null &&
                t.getSlottedPart(PartMap.edgeBetween(side1, side2)) == null;
    }

    default boolean insideCornerEdgeOpen(int r) {
        return getTile().getSlottedPart(PartMap.edgeBetween(absoluteDir(r), getSide())) == null;
    }

    /**
     * Checks if an external connection towards r can be made. This should check if an existing face part
     * on the perpendicular face towards r is blocking this part from reaching straight outside.
     * <p>
     * If this part takes up the whole face such that no part can be placed on the perpendicular face towards r,
     * this will always return true.
     *
     * @param r The rotation towards the external pert being queried
     * @return True if connection can be made
     */
    boolean discoverOpen(int r);

    /**
     * Tries to locate a corner connectable part towards r and establish a connection.
     *
     * @param r The rotation towards the external corner part
     * @return True if connection was established
     */
    default int discoverCorner(int r) {
        if (!outsideCornerEdgeOpen(r)) return 0;
        IConnectable c = getCorner(r);
        if (c != null) {
            if ((c.canConnectCorner(rotFromCorner(r)) || canConnectCorner(r)) && canConnectPart(c, r) && c.connectCorner(this, rotFromCorner(r), -1)) {
                return setRenderFlag(c) ? 2 : 1;
            }
            return 0;
        }
        return discoverCornerOverride(absoluteDir(r)) ? 2 : 0;
    }

    default boolean discoverStraight(int r) {
        IConnectable c = getStraight(r);
        if (c != null) {
            return canConnectPart(c, r) && c.connectStraight(this, rotFromStraight(r), -1);
        }
        return discoverStraightOverride(absoluteDir(r));
    }

    default boolean discoverInternal(int r) {
        if (!insideCornerEdgeOpen(r)) return false;
        IConnectable c = getInternal(r);
        if (c != null) {
            return canConnectPart(c, r) && c.connectInternal(this, rotFromInternal(r));
        }
        return discoverInternalOverride(r);
    }

    default boolean discoverCenter() {
        IConnectable c = getCenter();
        if (c != null) {
            return canConnectPart(c, -1) && c.connectInternal(this, -1);
        }
        return false;
    }

    default boolean shouldDiscoverCenter() { return true; }

    default boolean discoverCornerOverride(int absDir) { return false; }

    default boolean discoverStraightOverride(int absDir) { return false; }

    default boolean discoverInternalOverride(int r) { return false; }

    @Override
    default boolean updateOpenConns() {
        int connMap = getConnMap() & ~0xF000;
        for (int r = 0; r < 4; r++) {
            if (discoverOpen(r)) { connMap |= 0x1000 << r; }
        }
        if (connMap != getConnMap()) {
            setConnMap(connMap);
            return true;
        }
        return false;
    }

    @Override
    default boolean updateExternalConns() {
        int connMap = getConnMap() & ~0xF000FF;
        for (int r = 0; r < 4; r++) {
            if (!maskOpen(r)) continue;
            if (discoverStraight(r)) {
                connMap |= 0x10 << r;
            } else {
                int cornerMode = discoverCorner(r);
                if (cornerMode != 0) {
                    connMap |= 1 << r;
                    if (cornerMode == 2) connMap |= 0x100000 << r;
                }
            }
        }
        if (connMap != getConnMap()) {
            int diff = connMap ^ getConnMap();
            setConnMap(connMap);
            // Notify corners, normal block updates won't touch them
            for (int r = 0; r < 4; r++) {
                if ((diff & (1 << r)) != 0) { notifyCorner(r); }
            }
            return true;
        }
        return false;
    }

    @Override
    default boolean updateInternalConns() {
        int connMap = getConnMap() & ~0x10F00;
        for (int r = 0; r < 4; r++) {
            if (discoverInternal(r)) { connMap |= 0x100 << r; }
        }
        if (shouldDiscoverCenter() && discoverCenter()) connMap |= 0x10000;
        if (connMap != getConnMap()) {
            setConnMap(connMap);
            return true;
        }
        return false;
    }

    @Override
    default void notifyAllExternals() {
        notifyExternals(0xF);
    }

    default void notifyExternals(int mask) {
        for (int r = 0; r < 4; r++) {
            if ((mask & (1 << r)) != 0) {
                if (maskConnectsCorner(r)) {
                    notifyCorner(r);
                } else if (maskConnectsStraight(r)) {
                    notifyStraight(r);
                }
            }
        }
    }

    default void notifyCorner(int r) {
        BlockPos pos = posOfCorner(r);
        getLevel().neighborChanged(pos, getTile().getBlockState().getBlock(), pos);
    }

    default void notifyStraight(int r) {
        BlockPos pos = posOfStraight(r);
        getLevel().neighborChanged(pos, getTile().getBlockState().getBlock(), pos);
    }
}
