package mrtjp.projectred.core.tile;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.part.TMultiPart;
import codechicken.multipart.block.BlockMultiPart;
import codechicken.multipart.block.TileMultiPart;
import codechicken.multipart.util.PartMap;
import mrtjp.projectred.api.IConnectable;
import net.minecraft.block.BlockState;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;

public interface IConnectableTile extends IBlockEventTile, IConnectable {

    //region Position calculations

    default BlockPos posOfInternal() {
        return getBlockPosition();
    }

    default BlockPos posOfStraight(int s) {
        return getBlockPosition().relative(Direction.values()[s]);
    }

    default BlockPos posOfCorner(int s, int edgeRot) {
        return getBlockPosition().relative(Direction.values()[s]).relative(Direction.values()[Rotation.rotateSide(s ^ 1, edgeRot)]);
    }

    default int rotFromStraight(int s, int edgeRot) {
        return Rotation.rotationTo(Rotation.rotateSide(s ^ 1, edgeRot), s ^ 1);
    }

    default int rotFromCorner(int s, int edgeRot) {
        return Rotation.rotationTo(s ^ 1, Rotation.rotateSide(s ^ 1, edgeRot) ^ 1);
    }

    //endregion

    //region Utilities

    default void notifyStraight(int s) {
        BlockPos pos = posOfStraight(s);
        getBlockLevel().neighborChanged(pos, getBlockLevel().getBlockState(pos).getBlock(), posOfInternal());
    }

    default void notifyCorner(int s, int edgeRot) {
        BlockPos pos = posOfCorner(s, edgeRot);
        getBlockLevel().neighborChanged(pos, getBlockLevel().getBlockState(pos).getBlock(), posOfInternal());
    }

    default void notifyConnectedExternals() {
        int smask = 0;
        for (int s = 0; s < 6; s++) {
            if (maskConnects(s)) {
                smask |= 1 << s;
            }
        }
        notifyExternals(smask);
    }

    default void notifyExternals(int mask) {
        int smask = 0;

        for (int s = 0; s < 6; s++) {
            if ((mask & 1 << s) != 0) {

                BlockPos pos = posOfStraight(s);
                BlockState state = getBlockLevel().getBlockState(pos);
                getBlockLevel().neighborChanged(pos, state.getBlock(), posOfInternal());

                for (int s2 = 0; s2 < 6; s2++) {

                    BlockPos pos2 = pos.relative(Direction.values()[s2]);
                    BlockState state2 = getBlockLevel().getBlockState(pos2);

                    if (s2 != (s ^ 1) && (smask & 1 << s2) == 0) {
                        getBlockLevel().neighborChanged(pos2, state2.getBlock(), pos);
                    }
                }

                smask |= 1 << s;
            }
        }
    }

    //endregion

    //region Neighbor acquisitions

    default TMultiPart getStraightCenter(int s) {
        BlockPos pos = posOfStraight(s);
        TileMultiPart tile = BlockMultiPart.getTile(getBlockLevel(), pos);
        if (tile == null) return null;
        return tile.getSlottedPart(6);
    }

    default TMultiPart getStraight(int s, int edgeRot) {
        BlockPos pos = posOfStraight(s);
        TileMultiPart tile = BlockMultiPart.getTile(getBlockLevel(), pos);
        if (tile == null) return null;
        return tile.getSlottedPart(Rotation.rotateSide(s ^ 1, edgeRot));
    }

    default TMultiPart getCorner(int s, int edgeRot) {
        BlockPos pos = posOfCorner(s, edgeRot);
        TileMultiPart tile = BlockMultiPart.getTile(getBlockLevel(), pos);
        if (tile == null) return null;
        return tile.getSlottedPart(s ^ 1);
    }

    //endregion

    //region Connection mask

    /**
     * Full block connection mask
     * <p>
     * 0000 0000 EEEE WWWW SSSS NNNN UUUU DDDD | 00FF FFFF EEEE WWWW SSSS NNNN UUUU DDDD
     * <p>
     * For a full block, you can connect through all 6 sides, with 5 connections on each side. (1 through center, and 4 edges)
     * <p>
     * First 8 nibbles, straight connections, of nibble of 1 << r where r is a Rotation.rotationTo(blockSide, edgeSide)
     * D - Down
     * U - Up
     * N - North
     * S - South
     * W - West
     * E - East
     * F - Straight connections to center part or another full block
     * <p>
     * Second 8 nibbles, corner face connections:
     * D - Down
     * U - Up
     * N - North
     * S - South
     * W - West
     * E - East
     */
    long getConnMap();

    void setConnMap(long connMap);

    default boolean maskConnects(int s) {
        return (getConnMap() & (0xF0000000FL << (s * 4) | 0x1000000L << s)) != 0;
    }

    default boolean maskConnectsStraightCenter(int s) {
        return (getConnMap() & 0x1000000L << s) != 0;
    }

    default boolean maskConnectsStraight(int s, int edgeRot) {
        return (getConnMap() & ((1L << edgeRot) << s * 4)) != 0;
    }

    default boolean maskConnectsCorner(int s, int edgeRot) {
        return (getConnMap() & ((0x100000000L << s * 4) << edgeRot)) != 0;
    }

    //endregion

    //region Connection logic

    default boolean outsideCornerEdgeOpen(int s, int edgeRot) {
        BlockPos pos = posOfInternal().relative(Direction.values()[s]);
        if (getBlockLevel().isEmptyBlock(pos)) return true;

        int side1 = s ^ 1;
        int side2 = Rotation.rotateSide(s ^ 1, edgeRot);
        TileMultiPart t = BlockMultiPart.getTile(getBlockLevel(), pos);
        if (t == null) return false;

        return t.getSlottedPart(side1) == null &&
                t.getSlottedPart(side2) == null &&
                t.getSlottedPart(PartMap.edgeBetween(side1, side2)) == null;
    }

    default boolean discoverStraightCenter(int s) {
        TMultiPart p = getStraightCenter(s);
        if (p instanceof IConnectable) {
            IConnectable connectable = (IConnectable) p;
            return canConnectPart(connectable, s, -1) && connectable.connectStraight(this, s ^ 1, -1);
        }

        return discoverStraightCenterOverride(s);
    }

    default boolean discoverStraight(int s, int edgeRot) {
        TMultiPart p = getStraight(s, edgeRot);
        if (p instanceof IConnectable) {
            IConnectable connectable = (IConnectable) p;
            return canConnectPart(connectable, s, edgeRot) && connectable.connectStraight(this, rotFromStraight(s, edgeRot), -1);
        }

        return false;
    }

    default boolean discoverCorner(int s, int edgeRot) {
        TMultiPart p = getCorner(s, edgeRot);
        if (p instanceof IConnectable) {
            IConnectable connectable = (IConnectable) p;
            return canConnectPart(connectable, s, edgeRot) &&
                    outsideCornerEdgeOpen(s, edgeRot) &&
                    connectable.canConnectCorner(rotFromCorner(s, edgeRot)) && //TODO shouldnt this be handled by next line?
                    connectable.connectCorner(this, rotFromCorner(s, edgeRot), -1);

        }

        return false;
    }

    default boolean discoverStraightCenterOverride(int s) {
        BlockPos pos = posOfStraight(s);
        TileEntity tile = getBlockLevel().getBlockEntity(pos);
        if (tile instanceof IConnectable) {
            IConnectable connectable = (IConnectable) tile;
            return canConnectPart(connectable, s, -1) && connectable.connectStraight(this, s, -1);
        }

        return false;
    }

    default boolean updateExternals() {
        long connMap = 0L;

        for (int s = 0; s < 6; s++) {
            if (discoverStraightCenter(s)) connMap |= 0x1000000L << s;

            for (int edgeRot = 0; edgeRot < 4; edgeRot++) {
                if (discoverStraight(s, edgeRot)) {
                    connMap |= (0x1L << edgeRot) << s * 4;
                }
            }

            for (int edgeRot = 0; edgeRot < 4; edgeRot++) {
                if (discoverCorner(s, edgeRot)) {
                    connMap |= (0x100000000L << edgeRot) << s * 4;
                }
            }
        }

        if (connMap != getConnMap()) {
            setConnMap(connMap);
            onMaskChanged();
            return true;
        }
        return false;
    }

    //endregion

    //region IConnectable implementations

    default boolean connectStraight(IConnectable part, int s, int edgeRot) {
        if (canConnectPart(part, s, edgeRot)) {
            long connMap = getConnMap();

            if (edgeRot > -1) {
                connMap |= (0x1L << edgeRot) << s * 4;
            } else {
                connMap |= 0x1000000L << s * 4;
            }

            if (connMap != getConnMap()) {
                setConnMap(connMap);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }

    default boolean connectCorner(IConnectable part, int s, int edgeRot) {
        if (canConnectPart(part, s, edgeRot)) {
            long connMap = getConnMap();
            connMap |= (0x100000000L << edgeRot) << s * 4;
            if (connMap != getConnMap()) {
                setConnMap(connMap);
                onMaskChanged();
            }
            return true;
        }
        return false;
    }

    default boolean connectInternal(IConnectable part, int r) {
        return false;
    }

    default boolean canConnectCorner(int r) {
        return false;
    }

    default void onMaskChanged() { }

    boolean canConnectPart(IConnectable part, int s, int edgeRot);

    //endregion
}
