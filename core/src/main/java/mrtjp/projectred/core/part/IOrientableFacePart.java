package mrtjp.projectred.core.part;

import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;

/**
 * Defines a part that affixes to the side of the block, and can optionally be rotated about
 * that face. Contains utility methods for converting between internal and absolute rotations and directions.
 */
public interface IOrientableFacePart {

    int getSide();

    int getRotation();

    void setSide(int s);

    void setRotation(int r);

    default Transformation rotationT() {
        return Rotation.sideOrientation(getSide(), getRotation()).at(Vector3.CENTER);
    }

    default void onOrientationChange() { }

    default int toInternal(int absRot) {
        return (absRot + 6 - getRotation()) % 4;
    }

    default int toAbsolute(int r) {
        return (r + getRotation() + 2) % 4;
    }

    default int absoluteDir(int absRot) {
        return Rotation.rotateSide(getSide(), absRot);
    }

    default int absoluteRot(int absDir) {
        return Rotation.rotationTo(getSide(), absDir);
    }

    default int toInternalMask(int mask) {
        return shiftMask(mask, toInternal(0));
    }

    default int toAbsoluteMask(int mask) {
        return shiftMask(mask, toAbsolute(0));
    }

    static int shiftMask(int mask, int r) {
        return (mask & ~0xF) | (mask << r | mask >> 4 - r) & 0xF;
    }

    static int flipMaskZ(int mask) {
        return mask & 5 | mask << 2 & 8 | mask >> 2 & 2;
    }
}
