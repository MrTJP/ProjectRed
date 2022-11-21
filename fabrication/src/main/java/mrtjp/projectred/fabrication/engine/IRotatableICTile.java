package mrtjp.projectred.fabrication.engine;

public interface IRotatableICTile {

    int getRotation();

    void setRotation(int r);

    default int toInternalRotation(int absRot) {
        return (absRot + 6 - getRotation()) % 4;
    }

    default int toAbsoluteRotation(int r) {
        return (r + 2 + getRotation()) % 4;
    }

    default int toInternalMask(int absMask) {
        return shiftMask(absMask, toInternalRotation(0));
    }

    default int toAbsoluteMask(int intMask) {
        return shiftMask(intMask, toAbsoluteRotation(0));
    }

    //@formatter:off
    static int rotationToDir(int r) {
        switch (r) {
           case 0: return 3;
            case 1: return 4;
            case 2: return 2;
            case 3: return 5;
            default: return -1;
        }
    }

    static int dirToRotation(int dir) {
        switch (dir) {
            case 2: return 2;
            case 3: return 0;
            case 4: return 1;
            case 5: return 3;
            default: return -1;
        }
    }
    //@formatter:on

    static int dirMaskToRotationMask(int connMask) {
        int rMask = 0;
        for (int r = 0; r < 4; r++) {
            int s = rotationToDir(r);
            if ((connMask & 1 << s) != 0) rMask |= 1 << r;
        }
        return rMask;
    }

    static int shiftMask(int  mask, int r) {
        return (mask & ~0xF) | (mask << r | mask >> 4-r) & 0xF;
    }

    static int flipMaskZ(int mask) {
        return mask & 5 | mask << 2 & 8 | mask >> 2 & 2;
    }
}
