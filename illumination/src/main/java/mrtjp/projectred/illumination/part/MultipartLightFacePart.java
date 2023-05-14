package mrtjp.projectred.illumination.part;

import codechicken.lib.vec.Rotation;
import codechicken.multipart.api.MultiPartType;
import codechicken.multipart.api.part.TFacePart;
import codechicken.multipart.api.part.redstone.IMaskedRedstonePart;
import mrtjp.projectred.illumination.MultipartLightProperties;

public class MultipartLightFacePart extends MultipartLightPart implements TFacePart, IMaskedRedstonePart {

    public MultipartLightFacePart(MultiPartType<?> type, MultipartLightProperties properties, int color, boolean inverted) {
        super(type, properties, color, inverted);
    }

    @Override
    public boolean solid(int side) {
        return false;
    }

    @Override
    public int redstoneConductionMap() {
        return 0x1F;
    }

    @Override
    public int getConnectionMask(int side) {
        if ((side^1) == getSide()) {
            return 0;
        }

        if (side == getSide()) {
            return 0x10;
        }

        return 1 << Rotation.rotationTo(side & 6, getSide());
    }
}
