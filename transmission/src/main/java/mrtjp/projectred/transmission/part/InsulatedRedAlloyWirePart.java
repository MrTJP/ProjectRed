package mrtjp.projectred.transmission.part;

import codechicken.lib.vec.Rotation;
import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.FaceLookup;
import mrtjp.projectred.core.part.IConnectableFacePart;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;

public class InsulatedRedAlloyWirePart extends RedwirePart implements IInsulatedRedwirePart {

    public InsulatedRedAlloyWirePart(WireType wireType) {
        super(wireType);
    }

    //region RedwirePart overrides
    @Override
    public TextureAtlasSprite getIcon() {
        return getWireType().getTextures().get(getSignal() == 0 ? 0 : 1);
    }

    @Override
    protected boolean powerUnderside() {
        return false;
    }

    @Override
    protected int resolveSignal(FaceLookup lookup) {
        if (lookup.part instanceof IBundledCablePart bundledPart) {
            return (bundledPart.getBundledSignal()[getInsulatedColour()] & 0xFF) - 1;
        }
        return super.resolveSignal(lookup);
    }
    //endregion

    //region IConnectable overrides
    @Override
    public boolean canConnectPart(IConnectable part, int dir) {
        if (part instanceof IBundledCablePart) {
            return true;
        } else if (part instanceof IInsulatedRedwirePart) {
            return ((IInsulatedRedwirePart) part).getInsulatedColour() == getInsulatedColour();
        }
        return super.canConnectPart(part, dir);
    }
    //endregion

    //region IRedstonePart overrides
    @Override
    public int strongPowerLevel(int side) {
        return 0;
    }

    @Override
    public int weakPowerLevel(int side) {
        // Can't power above or below
        if (side == getSide() || side == (getSide() ^ 1))
            return 0;

        // Can't power unconnected sides (unlike uninsulated)
        int r = IConnectableFacePart.absoluteRot(this, side);
        if (!maskConnects(r))
            return 0;

        return super.weakPowerLevel(side);
    }
    //endregion

    @Override
    public int getInsulatedColour() {
        return getWireType().getColourIdx();
    }
}
