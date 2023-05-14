package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.FaceLookup;
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
        if (lookup.part instanceof IBundledCablePart) {
            IBundledCablePart bundledPart = (IBundledCablePart) lookup.part;
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

        // If side is towards a rotation
        if ((side & 6) != (getSide() & 6)) {
            int r = absoluteRot(side);
            if (!maskConnects(r))
                return 0;
        }
        return super.weakPowerLevel(side);
    }
    //endregion

    @Override
    public int getInsulatedColour() {
        return getWireType().getColourIdx();
    }
}
