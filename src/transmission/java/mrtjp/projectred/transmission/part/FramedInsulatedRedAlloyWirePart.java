package mrtjp.projectred.transmission.part;

import mrtjp.projectred.api.IConnectable;
import mrtjp.projectred.core.CenterLookup;
import mrtjp.projectred.transmission.WireType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;

public class FramedInsulatedRedAlloyWirePart extends FramedRedwirePart implements IInsulatedRedwirePart {

    public FramedInsulatedRedAlloyWirePart(WireType wireType) {
        super(wireType);
    }

    @Override
    public int getInsulatedColour() {
        return getWireType().getColourIdx();
    }

    @Override
    public TextureAtlasSprite getIcon() {
        return getWireType().getTextures().get(getSignal() == 0 ? 0 : 1);
    }

    //region FramedRedwirePart overrides
    @Override
    protected int resolveSignal(CenterLookup lookup) {
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
    public int weakPowerLevel(int side) {
        if (!maskConnects(side)) {
            return 0;
        }
        return super.weakPowerLevel(side);
    }
    //endregion
}
