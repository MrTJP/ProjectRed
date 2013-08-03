package mrtjp.projectred.transmission;

import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.util.Icon;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class InsulatedRedAlloyPart extends RedwirePart implements IBundledUpdatable {

	public InsulatedRedAlloyPart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
		syncSignalStrength = true;
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		if (wire.getWireType() == EnumWire.RED_ALLOY || wire instanceof BundledCablePart) {
			return true;
		}
		return wire.getWireType() == getWireType();
	}

	@Override
	public boolean canProvideStrongPowerInDirection(int dir) {
		return false;
	}

	@Override
	public int getInputPowerStrength(int x, int y, int z, int dir, int side, boolean countWires) {
		int rv = BasicWireUtils.getPowerStrength(world(), x, y, z, dir, side, countWires);

		if (rv > 0) {
			return rv;
		}
		
		TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		if (tile != null) {
			TMultiPart te = tile.partMap(side);
			if (te instanceof IBundledEmitter) {
				int colour = getInsulatedWireColour();
				byte[] bcStrengthArray = ((IBundledEmitter)te).getBundledCableStrength(side, dir);
				if (bcStrengthArray != null) {
					int bcStrength = bcStrengthArray[colour] & 0xFF;
					rv = Math.max(rv, bcStrength);
				}
			}
		}
		return rv;
	}

	@Override
	public void onBundledInputChanged() {
		updateSignal(null);
		updateChange();
	}

	public int getInsulatedWireColour() {
		if (getWireType() == null) {
			return 0;
		}
		return getWireType().ordinal() - EnumWire.INSULATED_0.ordinal();
	}

	@Override
	public Icon getSpecialIconForRender() {
		if (getRedstoneSignalStrength() > 0) {
			return this.getWireType().wireSprites[1];
		} else {
			return this.getWireType().wireSprites[0];
		}
	}
}
