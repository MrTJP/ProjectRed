package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.interfaces.wiring.IBundledEmitter;
import mrtjp.projectred.interfaces.wiring.IBundledUpdatable;
import mrtjp.projectred.interfaces.wiring.IInsulatedRedstoneWire;
import mrtjp.projectred.transmission.EnumWire;
import mrtjp.projectred.transmission.RedwirePart;
import mrtjp.projectred.transmission.WirePart;
import mrtjp.projectred.utils.BasicWireUtils;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.world.World;

public class TileInsulatedRedAlloy extends RedwirePart implements IBundledUpdatable {
	
	public TileInsulatedRedAlloy(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
		syncSignalStrength = true;
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		if (wire.getWireType() == EnumWire.RED_ALLOY || wire instanceof TileBundled) {
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
		TileEntity te = world().getBlockTileEntity(x, y, z);
		if (te instanceof IBundledEmitter) {
			int colour = getInsulatedWireColour();
			byte[] bcStrengthArray = ((IBundledEmitter) te).getBundledCableStrength(side, dir);
			if (bcStrengthArray != null) {
				int bcStrength = bcStrengthArray[colour] & 0xFF;
				rv = Math.max(rv, bcStrength);
			}
		}
		return rv;
	}

	@Override
	public void onBundledInputChanged() {
		updateSignal(null);
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
