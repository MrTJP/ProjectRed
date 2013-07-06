package mrtjp.projectred.multipart.wiring.wires;

import mrtjp.projectred.interfaces.wiring.IBundledEmitter;
import mrtjp.projectred.interfaces.wiring.IBundledUpdatable;
import mrtjp.projectred.interfaces.wiring.IInsulatedRedstoneWire;
import mrtjp.projectred.utils.BasicWireUtils;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;

public class TileInsulatedRedAlloy extends TileRedAlloy implements IBundledUpdatable, IInsulatedRedstoneWire {
	@Override
	protected boolean canConnectToWire(TileWire wire) {
		if (wire.getType() == EnumWire.RED_ALLOY || wire.getType() == EnumWire.BUNDLED) {
			return true;
		}
		return wire.getType() == getType();
	}

	@Override
	public boolean canProvideWeakPowerInDirection(int dir) {
		// return connectsInDirection(dir) &&
		// super.canProvideWeakPowerInDirection(dir);
		return false;
	}

	@Override
	public boolean canProvideStrongPowerInDirection(int dir) {
		return false;
	}

	@Override
	protected int getInputPowerStrength(World worldObj, int x, int y, int z, int dir, int side, boolean countWires) {
		int rv = BasicWireUtils.getPowerStrength(worldObj, x, y, z, dir, side, countWires);

		if (rv > 0)
			return rv; // no block emits bundled and normal power from the same
						// side

		TileEntity te = worldObj.getBlockTileEntity(x, y, z);
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

	@Override
	public int getInsulatedWireColour() {
		if (getType() == null) {
			return 0;
		}
		return getType().ordinal() - EnumWire.INSULATED_0.ordinal();
	}
}
