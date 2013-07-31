package mrtjp.projectred.multipart.wiring.wires;

import java.util.Arrays;

import mrtjp.projectred.interfaces.wiring.IBundledEmitter;
import mrtjp.projectred.interfaces.wiring.IBundledUpdatable;
import mrtjp.projectred.interfaces.wiring.IBundledWire;
import mrtjp.projectred.interfaces.wiring.IRedstoneUpdatable;
import mrtjp.projectred.multipart.wiring.CommandDebug;
import mrtjp.projectred.transmission.TileWire;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ChatMessageComponent;
import net.minecraftforge.common.ForgeDirection;

public class TileBundled extends TileWire implements IRedstoneUpdatable, IBundledEmitter, IBundledUpdatable, IBundledWire {

	private byte[] strength = new byte[16];

	@Override
	public void writeToNBT(NBTTagCompound tag) {
		super.writeToNBT(tag);
		tag.setByteArray("strength", strength);
	}

	@Override
	public void readFromNBT(NBTTagCompound tag) {
		super.readFromNBT(tag);
		strength = tag.getByteArray("strength");
	}

	@Override
	protected boolean canConnectToWire(TileWire wire) {
		return super.canConnectToWire(wire) || wire instanceof TileInsulatedRedAlloy || getWireType() == wire.getWireType() || wire.getWireType() == EnumWire.BUNDLED_N || (getWireType() == EnumWire.BUNDLED_N && wire instanceof TileBundled);
	}

	@Override
	public boolean canUpdate() {
		return false;
	}

	private byte[] oldStrength = new byte[16];

	private boolean isUpdating;
	private boolean recursiveUpdatePending = true;

	private void updateStrengthFromBlock(int x, int y, int z, int side, int dir) {
		TileEntity te = worldObj.getBlockTileEntity(x, y, z);

		if (te instanceof TileInsulatedRedAlloy) {
			TileInsulatedRedAlloy o = (TileInsulatedRedAlloy) te;
			int o_strength = o.getRedstoneSignalStrength() - 1;
			int colour = o.getInsulatedWireColour();
			if ((strength[colour] & 0xFF) < o_strength)
				strength[colour] = (byte) o_strength;

		} else if (te instanceof IBundledEmitter) {
			byte[] o_strength = ((IBundledEmitter) te).getBundledCableStrength(side, dir);
			if (o_strength != null) {
				// null = all 0
				for (int k = 0; k < 16; k++) {
					int o_c_strength = (o_strength[k] & 0xFF) - 1;
					if ((strength[k] & 0xFF) < o_c_strength)
						strength[k] = (byte) o_c_strength;
				}
			}
		}
	}

	private void updateStrengthFromSurroundingBlocks() {
		{
			byte[] temp = oldStrength;
			oldStrength = strength;
			strength = temp;
			Arrays.fill(strength, (byte) 0);
		}

		for (int side = 0; side < 6; side++) {
			for (int dir = 0; dir < 6; dir++) {
				if (connectsInDirection(side, dir)) {
					ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
					int x = xCoord + fd.offsetX, y = yCoord + fd.offsetY, z = zCoord + fd.offsetZ;

					int oside = side, odir = dir ^ 1;

					if (connectsInDirectionAroundCorner(side, dir)) {
						fd = ForgeDirection.VALID_DIRECTIONS[side];
						x += fd.offsetX;
						y += fd.offsetY;
						z += fd.offsetZ;
						oside = dir ^ 1;
						odir = side ^ 1;
					}

					updateStrengthFromBlock(x, y, z, oside, odir);
				}
			}

			if (connectsInDirectionByJacketedWire(side)) {
				ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[side];
				int x = xCoord + fd.offsetX, y = yCoord + fd.offsetY, z = zCoord + fd.offsetZ;

				updateStrengthFromBlock(x, y, z, -1, side ^ 1);
			}
		}
	}

	private void updateStrength() {
		if (worldObj.isRemote)
			return;

		if (isUpdating) {
			recursiveUpdatePending = true;
			return;
		}

		if (CommandDebug.WIRE_DEBUG_PARTICLES)
			debugEffect_bonemeal();

		isUpdating = true;

		do {
			recursiveUpdatePending = false;

			updateStrengthFromSurroundingBlocks();

			if (!Arrays.equals(oldStrength, strength)) {
				boolean any_decreased = false;
				for (int k = 0; k < 16; k++) {
					if ((strength[k] & 0xFF) < (oldStrength[k] & 0xFF)) {
						strength[k] = 0;
						any_decreased = true;
					}
				}

				if (any_decreased) {
					updateConnectedThings();
					updateStrengthFromSurroundingBlocks();
				}

				updateConnectedThings();
			}

		} while (recursiveUpdatePending);

		isUpdating = false;
	}

	private void updateConnectedThings() {
		int notifiedSides = 0;

		if (CommandDebug.WIRE_DEBUG_PARTICLES)
			debugEffect_bonemeal();

		for (int side = 0; side < 6; side++) {
			for (int dir = 0; dir < 6; dir++) {
				if (connectsInDirection(side, dir)) {
					ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
					int x = xCoord + fd.offsetX, y = yCoord + fd.offsetY, z = zCoord + fd.offsetZ;

					if (connectsInDirectionAroundCorner(side, dir)) {
						fd = ForgeDirection.VALID_DIRECTIONS[side];
						x += fd.offsetX;
						y += fd.offsetY;
						z += fd.offsetZ;

					} else {
						if ((notifiedSides & (1 << dir)) != 0)
							continue;
						notifiedSides |= 1 << dir;
					}

					TileEntity t = worldObj.getBlockTileEntity(x, y, z);
					if (t instanceof IBundledUpdatable) {
						((IBundledUpdatable) t).onBundledInputChanged();
					}
				}
			}

			if (connectsInDirectionByJacketedWire(side)) {
				if ((notifiedSides & (1 << side)) == 0) {
					notifiedSides |= 1 << side;

					ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[side];
					int x = xCoord + fd.offsetX, y = yCoord + fd.offsetY, z = zCoord + fd.offsetZ;

					TileEntity t = worldObj.getBlockTileEntity(x, y, z);
					if (t instanceof IBundledUpdatable)
						((IBundledUpdatable) t).onBundledInputChanged();
				}
			}
		}
	}

	@Override
	public void onRedstoneInputChanged() {
		updateStrength();
	}

	@Override
	public void onBundledInputChanged() {
		updateStrength();
	}

	@Override
	void onNeighbourBlockChange() {
		super.onNeighbourBlockChange();
		updateStrength();
	}

	@Override
	public byte[] getBundledCableStrength(int blockFace, int direction) {
		return connectsInDirection(blockFace, direction) ? strength : null;
	}

	@Override
	protected boolean debug(EntityPlayer ply) {
		if (!worldObj.isRemote) {
			int[] i = new int[16];
			for (int k = 0; k < 16; k++)
				i[k] = strength[k] & 0xFF;
			ply.sendChatToPlayer(ChatMessageComponent.func_111077_e("Bundled cable strength: " + Arrays.toString(i)));
		}
		return true;
	}
}
