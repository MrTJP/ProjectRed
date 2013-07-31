package mrtjp.projectred.multipart.wiring.wires;

import java.lang.reflect.Field;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.interfaces.wiring.IRedstoneEmitter;
import mrtjp.projectred.interfaces.wiring.IRedstoneUpdatable;
import mrtjp.projectred.interfaces.wiring.IRedstoneWire;
import mrtjp.projectred.multipart.wiring.CommandDebug;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.ChatMessageComponent;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class TileRedAlloy extends TileWire implements IRedstoneEmitter, IRedstoneWire {
	private short MAX_STRENGTH = 255;

	private short strength;
	private short strengthFromNonWireBlocks; // this is synced to the client,
												// not used by the server

	protected boolean syncSignalStrength;
	protected boolean connectToBlockBelow;

	private boolean isUpdatingStrength, recursiveUpdatePending;

	private static boolean dontEmitPower = false;

	public TileRedAlloy() {
	}

	private int updateStrengthFromBlock(int x, int y, int z, int odir, int testside, int newStrength) {
		int thisStrength = this.getInputPowerStrength(worldObj, x, y, z, odir, testside, true);
		newStrength = Math.max(newStrength, Math.min(thisStrength - 1, MAX_STRENGTH));

		if (!worldObj.isRemote) {
			thisStrength = this.getInputPowerStrength(worldObj, x, y, z, odir, testside, false);
			strengthFromNonWireBlocks = (short) Math.max(strengthFromNonWireBlocks, Math.min(thisStrength - 1, MAX_STRENGTH));
		}

		return newStrength;
	}

	private int getStrengthFromSurroundingBlocks() {

		if (!worldObj.isRemote)
			strengthFromNonWireBlocks = 0;

		int newStrength = 0;
		for (int side = 0; side < 6; side++) {

			if (connectsInDirectionByJacketedWire(side)) {
				ForgeDirection dir = ForgeDirection.VALID_DIRECTIONS[side];
				int x = xCoord + dir.offsetX;
				int y = yCoord + dir.offsetY;
				int z = zCoord + dir.offsetZ;
				newStrength = updateStrengthFromBlock(x, y, z, side ^ 1, -1, newStrength);
			}

			if (!isWirePresent(side))
				continue;

			if (connectToBlockBelow) {
				ForgeDirection wdir = ForgeDirection.VALID_DIRECTIONS[side];
				int x = xCoord + wdir.offsetX;
				int y = yCoord + wdir.offsetY;
				int z = zCoord + wdir.offsetZ;

				int thisStrength = BasicWireUtils.getPowerStrength(worldObj, x, y, z, wdir.ordinal() ^ 1, -1, false);
				newStrength = Math.max(newStrength, Math.min(thisStrength - 1, MAX_STRENGTH));

				if (!worldObj.isRemote)
					strengthFromNonWireBlocks = (short) Math.max(strengthFromNonWireBlocks, Math.min(thisStrength - 1, MAX_STRENGTH));
			}

			// dontEmitPower = true;

			for (int dir = 0; dir < 6; dir++) {
				if (!connectsInDirection(side, dir))
					continue;

				ForgeDirection fdir = ForgeDirection.VALID_DIRECTIONS[dir];
				int x = xCoord + fdir.offsetX, y = yCoord + fdir.offsetY, z = zCoord + fdir.offsetZ;

				int odir = dir ^ 1;
				int testside = side;

				if (connectsInDirectionAroundCorner(side, dir)) {
					fdir = ForgeDirection.VALID_DIRECTIONS[side];
					x += fdir.offsetX;
					y += fdir.offsetY;
					z += fdir.offsetZ;

					odir = side ^ 1;
					testside = dir ^ 1;
				}

				newStrength = updateStrengthFromBlock(x, y, z, odir, testside, newStrength);
			}

			// dontEmitPower = false;
		}

		if (worldObj.isRemote)
			newStrength = Math.max(newStrength, strengthFromNonWireBlocks);

		return newStrength;
	}

	protected int getInputPowerStrength(World worldObj, int x, int y, int z, int dir, int side, boolean countWires) {
		return BasicWireUtils.getPowerStrength(worldObj, x, y, z, dir, side, countWires);
	}

	private void updateConnectedWireSignal() {
		int notifiedSides = 0;

		if (CommandDebug.WIRE_DEBUG_PARTICLES)
			debugEffect_bonemeal();

		for (int side = 0; side < 6; side++) {
			if (connectsInDirectionByJacketedWire(side)) {
				if ((notifiedSides & (1 << side)) == 0) {
					notifiedSides |= 1 << side;

					ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[side];
					int x = xCoord + fd.offsetX, y = yCoord + fd.offsetY, z = zCoord + fd.offsetZ;

					TileEntity t = worldObj.getBlockTileEntity(x, y, z);
					if (t instanceof TileRedAlloy)
						((TileRedAlloy) t).updateSignal(this);
				}
			}

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

					if (worldObj.getBlockId(x, y, z) == ProjectRed.blockWire.blockID) {
						TileWire t = (TileWire) worldObj.getBlockTileEntity(x, y, z);
						if (t instanceof TileRedAlloy) {
							((TileRedAlloy) t).updateSignal(this);
						}
					}
				}
			}
		}
	}

	private static boolean blockUpdateCausedByAlloyWire = false;

	protected void updateSignal(TileRedAlloy source) {

		if (worldObj.isRemote && !syncSignalStrength)
			return; // doesn't make sense for unsynced wire types

		if (isUpdatingStrength) {
			recursiveUpdatePending = true;
			return;
		}

		// True if this is the first red alloy tile to update, which received an
		// update from something else,
		// and is now causing a whole bunch of red alloy tiles to change.
		// Only this change will be sent to the client, which will then mirror
		// the processing the server does,
		// to save bandwidth.
		// Note: if syncSignalStrength is false nothing is sent to the client.
		boolean wasFirstServerChange = !worldObj.isRemote && source == null;

		int oldStrengthFromNonWireBlocks = strengthFromNonWireBlocks;

		isUpdatingStrength = true;

		int newStrength;
		int startStrength = strength;

		do {
			recursiveUpdatePending = false;

			int prevStrength = strength;
			strength = 0;
			newStrength = getStrengthFromSurroundingBlocks();

			// if(prevStrength != newStrength)
			// System.out.println(xCoord+","+yCoord+","+zCoord+" red alloy update pass; "+prevStrength+" -> "+newStrength);

			if (newStrength < prevStrength) {
				// this is a huge optimization - it results in a "pulse" of 0
				// strength being sent down the wire
				// when turning off. if there is another source of power further
				// down the wire, that one will block
				// the pulse and propagate backwards, turning the wires back on
				// with the correct strength in 2 updates.
				updateConnectedWireSignal();
				newStrength = getStrengthFromSurroundingBlocks();
			}

			strength = (short) newStrength;

			if (strength != prevStrength)
				updateConnectedWireSignal();

		} while (recursiveUpdatePending);

		isUpdatingStrength = false;

		// if(startStrength != newStrength)
		// System.out.println(xCoord+","+yCoord+","+zCoord+" red alloy update; "+startStrength+" -> "+newStrength);

		if (strength != startStrength) {
			if (!worldObj.isRemote) {
				blockUpdateCausedByAlloyWire = true;
				notifyExtendedPowerableNeighbours();
				blockUpdateCausedByAlloyWire = false;
			}

			// System.out.println((worldObj.isRemote ? "client " :
			// wasFirstServerChange ? "was first " : "Not first ") +
			// "change at: " +
			// xCoord+","+yCoord+","+zCoord+", new strength: "+strength+", sfnwb: "+oldStrengthFromNonWireBlocks+" -> "+strengthFromNonWireBlocks);

			if (syncSignalStrength && (worldObj.isRemote || wasFirstServerChange || strengthFromNonWireBlocks != oldStrengthFromNonWireBlocks)) {
				if (!worldObj.isRemote && CommandDebug.WIRE_LAG_PARTICLES)
					debugEffect_bonemeal();
				worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
			}

		} else if (syncSignalStrength && !worldObj.isRemote && oldStrengthFromNonWireBlocks != strengthFromNonWireBlocks) {
			// System.out.println("SFNWB change at: " +
			// xCoord+","+yCoord+","+zCoord+", new strength: "+strength+", sfnwb: "+oldStrengthFromNonWireBlocks+" -> "+strengthFromNonWireBlocks);

			worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
			if (CommandDebug.WIRE_LAG_PARTICLES)
				debugEffect_bonemeal();
		}
	}

	@Override
	void onNeighbourBlockChange() {
		if (blockUpdateCausedByAlloyWire)
			return;

		super.onNeighbourBlockChange();

		updateSignal(null);
	}

	@Override
	public void readFromNBT(NBTTagCompound tag) {
		super.readFromNBT(tag);

		strength = tag.getShort("strength");
		strengthFromNonWireBlocks = tag.getShort("strengthNWB");
	}

	@Override
	public void writeToNBT(NBTTagCompound tag) {
		super.writeToNBT(tag);

		tag.setShort("strength", strength);
		tag.setShort("strengthNWB", strengthFromNonWireBlocks);
	}

	@Override
	public boolean canUpdate() {
		return false;
	}

	@Override
	public void onDataPacket(INetworkManager net, Packet132TileEntityData pkt) {
		super.onDataPacket(net, pkt);

		if (syncSignalStrength) {
			strength = pkt.customParam1.getShort("_str");
			strengthFromNonWireBlocks = pkt.customParam1.getShort("_snwb");
		}

		// The server will only send an update for the first piece of alloy wire
		// that changed strength.
		// It will not send updates for any other pieces that changed as a
		// result.
		// So we have to simulate that on the client as well.
		// notifyExtendedNeighbourWiresOnClient();
		// updateSignal(null);
		updateConnectedWireSignal();
	}

	@Override
	public Packet132TileEntityData getDescriptionPacket() {
		Packet132TileEntityData p = super.getDescriptionPacket();
		if (syncSignalStrength) {
			p.customParam1.setShort("_str", strength);
			p.customParam1.setShort("_snwb", strengthFromNonWireBlocks);
		}
		return p;
	}

	// normal direction -> canConnectRedstone direction parameter
	// (-2 indicates this direction cannot be passed)
	private static int[] canConnectRedstoneDirectionMap = {-2, -1, 0, 2, 3, 1};

	@Override
	protected boolean connects(int x, int y, int z, int wireSide, int direction) {
		Block b = Block.blocksList[worldObj.getBlockId(x, y, z)];
		if (b == null)
			return false;
		if (b.canProvidePower()) {
			return true;
		}
		if (direction >= 0 && direction < 6 && canConnectRedstoneDirectionMap[direction] != -2 && b.canConnectRedstone(worldObj, x, y, z, canConnectRedstoneDirectionMap[direction])) {
			return true;
		}
		return false;
	}

	/**
	 * Returns signal strength from 0 to 255.
	 */
	public short getRedstoneSignalStrength() {
		return dontEmitPower ? 0 : strength;
	}

	@Override
	public short getEmittedSignalStrength(int side, int dir) { // IRedstoneEmittingTile
		return connectsInDirection(side, dir) ? getRedstoneSignalStrength() : 0;
	}

	private static Field wiresProvidePower = BlockRedstoneWire.class.getDeclaredFields()[0];
	static {
		try {
			wiresProvidePower.setAccessible(true);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		if (wiresProvidePower.getType() != boolean.class)
			throw new AssertionError("field order changed; fix me");
	}

	public boolean canProvideStrongPowerInDirection(int dir) {
		try {
			return connectToBlockBelow && isWirePresent(dir) && wiresProvidePower.getBoolean(Block.redstoneWire);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void notifyExtendedPowerableNeighbours() {
		boolean any = false;

		for (int k = 0; k < 6; k++) {
			ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[k];
			int x = xCoord + fd.offsetX;
			int y = yCoord + fd.offsetY;
			int z = zCoord + fd.offsetZ;

			boolean causedBlockUpdate = false;

			if (canProvideWeakPowerInDirection(k)) {
				causedBlockUpdate = true;
				worldObj.notifyBlockOfNeighborChange(x, y, z, ProjectRed.blockWire.blockID);
			}

			if (canProvideStrongPowerInDirection(k)) {
				causedBlockUpdate = true;
				worldObj.notifyBlocksOfNeighborChange(x, y, z, ProjectRed.blockWire.blockID);
			}

			if (!causedBlockUpdate) {
				Block block = Block.blocksList[worldObj.getBlockId(x, y, z)];
				if (block != null && block.hasTileEntity(worldObj.getBlockMetadata(x, y, z))) {
					TileEntity te = worldObj.getBlockTileEntity(x, y, z);
					if (te instanceof IRedstoneUpdatable)
						((IRedstoneUpdatable) te).onRedstoneInputChanged();
				}
			}

			any |= causedBlockUpdate;
		}

		if (canProvideWeakPowerInDirection(Dir.NX)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord + 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideWeakPowerInDirection(Dir.PX)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord - 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideWeakPowerInDirection(Dir.NY)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord, yCoord - 1, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideWeakPowerInDirection(Dir.PY)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord, yCoord + 1, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideWeakPowerInDirection(Dir.NZ)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord, yCoord, zCoord - 1, ProjectRed.blockWire.blockID);
		}
		if (canProvideWeakPowerInDirection(Dir.PZ)) {
			any = true;
			worldObj.notifyBlockOfNeighborChange(xCoord, yCoord, zCoord + 1, ProjectRed.blockWire.blockID);
		}

		if (canProvideStrongPowerInDirection(Dir.NX)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord + 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideStrongPowerInDirection(Dir.PX)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord - 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideStrongPowerInDirection(Dir.NY)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord - 1, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideStrongPowerInDirection(Dir.PY)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord + 1, zCoord, ProjectRed.blockWire.blockID);
		}
		if (canProvideStrongPowerInDirection(Dir.NZ)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord - 1, ProjectRed.blockWire.blockID);
		}
		if (canProvideStrongPowerInDirection(Dir.PZ)) {
			any = true;
			worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord + 1, ProjectRed.blockWire.blockID);
		}

		if (any && CommandDebug.WIRE_LAG_PARTICLES)
			debugEffect_fireburst();
	}

	public boolean canProvideWeakPowerInDirection(int dir) {
		try {
			// if(!isWirePresent(dir) && !connectsInDirection(dir))
			// return false;

			// There must be a wire on any side except the opposite side, or a
			// freestanding wire
			if ((getSideMask() & ~(1 << (dir ^ 1))) == 0 && !hasJacketedWire()) {
				return false;
			}

			if (!wiresProvidePower.getBoolean(Block.redstoneWire))
				return false;

			return true;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * Returns the vanilla redstone strength from 0 to 15.
	 */
	public byte getVanillaRedstoneStrength() {
		return (byte) (getRedstoneSignalStrength() / 17);
	}

	@Override
	protected boolean debug(EntityPlayer ply) {
		ply.sendChatToPlayer(ChatMessageComponent.func_111077_e((worldObj.isRemote ? "Client" : "Server") + " signal strength: " + strength + ", nwb: " + strengthFromNonWireBlocks));
		
		super.debug(ply);
		return true;
	}

	@Override
	protected boolean canConnectToWire(TileWire wire) {
		return wire instanceof TileRedAlloy;
	}

	@Override
	public void onRedstoneInputChanged() {
		updateSignal(null);
	}
}
