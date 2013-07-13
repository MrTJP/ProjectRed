package mrtjp.projectred.multipart.wiring.wires;

import java.util.Collections;
import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.interfaces.wiring.IConnectable;
import mrtjp.projectred.interfaces.wiring.IWire;
import mrtjp.projectred.multipart.TileCoverableBase;
import mrtjp.projectred.multipart.microblocks.EnumPosition;
import mrtjp.projectred.multipart.microblocks.EnumPositionClass;
import mrtjp.projectred.multipart.microblocks.IMicroblockCoverSystem;
import mrtjp.projectred.multipart.microblocks.Part;
import mrtjp.projectred.multipart.microblocks.PartType;
import mrtjp.projectred.multipart.wiring.InvalidTile;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Coords;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public abstract class TileWire extends TileCoverableBase implements IConnectable, IWire {

	// this is null before rawAddWire is called the first time (when placing a
	// wire).
	private EnumWire wireType;

	// true if there is a jacketed wire in this block
	private boolean haveJacketed;

	// bitmask where bit (side) is set if the jacketed wire in this block
	// connects to that side
	private byte jacketConnectMaskCache;

	// bitmask where bit (wireSide) is set if there is a wire on that side
	private byte wireMask;

	// bitmask where bit ((wireSide * 6) + direction - 2)
	// is set if the wire on that side connects in that direction
	// -1 if not cached (-1 is normally illegal as several bits are always 0)
	// note that bits -2 and -1 are never used, as are bits 32 and 33, so this
	// barely fits
	// into 32 bits (with some unused ones in the middle)
	private int connectMaskCache = -1;

	// similar to connectMaskCache but each bit indicates if that connection is
	// around a corner.
	private int connectCornerCache = -1;

	// used to trigger a block update next tick if the wire's connections
	// changed
	private boolean notifyNeighboursNextTick = false;

	@Override
	public void writeToNBT(NBTTagCompound tag) {
		if (wireType == null) {
			// block will get deleted on the first tick after being re-loaded.
			new InvalidTile().writeToNBT(tag);
			return;
		}

		super.writeToNBT(tag);

		tag.setByte("type", (byte) wireType.ordinal());
		tag.setByte("mask", wireMask);
		tag.setInteger("cmc", connectMaskCache);
		tag.setInteger("ccc", connectCornerCache);
		if (canUpdate()) {
			tag.setBoolean("notifyQueued", notifyNeighboursNextTick);
		}
		if (haveJacketed) {
			tag.setByte("jcmc", jacketConnectMaskCache);
		}
	}

	@Override
	public void readFromNBT(NBTTagCompound tag) {
		super.readFromNBT(tag);

		int type = tag.getByte("type");
		if (type < 0 || type >= EnumWire.VALUES.length) {
			wireType = null;
		} else {
			wireType = EnumWire.VALUES[type];
		}
		wireMask = tag.getByte("mask");

		if (haveJacketed = tag.hasKey("jcmc")) {
			jacketConnectMaskCache = tag.getByte("jcmc");
		}

		if (tag.hasKey("cmc")) {
			connectMaskCache = tag.getInteger("cmc");
			connectCornerCache = tag.getInteger("ccc");
		} else {
			connectMaskCache = -1;
			connectCornerCache = -1;
		}
		if (canUpdate()) {
			notifyNeighboursNextTick = tag.getBoolean("notifyQueued");
		}
	}

	@Override
	public abstract boolean canUpdate();

	@Override
	public void updateEntity() {
		if (notifyNeighboursNextTick) {
			notifyNeighboursNextTick = false;
			notifyNeighbours();
		}
		super.updateEntity();
	}

	@Override
	public boolean isPlacementBlockedByTile(PartType<?> type, EnumPosition pos) {

		final int MASK_NX = 1 << Dir.NX;
		final int MASK_PX = 1 << Dir.PX;
		final int MASK_NY = 1 << Dir.NY;
		final int MASK_PY = 1 << Dir.PY;
		final int MASK_NZ = 1 << Dir.NZ;
		final int MASK_PZ = 1 << Dir.PZ;

		final double MAX_OPPOSITE_THICKNESS = 1 - getType().thickness;
		final double MAX_SIDE_THICKNESS = 0.25;

		if (pos.clazz == EnumPositionClass.Centre || pos.clazz == EnumPositionClass.Post)
			return true;

		switch (pos) {
		case FaceNX:
			return (wireMask & MASK_NX) != 0 || ((wireMask & ~MASK_PX) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_PX) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		case FacePX:
			return (wireMask & MASK_PX) != 0 || ((wireMask & ~MASK_NX) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_NX) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		case FaceNY:
			return (wireMask & MASK_NY) != 0 || ((wireMask & ~MASK_PY) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_PY) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		case FacePY:
			return (wireMask & MASK_PY) != 0 || ((wireMask & ~MASK_NY) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_NY) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		case FaceNZ:
			return (wireMask & MASK_NZ) != 0 || ((wireMask & ~MASK_PZ) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_PZ) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		case FacePZ:
			return (wireMask & MASK_PZ) != 0 || ((wireMask & ~MASK_NZ) != 0 && type.getSize() > MAX_SIDE_THICKNESS) || ((wireMask & MASK_NZ) != 0 && type.getSize() > MAX_OPPOSITE_THICKNESS);
		default:
			final double FAKE_WIRE_MIN = 0.5 - (0.5 - MAX_SIDE_THICKNESS);
			final double FAKE_WIRE_MAX = 0.5 + (0.5 - MAX_SIDE_THICKNESS);
			final double FAKE_WIRE_THICK = 1 - MAX_OPPOSITE_THICKNESS;
			for (int k = 0; k < 6; k++) {
				if (!isWirePresent(k))
					continue;

				AxisAlignedBB bb;
				switch (k) {
				case Dir.NX:
					bb = AxisAlignedBB.getAABBPool().getAABB(0, FAKE_WIRE_MIN, FAKE_WIRE_MIN, FAKE_WIRE_THICK, FAKE_WIRE_MAX, FAKE_WIRE_MAX);
					break;
				case Dir.NY:
					bb = AxisAlignedBB.getAABBPool().getAABB(FAKE_WIRE_MIN, 0, FAKE_WIRE_MIN, FAKE_WIRE_MAX, FAKE_WIRE_THICK, FAKE_WIRE_MAX);
					break;
				case Dir.NZ:
					bb = AxisAlignedBB.getAABBPool().getAABB(FAKE_WIRE_MIN, FAKE_WIRE_MIN, 0, FAKE_WIRE_MAX, FAKE_WIRE_MAX, FAKE_WIRE_THICK);
					break;
				case Dir.PX:
					bb = AxisAlignedBB.getAABBPool().getAABB(1 - FAKE_WIRE_THICK, FAKE_WIRE_MIN, FAKE_WIRE_MIN, 1, FAKE_WIRE_MAX, FAKE_WIRE_MAX);
					break;
				case Dir.PY:
					bb = AxisAlignedBB.getAABBPool().getAABB(FAKE_WIRE_MIN, 1 - FAKE_WIRE_THICK, FAKE_WIRE_MIN, FAKE_WIRE_MAX, 1, FAKE_WIRE_MAX);
					break;
				case Dir.PZ:
					bb = AxisAlignedBB.getAABBPool().getAABB(FAKE_WIRE_MIN, FAKE_WIRE_MIN, 1 - FAKE_WIRE_THICK, FAKE_WIRE_MAX, FAKE_WIRE_MAX, 1);
					break;
				default:
					continue; // dead code
				}

				if (bb.intersectsWith(Part.getBoundingBoxFromPool(pos, type.getSize())))
					return true;
			}
			return false;
		}
	}

	public boolean canPlaceWireOnSide(EnumWire type, int side) {
		int newMask = wireMask | (1 << side);
		return type == wireType && (wireMask & (1 << side)) == 0 && (haveJacketed || (newMask != 0x03 && newMask != 0x0C && newMask != 0x30));
	}

	public boolean canAddJacketedWire(EnumWire type) {
		return type == this.getType() && !haveJacketed;
	}

	void rawAddWire(EnumWire type, int side) {
		if (type != wireType && wireType != null)
			throw new IllegalArgumentException("wrong type (passed " + type + ", expected " + wireType + ")");
		wireType = type;
		// System.out.println((worldObj.isRemote ? "client" : "server") +
		// " rawAddWire "+side+" type="+wireType);
		wireMask |= (byte) (1 << side);
	}

	public boolean addWire(EnumWire type, int side) {
		if (!canPlaceWireOnSide(type, side))
			return false;

		wireMask |= (byte) (1 << side);
		worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);

		computeConnections();
		notifyExtendedNeighbours();

		return true;
	}

	void rawAddJacketedWire(EnumWire type) {
		if (getType() != null && type != getType())
			throw new IllegalArgumentException("wrong type (passed " + type + ", expected " + getType() + ")");
		wireType = type;
		haveJacketed = true;
	}

	public boolean addJacketedWire(EnumWire type) {
		if (!canAddJacketedWire(type))
			return false;

		rawAddJacketedWire(type);

		computeConnections();
		notifyExtendedNeighbours();

		return true;
	}

	@Override
	public Packet132TileEntityData getDescriptionPacket() {
		if (wireType == null) {
			return new Packet132TileEntityData(xCoord, yCoord, zCoord, 0, new NBTTagCompound());
		}
		NBTTagCompound tag = new NBTTagCompound();

		if (wireType == null) {
			tag.setBoolean("invalid", true);

		} else {
			if (connectMaskCache == -1 || connectCornerCache == -1) {
				computeConnections();
			}
			tag.setByte("t", (byte) wireType.ordinal());
			tag.setByte("m", wireMask);
			tag.setByteArray("c", getCoverSystem().writeDescriptionBytes());
			tag.setInteger("C", connectMaskCache);
			tag.setInteger("C2", connectCornerCache);
			if (haveJacketed) {
				tag.setByte("j", jacketConnectMaskCache);
			}
		}
		return new Packet132TileEntityData(xCoord, yCoord, zCoord, 0, tag);
	}

	@Override
	public void onDataPacket(INetworkManager net, Packet132TileEntityData pkt) {
		if (pkt.customParam1.getBoolean("invalid"))
			return;

		wireType = EnumWire.VALUES[pkt.customParam1.getByte("t")];
		wireMask = pkt.customParam1.getByte("m");

		getCoverSystem().readDescriptionBytes(pkt.customParam1.getByteArray("c"), 0);

		connectMaskCache = pkt.customParam1.getInteger("C");
		connectCornerCache = pkt.customParam1.getInteger("C2");

		if (haveJacketed = pkt.customParam1.hasKey("j"))
			jacketConnectMaskCache = pkt.customParam1.getByte("j");

		worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
	}

	public final EnumWire getType() {
		return wireType;
	}

	public final byte getSideMask() {
		return wireMask;
	}

	void onNeighbourBlockChange() {
		if (worldObj.isRemote) {
			return;
		}

		for (int k = 0; k < 6; k++) {
			if ((wireMask & (1 << k)) == 0)
				continue;

			int x = xCoord + ForgeDirection.VALID_DIRECTIONS[k].offsetX;
			int y = yCoord + ForgeDirection.VALID_DIRECTIONS[k].offsetY;
			int z = zCoord + ForgeDirection.VALID_DIRECTIONS[k].offsetZ;
			;
			if (!BasicWireUtils.canPlaceWireOnSide(worldObj, x, y, z, ForgeDirection.VALID_DIRECTIONS[k ^ 1], true)) {
				removeAndDropWireOnSide(k);
				if (wireMask == 0)
					return;
			}
		}

		computeConnections();
	}

	/**
	 * Tile-overridable equivalent of
	 * {@link IConnectable#connects(EnumWire, int, int)}. Parameters are the
	 * same, so they are from the perspective of the other block.
	 */
	protected boolean connects(int x, int y, int z, int wireSide, int direction) {
		return false;
	}

	/**
	 * Tile-overridable equivalent of
	 * {@link IConnectable#connectsAroundCorner(EnumWire, int, int)}. Parameters
	 * are the same, so they are from the perspective of the other block.
	 */
	protected boolean connectsAroundCorner(int x, int y, int z, int wireSide, int direction) {
		return false;
	}

	private boolean connectsInDirectionUncached(int wireSide, int direction) {
		if ((wireSide & 6) == (direction & 6))
			return false;
		if (!getCoverSystem().isEdgeOpen(wireSide, direction))
			return false;
		if ((wireMask & (1 << direction)) != 0)
			return true;

		int x = xCoord, y = yCoord, z = zCoord;
		switch (direction) {
		case Dir.NX:
			x--;
			break;
		case Dir.PX:
			x++;
			break;
		case Dir.NY:
			y--;
			break;
		case Dir.PY:
			y++;
			break;
		case Dir.NZ:
			z--;
			break;
		case Dir.PZ:
			z++;
			break;
		default:
			throw new IllegalArgumentException("invalid direction " + direction);
		}
		if (!worldObj.isAirBlock(x, y, z)) {
			TileEntity te = worldObj.getBlockTileEntity(x, y, z);
			if (te instanceof IConnectable)
				return ((IConnectable) te).connects(this, wireSide, direction ^ 1);
			return connects(x, y, z, wireSide, direction ^ 1);
		}

		return false;
	}

	private boolean jacketConnectsInDirectionUncached(int direction) {
		if (!haveJacketed)
			return false;
		if (!getCoverSystem().isSideOpen(direction))
			return false;
		if ((wireMask & (1 << direction)) != 0)
			return true;

		int x = xCoord, y = yCoord, z = zCoord;
		switch (direction) {
		case Dir.NX:
			x--;
			break;
		case Dir.PX:
			x++;
			break;
		case Dir.NY:
			y--;
			break;
		case Dir.PY:
			y++;
			break;
		case Dir.NZ:
			z--;
			break;
		case Dir.PZ:
			z++;
			break;
		default:
			throw new IllegalArgumentException("invalid direction " + direction);
		}
		if (!worldObj.isAirBlock(x, y, z)) {
			TileEntity te = worldObj.getBlockTileEntity(x, y, z);
			if (te instanceof IConnectable)
				return ((IConnectable) te).connects(this, -1, direction ^ 1);
			return connects(x, y, z, -1, direction ^ 1);
		}

		return false;
	}

	private boolean connectsAroundCornerUncached(int wireSide, int direction) {
		if ((wireSide & 6) == (direction & 6))
			return false;
		if ((wireMask & (1 << direction)) != 0)
			return false;
		if (!getCoverSystem().isEdgeOpen(wireSide, direction))
			return false;

		int x = xCoord, y = yCoord, z = zCoord;
		switch (direction) {
		case Dir.NX:
			x--;
			break;
		case Dir.PX:
			x++;
			break;
		case Dir.NY:
			y--;
			break;
		case Dir.PY:
			y++;
			break;
		case Dir.NZ:
			z--;
			break;
		case Dir.PZ:
			z++;
			break;
		default:
			throw new IllegalArgumentException("invalid direction " + direction);
		}

		if (!BasicWireUtils.canConnectThroughEdge(worldObj, x, y, z, wireSide, direction ^ 1)) {
			return false;
		}

		switch (wireSide) {
		case Dir.NX:
			x--;
			break;
		case Dir.PX:
			x++;
			break;
		case Dir.NY:
			y--;
			break;
		case Dir.PY:
			y++;
			break;
		case Dir.NZ:
			z--;
			break;
		case Dir.PZ:
			z++;
			break;
		default:
			throw new IllegalArgumentException("invalid direction " + direction);
		}
		TileEntity te = worldObj.getBlockTileEntity(x, y, z);
		if (te instanceof IConnectable)
			return ((IConnectable) te).connectsAroundCorner(this, direction ^ 1, wireSide ^ 1);
		return connectsAroundCorner(x, y, z, direction ^ 1, wireSide ^ 1);
	}

	private void computeConnections() {
		int prev = connectMaskCache, prevC = connectCornerCache, prevJ = jacketConnectMaskCache;
		connectMaskCache = 0;
		connectCornerCache = 0;
		jacketConnectMaskCache = 0;
		for (int side = 0; side < 6; side++) {
			if (haveJacketed) {
				if (jacketConnectsInDirectionUncached(side))
					jacketConnectMaskCache |= (1 << side);
			}

			if ((wireMask & (1 << side)) != 0) {
				for (int dir = 0; dir < 6; dir++) {
					if (connectsInDirectionUncached(side, dir))
						connectMaskCache |= (1 << (side * 6 + dir - 2));
					else if (connectsAroundCornerUncached(side, dir)) {
						connectMaskCache |= (1 << (side * 6 + dir - 2));
						connectCornerCache |= (1 << (side * 6 + dir - 2));
					}
				}
			}
		}

		if (prev != connectMaskCache || prevC != connectCornerCache || prevJ != jacketConnectMaskCache) {
			worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
			if (canUpdate())
				notifyNeighboursNextTick = true;
			else
				notifyNeighbours();
		}
	}

	public boolean connectsInDirection(int wireSide, int direction) {
		if (connectMaskCache == -1)
			computeConnections();
		if (wireSide == -1)
			return connectsInDirectionByJacketedWire(direction);
		return (connectMaskCache & (1 << (wireSide * 6 + direction - 2))) != 0;
	}

	public boolean connectsInDirectionAroundCorner(int wireSide, int direction) {
		if (connectMaskCache == -1)
			computeConnections();
		return (connectCornerCache & (1 << (wireSide * 6 + direction - 2))) != 0;
	}

	public boolean connectsInDirectionByJacketedWire(int direction) {
		return haveJacketed && (jacketConnectMaskCache & (1 << direction)) != 0;
	}

	public boolean connectsInDirection(int direction) {
		if (connectMaskCache == -1)
			computeConnections();
		if (connectsInDirectionByJacketedWire(direction))
			return true;
		// +X -X +Z -Z +Y -Y
		// 01000001000001000001000001000001
		// 4 1 0 4 1 0 4 1
		return (connectMaskCache & (0x41041041 << (direction - 2))) != 0;
	}

	/** Destroys the wire block when the last wire is removed. */
	public void removeAndDropWireOnSide(int side) {
		if (!removeWireOnSide(side)) {
			return;
		}
		ItemStack dropped = new ItemStack(ProjectRed.blockWire, 1, wireType.ordinal());
		BasicUtils.ejectItem(worldObj, dropped, false, null, side, new Coords(this));
	}

	/** Destroys the wire block when the last wire is removed. */
	public boolean removeWireOnSide(int side) {
		if ((wireMask & (1 << side)) == 0)
			return false;

		wireMask &= ~(1 << side);
		if (wireMask == 0 && !haveJacketed) {
			if (cover != null)
				cover.convertToContainerBlock();
			else
				worldObj.setBlock(xCoord, yCoord, zCoord, 0, 0, 3);
			notifyExtendedNeighbours();

		} else {
			if (wireMask == 0x03 && !haveJacketed)
				removeAndDropWireOnSide(1);
			else if (wireMask == 0x0C && !haveJacketed)
				removeAndDropWireOnSide(3);
			else if (wireMask == 0x30 && !haveJacketed)
				removeAndDropWireOnSide(5);
			else {
				computeConnections();
				worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
				notifyExtendedNeighbours();
			}
		}

		return true;

	}

	/** Destroys the wire block when the last wire is removed. */
	public boolean removeJacketedWire() {
		if (!haveJacketed) {
			return false;
		}

		haveJacketed = false;
		if (wireMask == 0) {
			if (cover != null)
				cover.convertToContainerBlock();
			else
				worldObj.setBlock(xCoord, yCoord, zCoord, 0, 0, 3);
			notifyExtendedNeighbours();

		} else {
			if (wireMask == 0x03)
				removeAndDropWireOnSide(1);
			else if (wireMask == 0x0C)
				removeAndDropWireOnSide(3);
			else if (wireMask == 0x30)
				removeAndDropWireOnSide(5);
			else {
				computeConnections();
				worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
				notifyExtendedNeighbours();
			}
		}

		return true;
	}

	/**
	 * Notifies neighbours one or two blocks away, in the same pattern as most
	 * redstone updates.
	 */
	void notifyExtendedNeighbours() {
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord + 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord - 1, yCoord, zCoord, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord + 1, zCoord, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord - 1, zCoord, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord + 1, ProjectRed.blockWire.blockID);
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord - 1, ProjectRed.blockWire.blockID);
	}

	private void notifyNeighbourOnClient(int dx, int dy, int dz) {
		if (!worldObj.isRemote)
			throw new IllegalStateException("Not the client.");

		int blockID = worldObj.getBlockId(xCoord + dx, yCoord + dy, zCoord + dz);
		if (blockID == ProjectRed.blockWire.blockID)
			Block.blocksList[blockID].onNeighborBlockChange(worldObj, xCoord + dx, yCoord + dy, zCoord + dz, ProjectRed.blockWire.blockID);
	}

	void notifyExtendedNeighbourWiresOnClient() {
		for (int dz = -1; dz <= 1; dz++)
			for (int dy = -1; dy <= 1; dy++)
				for (int dx = -1; dx <= 1; dx++)
					notifyNeighbourOnClient(dx, dy, dz);
		notifyNeighbourOnClient(2, 0, 0);
		notifyNeighbourOnClient(-2, 0, 0);
		notifyNeighbourOnClient(0, 2, 0);
		notifyNeighbourOnClient(0, -2, 0);
		notifyNeighbourOnClient(0, 0, 2);
		notifyNeighbourOnClient(0, 0, -2);
	}

	void notifyNeighbours() {
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord, ProjectRed.blockWire.blockID);
	}

	void notifyConnectedWireNeighbours() {
		int notifiedSides = 0;

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
						if ((notifiedSides & (1 << side)) != 0)
							continue;
						notifiedSides |= 1 << side;
					}

					if (worldObj.getBlockId(x, y, z) == ProjectRed.blockWire.blockID) {
						ProjectRed.blockWire.onNeighborBlockChange(worldObj, x, y, z, ProjectRed.blockWire.blockID);
					}
				}
			}
		}
	}

	/**
	 * Override this to change the constraints on wire connections (for example,
	 * insulated wire only connects to the same colour). Must be symmetric
	 * (a.canConnectToWire(b) == b.canConnectToWire(a))
	 */
	protected boolean canConnectToWire(TileWire wire) {
		return wire.getType() == this.wireType;
	}

	// IWire
	@Override
	public boolean wireConnectsInDirection(int blockFace, int direction) {
		return connectsInDirection(blockFace, direction);
	}

	// IConnectable
	@Override
	public final boolean connects(IWire wire, int blockFace, int fromDirection) {
		if (!canConnectToWire((TileWire) wire) || !((TileWire) wire).canConnectToWire(this))
			return false;
		IMicroblockCoverSystem imcs = getCoverSystem();
		if (blockFace == -1)
			return haveJacketed && (imcs == null || imcs.isSideOpen(fromDirection));
		else if (blockFace >= 0 && blockFace <= 5)
			return (wireMask & (1 << blockFace)) != 0 && (imcs == null || imcs.isEdgeOpen(blockFace, fromDirection));
		else
			return false;
	}

	@Override
	public boolean connectsAroundCorner(IWire wire, int blockFace, int fromDirection) {
		return connects(wire, blockFace, fromDirection);
	}

	@Override
	public boolean isPositionOccupiedByTile(EnumPosition pos) {
		switch (pos) {
		case FaceNX:
			return (wireMask & (1 << Dir.NX)) != 0;
		case FacePX:
			return (wireMask & (1 << Dir.PX)) != 0;
		case FaceNY:
			return (wireMask & (1 << Dir.NY)) != 0;
		case FacePY:
			return (wireMask & (1 << Dir.PY)) != 0;
		case FaceNZ:
			return (wireMask & (1 << Dir.NZ)) != 0;
		case FacePZ:
			return (wireMask & (1 << Dir.PZ)) != 0;
		default:
			return false;
		}
	}

	public boolean isWirePresent(int side) {
		return (wireMask & (1 << side)) != 0;
	}

	public int getVisualWireColour() {
		return 0xFFFFFF;
	}

	@Override
	public EnumPosition getPartPosition(int part) {
		if (part == 6)
			return EnumPosition.Centre;
		return EnumPosition.getFacePosition(part);
	}

	@Override
	public AxisAlignedBB getPartAABBFromPool(int part) {
		if (part == 6)
			return haveJacketed ? Part.getBoundingBoxFromPool(EnumPosition.Centre, 0.5) : null;

		if ((wireMask & (1 << part)) == 0)
			return null;
		return Part.getBoundingBoxFromPool(EnumPosition.getFacePosition(part), wireType.thickness);
	}

	@Override
	protected int getNumTileOwnedParts() {
		return 7;
	}

	@Override
	public float getPlayerRelativePartHardness(EntityPlayer ply, int part) {
		return ply.getCurrentPlayerStrVsBlock(ProjectRed.blockWire, false, getType().ordinal()) / 0.25f / 30f;
	}

	@Override
	public ItemStack pickPart(MovingObjectPosition rayTrace, int part) {
		if (part == 6 && haveJacketed)
			return new ItemStack(ProjectRed.blockWire, 1, getType().ordinal() | WireDamageValues.DMG_FLAG_JACKETED);
		return new ItemStack(ProjectRed.blockWire, 1, getType().ordinal());
	}

	@Override
	public boolean isSolidOnSide(ForgeDirection side) {
		return false;
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void render(RenderBlocks render) {
		if (getType() == null)
			return;
		WireRenderer.instance.renderWorld(render, getType(), this, getSideMask(), haveJacketed);
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderPart(RenderBlocks render, int part) {
		if (part == 6) {
			WireRenderer.instance.renderWorld(render, getType(), this, 0, true);
			return;
		}

		if ((getSideMask() & (1 << part)) == 0 || getType() == null)
			return;
		WireRenderer.instance.renderWorld(render, getType(), this, 1 << part, false);
	}

	@Override
	public List<ItemStack> removePartByPlayer(EntityPlayer ply, int part) {
		EnumWire type = getType();

		if (part == 6) {
			if (!haveJacketed)
				return null;

			if (!removeJacketedWire())
				return null;

			return Collections.singletonList(new ItemStack(ProjectRed.blockWire, 1, type.ordinal() | WireDamageValues.DMG_FLAG_JACKETED));
		}

		if (type == null || (getSideMask() & (1 << part)) == 0)
			return null;
		if (!removeWireOnSide(part))
			return null;
		return Collections.singletonList(new ItemStack(ProjectRed.blockWire, 1, type.ordinal()));
	}

	@Override
	public void onMicroblocksChanged() {
		notifyExtendedNeighbours();
		worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
	}

	protected boolean debug(EntityPlayer ply) {
		return false;
	}

	protected void debugEffect_bonemeal() {
		worldObj.playAuxSFX(2005, xCoord, yCoord, zCoord, 0);
	}

	protected void debugEffect_fireburst() {
		worldObj.playAuxSFX(2004, xCoord, yCoord, zCoord, 0);
	}

	/*
	 * protected void debugEffect_ender() { worldObj.playAuxSFX(2003, xCoord,
	 * yCoord, zCoord, 0); }
	 */

	protected void debugEffect_potion() {
		worldObj.playAuxSFX(2002, xCoord, yCoord, zCoord, 0);
	}

	/*
	 * protected void debugEffect_blockbreak() { worldObj.playAuxSFX(2001,
	 * xCoord, yCoord, zCoord, 0); }
	 */

	protected void debugEffect_smoke() {
		worldObj.playAuxSFX(2000, xCoord, yCoord, zCoord, 0);
	}

	public boolean hasJacketedWire() {
		return haveJacketed;
	}

	public int getJacketedWireConnectionMask() {
		return haveJacketed ? jacketConnectMaskCache : 0;
	}

	@Override
	public void getCollidingBoundingBoxes(AxisAlignedBB mask, List<AxisAlignedBB> list) {
		if (haveJacketed) {
			AxisAlignedBB bb = getPartAABBFromPool(6);
			if (bb != null) {
				bb = bb.offset(xCoord, yCoord, zCoord);
				if (bb.intersectsWith(mask))
					list.add(bb);
			}
		}
	}
}
