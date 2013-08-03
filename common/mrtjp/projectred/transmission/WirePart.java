package mrtjp.projectred.transmission;

import static codechicken.lib.vec.Rotation.sideRotations;
import static codechicken.lib.vec.Vector3.center;

import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.Coords;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.Cuboid6;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.JPartialOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

/**
 * This is the base class for all wire types. It can be used for any sub type,
 * as it contains the base calculations necessary to create a working wire. This
 * calculates all possible connections to sides, around corners, and inside
 * corners, while checking for microblock obstructions.
 * 
 * @author MrTJP
 * 
 */
public abstract class WirePart extends JCuboidPart implements IConnectable, TFacePart, JNormalOcclusion {

	private EnumWire wireType;
	protected boolean isJacketed = false;
	protected boolean notifyNeighborsNextTick = false;
	public int side;
	public boolean isFirstTick = true;
	public boolean hasNewConnections = false;

	// Cached connection status of all sides, stored as absolute
	// ForgeDirections.
	private boolean[] sideExternalConnections = new boolean[6];
	private boolean[] sideInternalConnections = new boolean[6];
	private boolean[] sideCorneredConnections = new boolean[6];

	// True if this regular wire connects to a jacketed wire in this block. If
	// is jacketed, connection mask is stored in sideExternalConnections.
	protected boolean localJacketedConnection = false;

	public WirePart(EnumWire type, boolean isJacketedWire, int onside) {
		wireType = type;
		isJacketed = isJacketedWire;
		side = onside;
	}

	@Override
	public void save(NBTTagCompound tag) {
		super.save(tag);
		tag.setByte("type", (byte) wireType.ordinal());
		tag.setByte("side", (byte) side);
		tag.setBoolean("isjack", isJacketed);
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("ext" + i, sideExternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("int" + i, sideInternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("cor" + i, sideCorneredConnections[i]);
		}
		tag.setBoolean("jack", localJacketedConnection);
	}

	@Override
	public void load(NBTTagCompound tag) {
		super.load(tag);
		int type = tag.getByte("type");
		side = tag.getByte("side");
		isJacketed = tag.getBoolean("isjack");
		if (type < 0 || type >= EnumWire.VALID_WIRE.length) {
			wireType = EnumWire.RED_ALLOY;
		} else {
			wireType = EnumWire.VALID_WIRE[type];
		}
		for (int i = 0; i < 6; i++) {
			sideExternalConnections[i] = tag.getBoolean("ext" + i);
		}
		for (int i = 0; i < 6; i++) {
			sideInternalConnections[i] = tag.getBoolean("int" + i);
		}
		for (int i = 0; i < 6; i++) {
			sideCorneredConnections[i] = tag.getBoolean("cor" + i);
		}
		localJacketedConnection = tag.getBoolean("jack");
	}

	@Override
	public void update() {
		if (BasicUtils.isClient(world())) {
			isFirstTick = false;
			return;
		}
		if (isFirstTick) {
			isFirstTick = false;
			computeConnections();
			updateChange();
		}
		if (notifyNeighborsNextTick) {
			notifyNeighborsNextTick = false;
			updateChange();
		}
		if (hasNewConnections) {
			hasNewConnections = false;
			updateChange();
		}
		super.update();
	}

	@Override
	public void writeDesc(MCDataOutput packet) {
		packet.writeByte(wireType.ordinal());
		packet.writeByte(side);
		packet.writeBoolean(isJacketed ? true : false);
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideExternalConnections[i] ? true : false);
		}
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideInternalConnections[i] ? true : false);
		}
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideCorneredConnections[i] ? true : false);
		}
		packet.writeBoolean(localJacketedConnection ? true : false);

	}

	@Override
	public void readDesc(MCDataInput packet) {
		wireType = EnumWire.values()[packet.readByte()];
		side = packet.readByte();
		isJacketed = packet.readBoolean();
		for (int i = 0; i < 6; i++) {
			sideExternalConnections[i] = packet.readBoolean();
		}
		for (int i = 0; i < 6; i++) {
			sideInternalConnections[i] = packet.readBoolean();
		}
		for (int i = 0; i < 6; i++) {
			sideCorneredConnections[i] = packet.readBoolean();
		}
		localJacketedConnection = packet.readBoolean();

		notifyNeighborsNextTick = true;
	}

	public void updateChange() {
		tile().markDirty();
		tile().markRender();
		if (BasicUtils.isServer(world())) {
			sendDescUpdate();
		}
	}

	public final EnumWire getWireType() {
		return wireType;
	}

	@Override
	public void onPartChanged() {
		notifyExtendedNeighbors();
		computeConnections();
		updateChange();
	}

	@Override
	public void onRemoved() {
		super.onRemoved();
		notifyExtendedNeighbors();
	}

	@Override
	public void onAdded() {
		super.onAdded();
		update();
		notifyExtendedNeighbors();
		updateChange();
	}

	@Override
	public void onNeighborChanged() {
		if (BasicUtils.isClient(world())) {
			return;
		}
		if (!isJacketed) {
			int x = x() + ForgeDirection.getOrientation(side).offsetX;
			int y = y() + ForgeDirection.getOrientation(side).offsetY;
			int z = z() + ForgeDirection.getOrientation(side).offsetZ;
			if (!BasicWireUtils.canPlaceWireOnSide(world(), x, y, z, ForgeDirection.getOrientation(side ^ 1), false)) {
				BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new Coords(x(), y(), z()));
				tile().remPart(this);
			}
		}
		computeConnections();
	}

	protected void computeConnections() {
		if (isFirstTick) {
			return;
		}
		if (!isJacketed) { // Calculate all sides
			boolean[] oldExt = sideExternalConnections.clone();
			boolean[] oldInt = sideInternalConnections.clone();
			boolean[] oldCor = sideCorneredConnections.clone();
			boolean oldJack = localJacketedConnection;
			sideExternalConnections = new boolean[6];
			sideInternalConnections = new boolean[6];
			sideCorneredConnections = new boolean[6];
			for (int i = 0; i < 6; i++) {
				sideExternalConnections[i] = computeConnectTo(i);
				sideInternalConnections[i] = computeConnectInternallyTo(i);
				if (sideInternalConnections[i]) {
					sideExternalConnections[i] = true;
				}
				sideCorneredConnections[i] = computeConnectCornerTo(i);
				if (sideCorneredConnections[i]) {
					sideExternalConnections[i] = true;
				}
			}
			localJacketedConnection = computeConnectToLocalJacketed();
			if (oldJack != localJacketedConnection || !Arrays.equals(oldExt, sideExternalConnections) || !Arrays.equals(oldInt, sideInternalConnections) || !Arrays.equals(oldCor, sideCorneredConnections)) {
				hasNewConnections = true;
			}
		} else { // Calculate only jacketed connections
			boolean[] oldExt = sideExternalConnections.clone();
			for (int i = 0; i < 6; i++) {
				sideExternalConnections[i] = computeJacketedConnectTo(i);
			}
			if (!Arrays.equals(oldExt, sideExternalConnections)) {
				hasNewConnections = true;
			}
		}
	}

	public boolean computeConnectTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}
		// BasicWireUtils.canConnectThroughEdge(world(), x(), y(), z(), side,
		// absDir);
		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;
		TileMultipart t = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		boolean isMultiTile = false;
		if (t != null) {
			isMultiTile = true;
			TMultiPart tp = t.partMap(side);
			if (tp instanceof IConnectable) {
				return ((IConnectable) tp).connects(this, side, absDir);
			}
		}
		if (!isMultiTile) {
			return getExternalConnectionOveride(absDir);
		}
		return false;
	}

	public abstract boolean getExternalConnectionOveride(int absDir);

	public boolean computeConnectCornerTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}
		// BasicWireUtils.canConnectThroughEdge(world(), x(), y(), z(), side,
		// absDir);
		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;

		// if (!BasicWireUtils.canConnectThroughEdge(world(), x, y, z, side,
		// absDir ^ 1)) {
		// return false;
		// }
		x += ForgeDirection.VALID_DIRECTIONS[side].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[side].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[side].offsetZ;

		TileMultipart t = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		if (t != null) {
			TMultiPart tp = t.partMap(absDir ^ 1);
			if (tp instanceof IConnectable) {
				return ((IConnectable) tp).connectsAroundCorner(this, absDir ^ 1, side ^ 1);
			}
		}
		return false;
	}

	public boolean computeConnectInternallyTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}

		// TODO Inside Edge check here

		TMultiPart t = tile().partMap(absDir);
		if (t instanceof IConnectable) {
			return ((IConnectable) t).connectsToWireType(this);
		}
		return false;
	}

	public boolean computeConnectToLocalJacketed() {
		TMultiPart t = tile().partMap(PartMap.CENTER.i);
		if (t instanceof IConnectable) {
			return (((IConnectable) t).connects(this, -1, side));
		}
		return false;
	}

	public boolean computeJacketedConnectTo(int absDir) {
		TMultiPart t = tile().partMap(absDir);
		if (t instanceof IConnectable) {
			return (((IConnectable) t).connects(this, -1, absDir ^ 1));
		} else {
			// TODO Face open check to absDir here
			int x = x() + ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
			int y = y() + ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
			int z = z() + ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;
			TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
			if (tile != null) {
				TMultiPart p = tile.partMap(PartMap.CENTER.i);
				if (p instanceof IConnectable) {
					return ((IConnectable) p).connects(this, -1, absDir ^ 1);
				}
			}
		}
		return false;
	}

	/** START IConnectable **/
	// IConnectables are called when building connection arrays.
	// Basically used to check for anything blocking the direction.
	@Override
	public boolean connects(WirePart wire, int blockFace, int fromDirection) {
		if (!connectsToWireType(wire) || !wire.connectsToWireType(this)) {
			return false;
		}
		if (blockFace > -1) {
			// TODO Edge open checks here
		} else {
			// TODO Jacketed face checks here
		}
		return true;
	}

	@Override
	public boolean connectsAroundCorner(WirePart wire, int blockFace, int fromDirection) {
		return connects(wire, blockFace, fromDirection);
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		return wire.getWireType() == this.wireType;
	}

	/** END IConnectable **/

	// mask checks are called usually by renders, etc.
	public boolean maskConnects(int absDir) {
		return sideExternalConnections[absDir];
	}

	public boolean maskConnectsInternally(int absDir) {
		return sideInternalConnections[absDir];
	}

	public boolean maskConnectsAroundCorner(int absDir) {
		return sideCorneredConnections[absDir];
	}

	public boolean maskConnectsJacketed(int absDir) {
		return isJacketed && sideExternalConnections[absDir];
	}

	/**
	 * Notifies neighbours one or two blocks away, in the same pattern as most
	 * redstone updates.
	 */
	public void notifyExtendedNeighbors() {
		world().notifyBlocksOfNeighborChange(x(), y(), z(), tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x() + 1, y(), z(), tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x() - 1, y(), z(), tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x(), y() + 1, z(), tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x(), y() - 1, z(), tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x(), y(), z() + 1, tile().getBlockType().blockID);
		world().notifyBlocksOfNeighborChange(x(), y(), z() - 1, tile().getBlockType().blockID);
	}

	public int getVisualWireColour() {
		return 0xFFFFFF;
	}

	public Icon getSpecialIconForRender() {
		return null;
	}

	protected boolean debug(EntityPlayer ply) {
		return false;
	}

	protected void debugEffect_bonemeal() {
		world().playAuxSFX(2005, x(), y(), z(), 0);
	}

	protected void debugEffect_fireburst() {
		world().playAuxSFX(2004, x(), y(), z(), 0);
	}

	protected void debugEffect_potion() {
		world().playAuxSFX(2002, x(), y(), z(), 0);
	}

	protected void debugEffect_smoke() {
		world().playAuxSFX(2000, x(), y(), z(), 0);
	}

	/** START TILEMULTIPART INTERACTIONS **/
	@Override
	public float getStrength(MovingObjectPosition hit, EntityPlayer player) {
		return 4;
	}

	public ItemStack getItem() {
		if (isJacketed) {
			return new ItemStack(ProjectRed.itemPartJacketedWire, 1, wireType.ordinal());
		}
		return new ItemStack(ProjectRed.itemPartWire, 1, wireType.ordinal());
	}

	@Override
	public Iterable<ItemStack> getDrops() {
		return Arrays.asList(getItem());
	}

	@Override
	public ItemStack pickItem(MovingObjectPosition hit) {
		return getItem();
	}

	@Override
	public int getSlotMask() {
		return (1 << (!isJacketed ? side : PartMap.CENTER.i));
	}

	@Override
	public Cuboid6 getBounds() {
		if (isJacketed) {
			return getJacketBounds();
		}
		Cuboid6 base = new Cuboid6(0 / 8D, 0, 0 / 8D, 8 / 8D, wireType.thickness, 8 / 8D);
		return base.transform(sideRotations[side].at(center));
	}

	public Cuboid6 getJacketBounds() {
		return new Cuboid6(4 / 16D, 4 / 16D, 4 / 16D, 12 / 16D, 12 / 16D, 12 / 16D);
	}

	@Override
	public Iterable<Cuboid6> getCollisionBoxes() {
		if (isJacketed) {
			Cuboid6 base;
			base = getJacketBounds();
			return Arrays.asList(base);
		} else {
			return Arrays.asList();
		}
	}

	@Override
	public Iterable<IndexedCuboid6> getSubParts() {
		Cuboid6 base;
		if (isJacketed) {
			base = getJacketBounds();
		} else {
			base = new Cuboid6(0 / 8D, 0, 0 / 8D, 8 / 8D, wireType.thickness, 8 / 8D);
			base.transform(sideRotations[side].at(center));
		}
		return Arrays.asList(new IndexedCuboid6(0, base));
	}

	@Override
	public boolean occlusionTest(TMultiPart npart) {
		return NormalOcclusionTest.apply(this, npart);
	}

	@Override
	public Iterable<Cuboid6> getOcclusionBoxes() {
		Cuboid6 base;
		if (isJacketed) {
			base = getJacketBounds();
		} else {
			base = new Cuboid6(4 / 16D, 0 / 16D, 4 / 16D, 12 / 16D, 4 / 16D, 12 / 16D);
			base.transform(sideRotations[side].at(center));
		}
		return Arrays.asList(base);
	}

	@Override
	public int redstoneConductionMap() {
		return 0;
	}

	@Override
	public boolean solid(int arg0) {
		return false;
	}

	@Override
	public String getType() {
		return wireType.name;
	}

	@Override
	public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
		return false;
	}
}
