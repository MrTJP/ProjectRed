package mrtjp.projectred.transmission;

import static codechicken.lib.vec.Rotation.sideRotations;
import static codechicken.lib.vec.Vector3.center;

import java.util.ArrayList;
import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.GateLogic;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Coords;
import mrtjp.projectred.utils.Directions;
import net.minecraft.block.Block;
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
import codechicken.multipart.NormalOcclusionTest;
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
	private boolean isJacketed = false;

	/** START NEW LOGIC **/
	private boolean notifyNeighboursNextTick = false;
	public int side;
	public boolean isFirstTick = true;
	public boolean hasNewConnections = false;

	// Cached connection status of all sides, stored as absolute
	// ForgeDirections.
	private boolean[] sideExternalConnections = new boolean[6];
	private boolean[] sideInternalConnections = new boolean[6];
	private boolean[] sideCorneredConnections = new boolean[6];

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
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("ext" + i, sideExternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("int" + i, sideInternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			tag.setBoolean("cor" + i, sideCorneredConnections[i]);
		}
	}

	@Override
	public void load(NBTTagCompound tag) {
		super.load(tag);
		int type = tag.getByte("type");
		side = tag.getByte("side");
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
		if (notifyNeighboursNextTick) {
			notifyNeighboursNextTick = false;
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
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideExternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideInternalConnections[i]);
		}
		for (int i = 0; i < 6; i++) {
			packet.writeBoolean(sideCorneredConnections[i]);
		}
	}

	@Override
	public void readDesc(MCDataInput packet) {
		wireType = EnumWire.values()[packet.readByte()];
		side = packet.readByte();
		for (int i = 0; i < 6; i++) {
			sideExternalConnections[i] = packet.readBoolean();
		}
		for (int i = 0; i < 6; i++) {
			sideInternalConnections[i] = packet.readBoolean();
		}
		for (int i = 0; i < 6; i++) {
			sideCorneredConnections[i] = packet.readBoolean();
		}

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
		notifyExtendedNeighbors();
	}
	
	@Override
	public void onNeighborChanged() {
		if (BasicUtils.isClient(world())) {
			return;
		}
		int x = x() + ForgeDirection.getOrientation(side).offsetX;
		int y = y() + ForgeDirection.getOrientation(side).offsetY;
		int z = z() + ForgeDirection.getOrientation(side).offsetZ;

		if (!BasicWireUtils.canPlaceWireOnSide(world(), x, y, z, ForgeDirection.getOrientation(side ^ 1), false)) {
			BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new Coords(x(), y(), z()));
			tile().remPart(this);
		}
		computeConnections();
	}

	protected void computeConnections() {
		if (isFirstTick) {
			return;
		}
		boolean[] oldExt = sideExternalConnections.clone();
		boolean[] oldInt = sideInternalConnections.clone();
		boolean[] oldCor = sideCorneredConnections.clone();
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
		if (!BasicUtils.areArraysEqual(oldExt, sideExternalConnections) || !BasicUtils.areArraysEqual(oldInt, sideInternalConnections) || !BasicUtils.areArraysEqual(oldCor, sideCorneredConnections)) {
			hasNewConnections = true;
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
		if (!BasicWireUtils.canConnectThroughEdge(world(), x(), y(), z(), side, absDir)) {
			return false;
		}
		TMultiPart t = tile().partMap(absDir);
		if (t instanceof IConnectable) {
			return ((IConnectable) t).connectsToWireType(this) && connectsToWireType((WirePart) t);
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
		// Edge open checks here
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
		return 1 << side;
	}

	@Override
	public Cuboid6 getBounds() {
		// Cuboid6 base = new Cuboid6(0 / 8D, 0, 0 / 8D, 8 / 8D,
		// wireType.thickness, 8 / 8D);
		// return base.transform(sideRotations[side].at(center));
		return new Cuboid6(0, 0, 0, 0, 0, 0);
	}

	@Override
	public Iterable<IndexedCuboid6> getSubParts() {
		Cuboid6 base = new Cuboid6(0 / 8D, 0, 0 / 8D, 8 / 8D, wireType.thickness, 8 / 8D);
		base.transform(sideRotations[side].at(center));
		return Arrays.asList(new IndexedCuboid6(0, base));
	}

	@Override
	public boolean occlusionTest(TMultiPart npart) {
		return NormalOcclusionTest.apply(this, npart);
	}

	@Override
	public Iterable<Cuboid6> getOcclusionBoxes() {
		return Arrays.asList(getBounds());
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

	public Icon getSpecialIconForRender() {
		return null;
	}

	@Override
	public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
		return false;
	}
}
