package mrtjp.projectred.transmission;

import static codechicken.lib.vec.Rotation.sideRotations;
import static codechicken.lib.vec.Vector3.center;

import java.util.ArrayList;
import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.integration.GateLogic;
import mrtjp.projectred.interfaces.wiring.IConnectable;
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

public abstract class WirePart extends JCuboidPart implements IConnectable, TFacePart, JNormalOcclusion {

	private EnumWire wireType;

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

	public void setWireType(EnumWire type) {
		wireType = type;
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
		if (type < 0 || type >= EnumWire.VALUES.length) {
			wireType = EnumWire.RED_ALLOY;
		} else {
			wireType = EnumWire.VALUES[type];
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
		if (notifyNeighboursNextTick) {
			notifyNeighboursNextTick = false;
			updateChange();
		}
		if (isFirstTick) {
			isFirstTick = false;
			computeConnections();
		}
		if (hasNewConnections) {
			hasNewConnections = false;
			updateChange();
		}
		super.update();
	}

	@Override
	public void writeDesc(MCDataOutput packet) {
		super.writeDesc(packet);
		NBTTagCompound tag = new NBTTagCompound();
		save(tag);
		packet.writeNBTTagCompound(tag);
	}

	@Override
	public void readDesc(MCDataInput packet) {
		super.readDesc(packet);
		NBTTagCompound tag = packet.readNBTTagCompound();
		load(tag);
	}

	public void updateChange() {
		tile().markDirty();
		tile().notifyPartChange();
		// notifyExtendedNeighbours();
		if (BasicUtils.isServer(world())) {
			sendDescUpdate();
		}
	}

	public final EnumWire getWireType() {
		return wireType;
	}

	@Override
	public void onPartChanged() {
		computeConnections();
	}

	@Override
	public void onNeighborChanged() {
		if (BasicUtils.isClient(world())) {
			return;
		}
		Coords localCoord = new Coords(x(), y(), z());
		localCoord.orientation = ForgeDirection.getOrientation(side);
		localCoord.moveForwards(1);
		Block supporter = Block.blocksList[world().getBlockId(localCoord.x, localCoord.y, localCoord.z)];
		if (!BasicWireUtils.canPlaceWireOnSide(world(), localCoord.x, localCoord.y, localCoord.z, localCoord.orientation.getOpposite(), false)) {
			BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new Coords(x(), y(), z()));
			tile().remPart(this);
		}
		computeConnections();
	}

	private void computeConnections() {
		if (isFirstTick) {
			return;
		}
		boolean[] oldExt = sideExternalConnections;
		boolean[] oldInt = sideInternalConnections;
		boolean[] oldCor = sideCorneredConnections;
		sideExternalConnections = new boolean[6];
		sideInternalConnections = new boolean[6];
		sideCorneredConnections = new boolean[6];
		for (int i = 0; i < 6; i++) {
			sideExternalConnections[i] = computeConnectTo(i);
			sideInternalConnections[i] = computeConnectInternallyTo(i);
			sideCorneredConnections[i] = computeConnectCornerTo(i);
		}
		if (!oldExt.equals(sideExternalConnections) || !oldInt.equals(sideInternalConnections) || !oldCor.equals(sideCorneredConnections)) {
			hasNewConnections = true;
		}
	}

	public boolean computeConnectTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}
		//BasicWireUtils.canConnectThroughEdge(world(), x(), y(), z(), side, absDir);
		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;
		TileMultipart t = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		if (t != null) {
			TMultiPart tp = t.partMap(side);
			if (tp instanceof IConnectable) {
				return ((IConnectable) tp).connects(this, side, absDir);
			}
		} else {
		}
		return false;
	}

	public boolean computeConnectCornerTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}
		//BasicWireUtils.canConnectThroughEdge(world(), x(), y(), z(), side, absDir);
		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;

		//if (!BasicWireUtils.canConnectThroughEdge(world(), x, y, z, side, absDir ^ 1)) {
		//	return false;
		//}
		x += ForgeDirection.VALID_DIRECTIONS[side].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[side].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[side].offsetZ;
		
		TileMultipart t = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		if (t != null) {
			TMultiPart tp = t.partMap(side);
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
		System.out.println("thier type: " + wire.wireType.name);
		System.out.println("our type: " + wireType.name);
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
	public void notifyExtendedNeighbours() {
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
		return 2;
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
		Cuboid6 base = new Cuboid6(0 / 8D, 0, 0 / 8D, 8 / 8D, wireType.thickness, 8 / 8D);
		return base.transform(sideRotations[side].at(center));
	}

	@Override
	public Iterable<IndexedCuboid6> getSubParts() {
		return Arrays.asList(new IndexedCuboid6(0, getBounds()));
	}

	@Override
	public boolean occlusionTest(TMultiPart npart) {
		return NormalOcclusionTest.apply(this, npart);
	}

	@Override
	public Iterable<Cuboid6> getOcclusionBoxes() {
		// return Arrays.asList(getBounds());
		return new ArrayList();
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
		for (int i = 0; i < 6; i++) {
			int x = i+1;
			System.out.println("SideConnections " + x + " : " + sideExternalConnections[i]);
		}
		return false;
	}
}
