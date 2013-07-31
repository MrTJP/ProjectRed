package mrtjp.projectred.transmission;

import static codechicken.lib.vec.Rotation.sideRotations;
import static codechicken.lib.vec.Vector3.center;

import java.util.ArrayList;
import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.multipart.microblocks.Part;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.BasicWireUtils;
import mrtjp.projectred.utils.Coords;
import mrtjp.projectred.utils.Rotator;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.vec.Cuboid6;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;

public class TileWire extends JCuboidPart implements TFacePart, JNormalOcclusion {

	private EnumWire wireType;

	/** START NEW LOGIC **/
	private boolean notifyNeighboursNextTick = false;
	public int side;
	// Cached connection status of all sides, stored as absolute
	// ForgeDirections.
	private boolean[] sideExternalConnections = new boolean[6];
	private boolean[] sideInternalConnections = new boolean[6];
	private boolean[] sideCorneredConnections = new boolean[6];

	@Override
	public void save(NBTTagCompound tag) {
		super.save(tag);
		tag.setByte("type", (byte) wireType.ordinal());
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
		super.update();
	}

	@Override
	public void writeDesc(MCDataOutput packet) {
		NBTTagCompound tag = new NBTTagCompound();
		computeConnections();
		tag.setByte("t", (byte) wireType.ordinal());
		packet.writeNBTTagCompound(tag);
	}

	@Override
	public void readDesc(MCDataInput packet) {
		NBTTagCompound tag = packet.readNBTTagCompound();
		wireType = EnumWire.VALUES[tag.getByte("t")];
		updateChange();
	}
	
	public void updateChange() {
		tile().markDirty();
		tile().notifyPartChange();
		notifyExtendedNeighbours();
		sendDescUpdate();
	}

	public final EnumWire getWireType() {
		return wireType;
	}

	@Override
	public void onPartChanged() {
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

	@Override
	public void onNeighborChanged() {
		onPartChanged();
	}
	
	private void computeConnections() {
		// TODO ask other methods, and build the connection arrays.
	}
	
	public boolean canConnectTo(int absDir) {
		return false;
	}
	
	public boolean canConnectCornerTo(int absDir) {
		return false;
	}
	
	public boolean canConnectInternallyTo(int absDir) {
		return false;
	}

	/**
	 * Notifies neighbours one or two blocks away, in the same pattern as most
	 * redstone updates.
	 */
	// TODO wrong, use TMultipart block id
	public void notifyExtendedNeighbours() {
		world().notifyBlocksOfNeighborChange(x(), y(), z(), ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x() + 1, y(), z(), ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x() - 1, y(), z(), ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x(), y() + 1, z(), ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x(), y() - 1, z(), ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x(), y(), z() + 1, ProjectRed.blockWire.blockID);
		world().notifyBlocksOfNeighborChange(x(), y(), z() - 1, ProjectRed.blockWire.blockID);
	}

	/**
	 * Override this to change the constraints on wire connections (for example,
	 * insulated wire only connects to the same colour). Must be symmetric
	 * (a.canConnectToWire(b) == b.canConnectToWire(a))
	 */
	protected boolean canConnectToWire(TileWire wire) {
		return wire.getWireType() == this.wireType;
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
		return null;
	}
}
