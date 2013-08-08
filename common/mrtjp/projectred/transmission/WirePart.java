package mrtjp.projectred.transmission;

import static codechicken.lib.vec.Rotation.sideRotations;
import static codechicken.lib.vec.Vector3.center;

import java.util.Arrays;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CommandDebug;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.raytracer.IndexedCuboid6;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.JCuboidPart;
import codechicken.multipart.JNormalOcclusion;
import codechicken.multipart.NormalOcclusionTest;
import codechicken.multipart.PartMap;
import codechicken.multipart.TFacePart;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import codechicken.multipart.scalatraits.TRedstoneTile;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

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
	protected boolean updateNextTick = false;
	public int side;
	public boolean isFirstTick = true;

	// Cached connection status of all sides, stored as absolute
	// ForgeDirections.
	private boolean[] sideExternalConnections = new boolean[6];
	private boolean[] sideInternalConnections = new boolean[6];
	private boolean[] sideCorneredConnections = new boolean[6];

	// True if this regular wire connects to a jacketed wire in this block.
	protected boolean localJacketedConnection = false;

	// If this is jacketed, connection mask is stored in
	// sideExternalConnections.
	protected boolean isJacketed = false;

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
		if (updateNextTick) {
			updateNextTick = false;
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

		updateNextTick = true;
	}

	public void updateChange() {
		tile().markRender();
		tile().markDirty();
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
		updateNextTick = true;
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
		updateNextTick = true;
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
				BasicUtils.dropItemFromLocation(world(), getItem(), false, null, side, 10, new BlockCoord(x(), y(), z()));
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
				updateNextTick = true;
			}
		} else { // Calculate only jacketed connections
			boolean[] oldExt = sideExternalConnections.clone();
			for (int i = 0; i < 6; i++) {
				sideExternalConnections[i] = computeJacketedConnectTo(i);
			}
			if (!Arrays.equals(oldExt, sideExternalConnections)) {
				updateNextTick = true;
			}
		}
	}

	public boolean computeConnectTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}

		// TODO add edge open check here

		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;
		TileMultipart t = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
		if (t != null) {
			TMultiPart tp = t.partMap(side);
			if (tp instanceof IConnectable) {
				return ((IConnectable) tp).connects(this, side, absDir);
			}
		}
		return getExternalConnectionOveride(absDir);
		// return false;
	}

	public abstract boolean getExternalConnectionOveride(int absDir);

	public boolean computeConnectCornerTo(int absDir) {
		if ((side & 6) == (absDir & 6)) {
			return false;
		}

		// TODO check edge here

		int x = x(), y = y(), z = z();
		x += ForgeDirection.VALID_DIRECTIONS[absDir].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[absDir].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[absDir].offsetZ;

		// TODO check the other edge here.

		x += ForgeDirection.VALID_DIRECTIONS[side].offsetX;
		y += ForgeDirection.VALID_DIRECTIONS[side].offsetY;
		z += ForgeDirection.VALID_DIRECTIONS[side].offsetZ;

		TileMultipart t = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
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

		if (tile() instanceof TRedstoneTile) {
			if (!((TRedstoneTile) tile()).redstoneConductionE(PartMap.edgeBetween(side, absDir))) {
				return false;
			}
		}

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
			TileMultipart tile = BasicUtils.getTileEntity(world(), new BlockCoord(x, y, z), TileMultipart.class);
			if (tile != null) {
				TMultiPart p = tile.partMap(PartMap.CENTER.i);
				if (p instanceof IConnectable) {
					return ((IConnectable) p).connects(this, -1, absDir ^ 1);
				}
			}
			return getExternalConnectionOveride(absDir);
		}
		//return false;
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

	protected abstract boolean debug(EntityPlayer ply);

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
		return 1 << PartMap.CENTER.i | 1 << 0 | 1 << 1 | 1 << 2 | 1 << 3 | 1 << 4 | 1 << 5;
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
		if (CommandDebug.WIRE_READING) {
			return debug(player);
		}
		return false;
	}

	public void renderStatic(RenderBlocks r) {
		WireRenderAssistant wra = new WireRenderAssistant();
		wra.x = x();
		wra.y = y();
		wra.z = z();
		wra.renderBlocks = r;
		wra.model = getWireType().wireMap;
		wra.wireIcon = (getSpecialIconForRender() == null ? getWireType().wireSprites[0] : getSpecialIconForRender());
		BasicRenderUtils.setFullColor();
		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.setBrightness(world(), x(), y(), z());
		CCRenderState.setColourOpaque(getVisualWireColour());
		wra.side = side;
		wra.setWireRenderState(this);
		wra.pushRender();
		BasicRenderUtils.setFullColor();
	}

	public void renderJacketStatic(RenderBlocks r) {
		WireRenderAssistant wra = new WireRenderAssistant();
		wra.x = x();
		wra.y = y();
		wra.z = z();
		wra.renderBlocks = r;
		wra.model = getWireType().jacketMap;
		wra.wireIcon = (getSpecialIconForRender() == null ? getWireType().wireSprites[0] : getSpecialIconForRender());
		wra.side = side;
		wra.setJacketRender(this);
		BasicRenderUtils.setFullColor();
		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.setBrightness(world(), x(), y(), z());
		wra.pushJacketFrameRender();
		CCRenderState.setColourOpaque(getVisualWireColour());
		wra.pushJacketWireRender();
		BasicRenderUtils.setFullColor();
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
		if (pass == 0) {
			if (isJacketed) {
				renderJacketStatic(null);
			} else {
				renderStatic(null);
			}
		}
	}

	@Override
	public void drawBreaking(RenderBlocks r) {
		if (isJacketed) {
			renderJacketStatic(r);
		} else {
			renderStatic(r);
		}
	}
}
