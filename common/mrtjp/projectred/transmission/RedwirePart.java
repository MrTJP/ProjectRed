package mrtjp.projectred.transmission;

import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CommandDebug;
import mrtjp.projectred.core.Coords;
import mrtjp.projectred.core.Messenger;
import net.minecraft.block.Block;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.IFaceRedstonePart;
import codechicken.multipart.PartMap;
import codechicken.multipart.RedstoneInteractions;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class RedwirePart extends WirePart implements IRedstoneEmitter, IFaceRedstonePart {

	private short MAX_STRENGTH = 255;

	private short strength = 0;
	private short strengthFromNonWireBlocks = 0;
	protected boolean syncSignalStrength;
	protected boolean connectToBlockBelow;

	private boolean isUpdatingStrength, recursiveUpdatePending;
	private static boolean blockUpdateCausedByAlloyWire = false;
	private static boolean dontEmitPower = false;

	private boolean sceduleConnectedThingsUpdate = false;


	public RedwirePart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
	}

	@Override
	public void save(NBTTagCompound tag) {
		super.save(tag);
		tag.setShort("strength", strength);
		tag.setShort("strengthNWB", strengthFromNonWireBlocks);
	}

	@Override
	public void load(NBTTagCompound tag) {
		super.load(tag);
		strength = tag.getShort("strength");
		strengthFromNonWireBlocks = tag.getShort("strengthNWB");
	}

	@Override
	public void writeDesc(MCDataOutput packet) {
		super.writeDesc(packet);
		packet.writeShort(strength > 0 ? strength : 0);
		packet.writeShort(strengthFromNonWireBlocks > 0 ? strengthFromNonWireBlocks : 0);
	}

	@Override
	public void readDesc(MCDataInput packet) {
		super.readDesc(packet);
		strength = packet.readShort();
		strengthFromNonWireBlocks = packet.readShort();
		if (!isFirstTick) {
			updateConnectedThings();
		} else {
			sceduleConnectedThingsUpdate = true;
		}
	}

	@Override
	public void update() {
		super.update();
		if (sceduleConnectedThingsUpdate) {
			sceduleConnectedThingsUpdate = false;
			updateConnectedThings();
		}
	}

	@Override
	public void onNeighborChanged() {
		if (blockUpdateCausedByAlloyWire) {
			// When we update a RedAlloyWire, it starts updating its neighbors,
			// including us. Dont update again, because we are the one that told
			// it to update in the first place.
			return;
		}

		super.onNeighborChanged();
		updateSignal(null);
	}

	/**
	 * This should return the max signal given off to the side.
	 */
	private int updateStrengthFromBlock(int x, int y, int z, int odir, int testside, int newStrength) {
		int thisStrength = getInputPowerStrength(x, y, z, odir, testside, true);
		newStrength = Math.max(newStrength, Math.min(thisStrength - 1, MAX_STRENGTH));
		if (BasicUtils.isServer(world())) {
			thisStrength = getInputPowerStrength(x, y, z, odir, testside, false);
			strengthFromNonWireBlocks = (short) Math.max(strengthFromNonWireBlocks, Math.min(thisStrength - 1, MAX_STRENGTH));
		}
		return newStrength;
	}

	public int getInputPowerStrength(int x, int y, int z, int outDir, int testside, boolean countWires) {
		return BasicWireUtils.getPowerStrength(world(), x, y, z, outDir, testside, countWires);
	}

	/**
	 * Asks all surrounding neighbor Wires/blocks (including indirect) for
	 * signal strength, 0 - 255. It uses the connection arrays to see what side
	 * it should check. It returns the max strength it found.
	 */
	private int updateInputStrength() {
		if (BasicUtils.isServer(world())) {
			strengthFromNonWireBlocks = 0;
		}
		int newStrength = 0;

		if (isJacketed) {  // Only update from the 6 sides, from either connected wires or other JWires on nearby blocks.
			for (int i = 0; i < 6; i++) {
				if (maskConnectsJacketed(i)) {
					if (tile().partMap(i) != null) {
						newStrength = updateStrengthFromBlock(x(), y(), z(), i ^ 1, i, newStrength);
					} else {
						ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[i];
						int x = x() + fd.offsetX;
						int y = y() + fd.offsetY;
						int z = z() + fd.offsetZ;
						newStrength = updateStrengthFromBlock(x, y, z, i ^ 1, -1, newStrength);
					}
				}
			}
		} else {
			if (localJacketedConnection) {
				int x = x(), y = y(), z = z();
				newStrength = updateStrengthFromBlock(x, y, z, side, -1, newStrength);
			} else { // Update local Jacketed if there is one, even if we dont connect.
				TMultiPart t = tile().partMap(PartMap.CENTER.i);
				if (t instanceof WirePart) {
					((WirePart)t).updateChange();
				}
			}
			if (connectToBlockBelow) {
				ForgeDirection wdir = ForgeDirection.VALID_DIRECTIONS[side];
				int x = x() + wdir.offsetX;
				int y = y() + wdir.offsetY;
				int z = z() + wdir.offsetZ;

				int thisStrength = BasicWireUtils.getPowerStrength(world(), x, y, z, side ^ 1, -1, false);
				newStrength = Math.max(newStrength, Math.min(thisStrength - 1, MAX_STRENGTH));

				if (BasicUtils.isServer(world())) {
					strengthFromNonWireBlocks = (short) Math.max(strengthFromNonWireBlocks, Math.min(thisStrength - 1, MAX_STRENGTH));
				}
			}
			for (int dir = 0; dir < 6; dir++) {
				if (maskConnectsInternally(dir)) {
					int x = x(), y = y(), z = z();
					TMultiPart t = tile().partMap(dir);
					newStrength = updateStrengthFromBlock(x, y, z, side, dir, newStrength);
					continue;
				}
				if (maskConnectsAroundCorner(dir)) {
					int x = x(), y = y(), z = z();
					ForgeDirection fdir = ForgeDirection.VALID_DIRECTIONS[dir];
					x += fdir.offsetX;
					y += fdir.offsetY;
					z += fdir.offsetZ;
					fdir = ForgeDirection.VALID_DIRECTIONS[side];
					x += fdir.offsetX;
					y += fdir.offsetY;
					z += fdir.offsetZ;
					int outputdir = side ^ 1;
					int testside = dir ^ 1;
					newStrength = updateStrengthFromBlock(x, y, z, outputdir, testside, newStrength);
					continue;
				}
				if (maskConnects(dir)) {
					int x = x(), y = y(), z = z();
					ForgeDirection fdir = ForgeDirection.VALID_DIRECTIONS[dir];
					x += fdir.offsetX;
					y += fdir.offsetY;
					z += fdir.offsetZ;
					int outputdir = dir ^ 1;
					int testside = side;
					newStrength = updateStrengthFromBlock(x, y, z, outputdir, testside, newStrength);
					continue;
				}
			}
		}
		if (BasicUtils.isClient(world())) {
			newStrength = Math.max(newStrength, strengthFromNonWireBlocks);
		}
		return newStrength;
	}

	private void updateConnectedThings() {
		if (CommandDebug.WIRE_DEBUG_PARTICLES) {
			debugEffect_bonemeal();
		}
		if (isJacketed) {
			for (int i = 0; i < 6; i++) {
				if (maskConnectsJacketed(i)) {
					TMultiPart t = null;
					if (tile().partMap(i) != null) {
						t = tile().partMap(i);
					} else {
						ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[i];
						int x = x() + fd.offsetX;
						int y = y() + fd.offsetY;
						int z = z() + fd.offsetZ;
						TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
						if (tile != null) {
							t = tile.partMap(PartMap.CENTER.i);
						}
					}
					if (t instanceof RedwirePart) {
						((RedwirePart) t).updateSignal(this);
					} else if (t instanceof BundledCablePart && this instanceof InsulatedRedAlloyPart) {
						((BundledCablePart) t).onBundledInputChanged();
					}
				}
			}

		} else {
			if (localJacketedConnection) {
				TMultiPart t = tile().partMap(PartMap.CENTER.i);
				if (t instanceof RedwirePart) {
					((RedwirePart) t).updateSignal(this);
				} else if (t instanceof BundledCablePart && this instanceof InsulatedRedAlloyPart) {
					((BundledCablePart) t).onBundledInputChanged();
				}
			}
			for (int dir = 0; dir < 6; dir++) {
				ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
				int x = x() + fd.offsetX, y = y() + fd.offsetY, z = z() + fd.offsetZ;
				if (maskConnects(dir)) {
					if (world().getBlockId(x, y, z) == tile().getBlockType().blockID) {
						TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
						if (tile != null) {
							TMultiPart t = tile.partMap(side);
							if (t instanceof RedwirePart) {
								((RedwirePart) t).updateSignal(this);
							} else if (t instanceof BundledCablePart && this instanceof InsulatedRedAlloyPart) {
								((BundledCablePart) t).onBundledInputChanged();
							}

						}
					}
				}
				if (maskConnectsAroundCorner(dir)) {
					fd = ForgeDirection.VALID_DIRECTIONS[side];
					x += fd.offsetX;
					y += fd.offsetY;
					z += fd.offsetZ;
					if (world().getBlockId(x, y, z) == tile().getBlockType().blockID) {
						TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
						if (tile != null) {
							TMultiPart t = tile.partMap(dir ^ 1);
							if (t instanceof RedwirePart) {
								((RedwirePart) t).updateSignal(this);
							} else if (t instanceof BundledCablePart && this instanceof InsulatedRedAlloyPart) {
								((BundledCablePart) t).onBundledInputChanged();
							}
						}
					}
				}
				if (maskConnectsInternally(dir)) {
					TMultiPart t = tile().partMap(dir);
					if (t instanceof RedwirePart) {
						((RedwirePart) t).updateSignal(this);
					} else if (t instanceof BundledCablePart && this instanceof InsulatedRedAlloyPart) {
						((BundledCablePart) t).onBundledInputChanged();
					}
				}
			}
		}
	}

	protected void updateSignal(RedwirePart source) {
		if (BasicUtils.isClient(world()) && !syncSignalStrength)
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
		boolean wasFirstServerChange = !world().isRemote && source == null;

		int oldStrengthFromNonWireBlocks = strengthFromNonWireBlocks;

		isUpdatingStrength = true;

		int newStrength;
		int startStrength = strength;

		do {
			recursiveUpdatePending = false;

			int prevStrength = strength;
			strength = 0;
			newStrength = updateInputStrength();

			if (newStrength < prevStrength) {
				// this is a huge optimization - it results in a "pulse" of 0
				// strength being sent down the wire
				// when turning off. if there is another source of power further
				// down the wire, that one will block
				// the pulse and propagate backwards, turning the wires back on
				// with the correct strength in 2 updates.
				updateConnectedThings();
				newStrength = updateInputStrength();
			}

			strength = (short) newStrength;

			if (strength != prevStrength)
				updateConnectedThings();

		} while (recursiveUpdatePending);

		isUpdatingStrength = false;

		if (strength != startStrength) {
			if (BasicUtils.isServer(world())) {
				blockUpdateCausedByAlloyWire = true;
				notifyExtendedPowerableNeighbours();
				blockUpdateCausedByAlloyWire = false;
			}
			if (syncSignalStrength && (BasicUtils.isClient(world()) || wasFirstServerChange || strengthFromNonWireBlocks != oldStrengthFromNonWireBlocks)) {
				if (!world().isRemote && CommandDebug.WIRE_LAG_PARTICLES)
					debugEffect_bonemeal();
				updateChange();
			}

		} else if (syncSignalStrength && BasicUtils.isServer(world()) && oldStrengthFromNonWireBlocks != strengthFromNonWireBlocks) {
			updateChange();
			if (CommandDebug.WIRE_LAG_PARTICLES)
				debugEffect_bonemeal();
		}
	}

	@Override
	public boolean getExternalConnectionOveride(int absDir) {
		int x = x(), y = y(), z = z();
		x += ForgeDirection.getOrientation(absDir).offsetX;
		y += ForgeDirection.getOrientation(absDir).offsetY;
		z += ForgeDirection.getOrientation(absDir).offsetZ;
		Block b = Block.blocksList[world().getBlockId(x, y, z)];
		if (b == null || b.isAirBlock(world(), x, y, z)) {
			return false;
		}
		if (b.canProvidePower()) {
			return true;
		}
		if (absDir >= 0 && absDir < 6) {
			int mappedSide = RedstoneInteractions.vanillaSideMap()[absDir];
			if (mappedSide > -2) {
				return b.canConnectRedstone(world(), x, y, z, RedstoneInteractions.vanillaSideMap()[absDir]);
			}
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
	public short getEmittedSignalStrength(int onSide, int dir) { // IRedstoneEmittingTile
		if (onSide == -1) {
			return maskConnectsJacketed(dir) ? getRedstoneSignalStrength() : 0;
		}
		if (dir == (side ^ 1) && localJacketedConnection) {
			return getRedstoneSignalStrength();
		}
		return maskConnects(dir) ? getRedstoneSignalStrength() : 0;
	}

	public boolean canProvideStrongPowerInDirection(int dir) {
		return BasicWireUtils.wiresProvidePower() && connectToBlockBelow && side == (dir);
	}

	public void notifyExtendedPowerableNeighbours() {
		boolean any = false;

		for (int k = 0; k < 6; k++) {
			ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[k];
			int x = x() + fd.offsetX;
			int y = y() + fd.offsetY;
			int z = z() + fd.offsetZ;

			boolean causedBlockUpdate = false;

			if (canProvideWeakPowerInDirection(k)) {
				causedBlockUpdate = true;
				world().notifyBlockOfNeighborChange(x, y, z, tile().getBlockType().blockID);
			}

			if (canProvideStrongPowerInDirection(k)) {
				causedBlockUpdate = true;
				world().notifyBlocksOfNeighborChange(x, y, z, tile().getBlockType().blockID);
			}

			if (!causedBlockUpdate) {
				Block block = Block.blocksList[world().getBlockId(x, y, z)];
				if (block != null) {
					world().markBlockForUpdate(x, y, z);
				}
			}

			any |= causedBlockUpdate;
		}

		for (ForgeDirection d : ForgeDirection.VALID_DIRECTIONS) {
			int x = x() + d.offsetX;
			int y = y() + d.offsetY;
			int z = z() + d.offsetZ;
			if (canProvideWeakPowerInDirection(d.ordinal())) {
				any = true;
				world().notifyBlockOfNeighborChange(x, y, z, tile().getBlockType().blockID);
			}
		}
		for (ForgeDirection d : ForgeDirection.VALID_DIRECTIONS) {
			int x = x() - d.offsetX;
			int y = y() - d.offsetY;
			int z = z() - d.offsetZ;
			if (canProvideWeakPowerInDirection(d.ordinal())) {
				any = true;
				world().notifyBlockOfNeighborChange(x, y, z, tile().getBlockType().blockID);
			}
		}

		if (any && CommandDebug.WIRE_LAG_PARTICLES)
			debugEffect_fireburst();
	}

	public boolean canProvideWeakPowerInDirection(int dir) {
		return BasicWireUtils.wiresProvidePower() && maskConnects(dir);
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		return wire instanceof RedwirePart;
	}

	/**
	 * Returns the vanilla redstone strength from 0 to 15.
	 */
	public byte getVanillaRedstoneStrength() {
		return (byte) (getRedstoneSignalStrength() / 17);
	}

	@Override
	protected boolean debug(EntityPlayer ply) {
		ply.sendChatToPlayer(ChatMessageComponent.func_111077_e((world().isRemote ? "Client" : "Server") + " signal strength: " + strength + ", nwb: " + strengthFromNonWireBlocks));
		super.debug(ply);
		return true;
	}

	@Override
	public boolean canConnectRedstone(int absDir) {
		return maskConnects(absDir);
	}

	@Override
	public int strongPowerLevel(int absDir) {
		return canProvideStrongPowerInDirection(absDir) ? getVanillaRedstoneStrength() : 0;
	}

	@Override
	public int weakPowerLevel(int absDir) {
		return canProvideWeakPowerInDirection(absDir) ? getVanillaRedstoneStrength() : 0;
	}

	@Override
	public int getFace() {
		return side;
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
