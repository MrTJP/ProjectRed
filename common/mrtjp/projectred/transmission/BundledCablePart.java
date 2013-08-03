package mrtjp.projectred.transmission;

import java.util.Arrays;

import mrtjp.projectred.multipart.wiring.CommandDebug;
import mrtjp.projectred.utils.BasicRenderUtils;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Coords;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.ChatMessageComponent;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class BundledCablePart extends WirePart implements IBundledEmitter, IBundledUpdatable {
	
	private boolean sceduleConnectedThingsUpdate = false;
	
	public BundledCablePart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
	}

	private byte[] strength = new byte[16];

	@Override
	public boolean getExternalConnectionOveride(int absDir) {
		return false;
	}

	@Override
	public void save(NBTTagCompound tag) {
		super.save(tag);
		tag.setByteArray("strength", strength);
	}

	@Override
	public void load(NBTTagCompound tag) {
		super.load(tag);
		strength = tag.getByteArray("strength");
	}

	@Override
	public boolean connectsToWireType(WirePart wire) {
		return super.connectsToWireType(wire) || wire instanceof InsulatedRedAlloyPart || getWireType() == wire.getWireType() || wire.getWireType() == EnumWire.BUNDLED_N || (getWireType() == EnumWire.BUNDLED_N && wire instanceof BundledCablePart);
	}

	@Override
	public void update() {
		super.update();
		if (sceduleConnectedThingsUpdate) {
			sceduleConnectedThingsUpdate = false;
			updateConnectedThings();
		}
	}

	private byte[] oldStrength = new byte[16];

	private boolean isUpdating;
	private boolean recursiveUpdatePending = true;

	private void updateStrengthFromBlock(int x, int y, int z, int side, int dir) {
		TileMultipart tmp = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
		TMultiPart te = tmp.partMap(side);
		if (te instanceof InsulatedRedAlloyPart) {
			InsulatedRedAlloyPart o = (InsulatedRedAlloyPart) te;
			int o_strength = o.getRedstoneSignalStrength() - 1;
			int colour = o.getInsulatedWireColour();
			if ((strength[colour] & 0xFF) < o_strength)
				strength[colour] = (byte) o_strength;
		} else if (te instanceof IBundledEmitter) {
			byte[] o_strength = ((IBundledEmitter) te).getBundledCableStrength(side, dir);
			if (o_strength != null) {
				for (int k = 0; k < 16; k++) {
					int o_c_strength = (o_strength[k] & 0xFF) - 1;
					if ((strength[k] & 0xFF) < o_c_strength)
						strength[k] = (byte) o_c_strength;
				}
			}
		}
	}

	private void updateStrengthFromSurroundingBlocks() {
		byte[] temp = oldStrength.clone();
		oldStrength = strength.clone();
		strength = temp.clone();
		Arrays.fill(strength, (byte) 0);

		for (int dir = 0; dir < 6; dir++) {
			if (maskConnectsInternally(dir)) {
				int x = x(), y = y(), z = z();
				updateStrengthFromBlock(x, y, z, dir, side);
				continue;
			}
			ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
			int x = x() + fd.offsetX;
			int y = y() + fd.offsetY;
			int z = z() + fd.offsetZ;

			if (maskConnectsAroundCorner(dir)) {
				fd = ForgeDirection.VALID_DIRECTIONS[side];
				x += fd.offsetX;
				y += fd.offsetY;
				z += fd.offsetZ;
				int oside = dir ^ 1;
				int odir = side ^ 1;
				updateStrengthFromBlock(x, y, z, oside, odir);
				continue;
			}
			if (maskConnects(dir)) {
				int oside = side;
				int odir = dir ^ 1;
				updateStrengthFromBlock(x, y, z, oside, odir);
				continue;
			}
		}

		// TODO jacketed wires stuff
	}

	private void updateStrength() {
		if (BasicUtils.isClient(world()))
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
		if (CommandDebug.WIRE_DEBUG_PARTICLES)
			debugEffect_bonemeal();
		// TODO add jacketed wire updating here, and return;
		for (int dir = 0; dir < 6; dir++) {
			ForgeDirection fd = ForgeDirection.VALID_DIRECTIONS[dir];
			int x = x() + fd.offsetX;
			int y = y() + fd.offsetY;
			int z = z() + fd.offsetZ;
			if (maskConnects(dir)) {
				if (world().getBlockId(x, y, z) == tile().getBlockType().blockID) {
					TileMultipart tile = BasicUtils.getTileEntity(world(), new Coords(x, y, z), TileMultipart.class);
					if (tile != null) {
						TMultiPart t = tile.partMap(side);
						if (t instanceof IBundledUpdatable) {
							((IBundledUpdatable) t).onBundledInputChanged();
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
						if (t instanceof IBundledUpdatable) {
							((IBundledUpdatable) t).onBundledInputChanged();
						}
					}
				}
			}
			if (maskConnectsInternally(dir)) {
				TMultiPart t = tile().partMap(dir);
				if (t instanceof IBundledUpdatable) {
					((IBundledUpdatable) t).onBundledInputChanged();
				}
			}
		}
	}

	@Override
	public void onBundledInputChanged() {
		updateStrength();
		updateChange();
	}

	@Override
	public void onNeighborChanged() {
		super.onNeighborChanged();
		updateConnectedThings();
		updateStrength();
		updateChange();
	}
	
	@Override
	public void onPartChanged() {
		super.onPartChanged();
		updateStrength();
		updateConnectedThings();
		updateChange();
	}
	
	@Override
	public void onAdded() {
		super.onAdded();
	}
	
	@Override
	public void updateChange() {
		super.updateChange();
	}

	@Override
	public byte[] getBundledCableStrength(int blockFace, int direction) {
		return maskConnects(direction) ? strength : null;
	}

	@Override
	protected boolean debug(EntityPlayer ply) {
		if (BasicUtils.isServer(world())) {
			int[] i = new int[16];
			for (int k = 0; k < 16; k++)
				i[k] = strength[k] & 0xFF;
			ply.sendChatToPlayer(ChatMessageComponent.func_111077_e("Bundled cable strength: " + Arrays.toString(i)));
		}
		return true;
	}

	public void renderStatic(RenderBlocks r) {
		WireRenderAssistant wra = new WireRenderAssistant();
		wra.x = x();
		wra.y = y();
		wra.z = z();
		wra.renderBlocks = r;
		wra.model = getWireType().wireMap;
		wra.wireIcon = (getSpecialIconForRender() == null ? getWireType().wireSprites[0] : getSpecialIconForRender());
		Tessellator.instance.setColorRGBA(255, 255, 255, 255);
		BasicRenderUtils.bindTerrainResource();
		CCRenderState.reset();
		CCRenderState.setBrightness(world(), x(), y(), z());
		CCRenderState.setColourOpaque(getVisualWireColour());
		wra.side = side;
		wra.setWireRenderState(this);
		wra.pushRender();
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
		if (pass == 0) {
			renderStatic(null);
		}
	}

	@Override
	public void drawBreaking(RenderBlocks r) {
		renderStatic(r);
	}

	@Override
	public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack held) {
		if (CommandDebug.WIRE_READING || true) {
			return debug(player);
		}
		return false;
	}
}
