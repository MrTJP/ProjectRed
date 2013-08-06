package mrtjp.projectred.transmission;

import java.lang.reflect.Field;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.PartMap;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class BasicWireUtils {

	public static final int FRONT = 0;
	public static final int BACK = 1;
	public static final int LEFT = 2;
	public static final int RIGHT = 3;


	/**
	 * Return the red-alloy power strength (0 to 255) the specified block is
	 * emitting towards the specified direction and side. onSide is used for
	 * emitters like wires; if a wire is on the NY side of the block containing
	 * it, then it will only emit power to side=NY, direction=NX/NZ/PX/PZ.
	 * onSide can be -1 to detect if power is coming through a block; the wire
	 * in the above example would use side=-1, direction=PY when detecting power
	 * coming from the block below it.
	 */
	public static short getPowerStrength(World w, int x, int y, int z, int toDirection, int onSide) {
		return getPowerStrength(w, x, y, z, toDirection, onSide, true);
	}

	/**
	 * Returns the red-alloy power strength (0 to 255) the specified block is
	 * emitting in the specified direction and side. onSide is -1 for jacketed.
	 */
	public static short getPowerStrength(World w, int x, int y, int z, int toDirection, int onSide, boolean countRedAlloyWire) {
		Block b = Block.blocksList[w.getBlockId(x, y, z)];
		int meta = w.getBlockMetadata(x, y, z);
		if (b == Block.redstoneWire)
			return (short) ((meta - 1) * 17);
		if (b == null)
			return 0;
		TileMultipart te = BasicUtils.getTileEntity(w, new BlockCoord(x, y, z), TileMultipart.class);
		if (te != null && onSide == -1) {
			TMultiPart part = te.partMap(PartMap.CENTER.i);
			if (!countRedAlloyWire && part instanceof RedAlloyWirePart) {
				return 0;
			}
			if (part != null && part instanceof IRedstoneEmitter) {
				return ((IRedstoneEmitter) part).getEmittedSignalStrength(onSide, toDirection);
			}
		}
		if (te != null && onSide > -1) {
			TMultiPart part = te.partMap(onSide);
			if (!countRedAlloyWire && part instanceof RedAlloyWirePart) {
				return 0;
			}
			if (part != null && part instanceof IRedstoneEmitter) {
				return ((IRedstoneEmitter) part).getEmittedSignalStrength(onSide, toDirection);
			}
		}

		// respond to weak power, or strong power, or strong power applied
		// through a block
		int pl = b.isProvidingStrongPower(w, x, y, z, toDirection ^ 1);
		if (pl > 0) {
			return (short) (pl * 17);
		}

		if (w.isBlockNormalCube(x, y, z)) {
			try {
				wiresProvidePower.set(Block.redstoneWire, false);
				pl = w.getBlockPowerInput(x, y, z);
				wiresProvidePower.set(Block.redstoneWire, true);
			} catch (Throwable t) {
			}
			;
			if (pl > 0) {
				return (short) (pl * 17);
			}
		} else {
			pl = b.isProvidingWeakPower(w, x, y, z, toDirection ^ 1);
			if (pl > 0)
				return (short) (pl * 17);
		}

		return 0;
	}

	/**
	 * Used to prevent 2 disabled wires on opposite sides of a block from
	 * keeping eachother on through a strong block signal.
	 */
	private static Field wiresProvidePower = BlockRedstoneWire.class.getDeclaredFields()[0];
	static {
		try {
			wiresProvidePower.setAccessible(true);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public static boolean wiresProvidePower() {
		try {
			return wiresProvidePower.getBoolean(Block.redstoneWire);
		} catch (Throwable t) {
			return false;
		}
	}

	public static boolean canPlaceWireOnSide(World w, int x, int y, int z, ForgeDirection side, boolean _default) {
		if (!w.blockExists(x, y, z)) {
			return _default;
		}

		Block b = Block.blocksList[w.getBlockId(x, y, z)];
		if (b == null)
			return false;
		// Manual list of allowed blocks that wire can sit on.
		if (b == Block.glowStone || b == Block.pistonBase || b == Block.pistonStickyBase || b == Block.pistonMoving || b == Block.glass)
			return true;
		return b.isBlockSolidOnSide(w, x, y, z, side);
	}
}
