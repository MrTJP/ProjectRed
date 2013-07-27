package mrtjp.projectred.utils;

import java.lang.reflect.Field;

import mrtjp.projectred.interfaces.wiring.IRedstoneEmitter;
import mrtjp.projectred.multipart.microblocks.IMicroblockCoverSystem;
import mrtjp.projectred.multipart.microblocks.IMicroblockSupporterTile;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.multipart.wiring.wires.TilePlainRedAlloy;
import net.minecraft.block.Block;
import net.minecraft.block.BlockRedstoneWire;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;

public class BasicWireUtils {
	private static final int plainRedAlloyMeta = EnumWire.CLASS_TO_META.get(TilePlainRedAlloy.class);

	/** Magic array of numbers */
	private static int[][] rotationMap = { { 9, 9, 4, 5, 3, 2 }, { 9, 9, 5, 4, 2, 3 }, { 5, 4, 9, 9, 0, 1 }, { 4, 5, 9, 9, 1, 0 }, { 2, 3, 1, 0, 9, 9 }, { 3, 2, 0, 1, 9, 9 }, };

	public static final int FRONT = 0;
	public static final int BACK = 1;
	public static final int LEFT = 2;
	public static final int RIGHT = 3;

	/** [side][front][relative direction] to absolute direction **/
	public static final int[][][] dirMap = new int[6][6][4];
	
	/** [side][front][absolute direction] to relative direction **/
	public static final int[][][] invDirMap = new int[6][6][6];
	
	static {
		for (int side = 0; side < 6; side++) {
			for (int front = 0; front < 6; front++) {
				if ((front & 6) == (side & 6)) {
					continue;
				}

				dirMap[side][front][FRONT] = front;
				dirMap[side][front][BACK] = front ^ 1;
				dirMap[side][front][LEFT] = rotationMap[side][front];
				dirMap[side][front][RIGHT] = rotationMap[side][front ^ 1];

				for (int dir = 0; dir < 4; dir++) {
					invDirMap[side][front][dirMap[side][front][dir]] = dir;
				}
			}
		}
	}

	/**
	 * Returns true if a given edge of a block is an open space, so that wires
	 * placed on both sides of the edge will connect. If side and dir are
	 * swapped, the result is the same.
	 * 
	 * @param w
	 *            The world containing the edge.
	 * @param x
	 *            The X coordinate of the block containing the edge.
	 * @param y
	 *            The Y coordinate of the block containing the edge.
	 * @param z
	 *            The Z coordinate of the block containing the edge.
	 * @param side
	 *            One of the faces the edge is on.
	 * @param dir
	 *            The other face the edge is on.
	 * @return True if this edge is an open space.
	 */
	public static boolean canConnectThroughEdge(World w, int x, int y, int z, int side, int dir) {
		TileEntity te = w.getBlockTileEntity(x, y, z);
		if (te instanceof IMicroblockSupporterTile) {
			IMicroblockCoverSystem ci = ((IMicroblockSupporterTile) te).getCoverSystem();
			return ci.isEdgeOpen(side, dir);
		}
		return !w.isBlockSolidOnSide(x, y, z, ForgeDirection.VALID_DIRECTIONS[side]) && !w.isBlockSolidOnSide(x, y, z, ForgeDirection.VALID_DIRECTIONS[dir]);
	}

	private static Field wiresProvidePower = BlockRedstoneWire.class.getDeclaredFields()[0];
	static {
		try {
			wiresProvidePower.setAccessible(true);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

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
		if (b.hasTileEntity(meta)) {
			TileEntity te = w.getBlockTileEntity(x, y, z);
			if (te instanceof IRedstoneEmitter) {
				if (!countRedAlloyWire && te instanceof TilePlainRedAlloy)
					return 0;
				return ((IRedstoneEmitter) te).getEmittedSignalStrength(onSide, toDirection);
			}
		}

		// respond to weak power, or strong power, or strong power applied
		// through a block
		int pl = b.isProvidingStrongPower(w, x, y, z, toDirection ^ 1);
		if (pl > 0)
			return (short) (pl * 17);

		if (w.isBlockNormalCube(x, y, z)) {
			try {
				wiresProvidePower.set(Block.redstoneWire, false);
				pl = w.getBlockPowerInput(x, y, z);
				wiresProvidePower.set(Block.redstoneWire, true);
				if (pl > 0)
					return (short) (pl * 17);
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		} else {
			pl = b.isProvidingWeakPower(w, x, y, z, toDirection ^ 1);
			if (pl > 0)
				return (short) (pl * 17);
		}

		return 0;
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
