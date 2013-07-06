package mrtjp.projectred.multipart;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.MicroblockBreakingPacket;
import mrtjp.projectred.renderstuffs.RenderIDs;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.block.Block;
import net.minecraft.block.BlockContainer;
import net.minecraft.block.material.Material;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.Entity;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.EnumMovingObjectType;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.util.Vec3;
import net.minecraft.world.IBlockAccess;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

/*
 * How block breaking works, since this is slightly complicated:
 * 
 * When a client player starts breaking a part, or changes the part they are breaking, they send a packet to the server specifying which.
 * When a server player finishes breaking a part, this value is used.
 * 
 * When a player finishes breaking a part, removeBlockByPlayer is called which removes the part but saves it as the last-removed part.
 * If they were using the right tool, harvestBlock is called which drops and clears the last-removed part.
 * 
 * The available part numbers are split into tile-owned parts and system-owned parts.
 * System-owned parts have subhit = (-1 - partNumber) and are handled by the ICoverSystem if available.
 * Tile-owned parts have subhit = partNumber and are handled by the IMultipartTile. 
 * 
 * If all tile-owned parts are broken the tile should call ICoverSystem.convertToSystemBlock to convert
 * the block to a block that contains only the system-owned parts - for example, with MicroblockCoverSystem
 * the block becomes a microblock container block.
 * 
 * Note: A "subhit" refers to the subHit value obtained in a ray trace.
 */
public abstract class BlockMultipartBase extends BlockContainer {

	// Maps players to the part they are currently breaking.
	// If the player is not currently breaking a part, their value is undefined
	// (they may or may not have an entry in the map)
	private static SynchronizedWeakIdentityListMap<EntityPlayer, PartCoordinates> breaking_part = new SynchronizedWeakIdentityListMap<EntityPlayer, PartCoordinates>();

	@Override
	public boolean getEnableStats() {
		return false;
	}

	@Override
	public void addCollisionBoxesToList(World world, int x, int y, int z, AxisAlignedBB mask, List list, Entity entity) {
		try {
			IMultipartTile te = ((IMultipartTile) world.getBlockTileEntity(x, y, z));
			ICoverSystem ci = te.getCoverSystem();
			if (ci != null)
				ci.getCollidingBoundingBoxes(mask, list);
			te.getCollidingBoundingBoxes(mask, list);
		} catch (ClassCastException e) {
			world.setBlock(x, y, z, 0, 0, 2);
		}
	}

	public void harvestBlockMultipart(World world, EntityPlayer ply, int x, int y, int z, int blockMetadata) {
		super.harvestBlock(world, ply, x, y, z, blockMetadata);
	}

	@Override
	public final boolean canHarvestBlock(EntityPlayer ply, int meta) {
		return true;
	}

	public float getPartHardness(World w, int x, int y, int z, int part) {
		return super.getBlockHardness(w, x, y, z);
	}

	@Override
	public final float getBlockHardness(World w, int x, int y, int z) {
		return -1;
	}

	@Override
	public boolean removeBlockByPlayer(World w, EntityPlayer ply, int x, int y, int z) {
		return removeBlockByPlayerStatic(w, ply, x, y, z);
	}

	public static boolean removeBlockByPlayerStatic(World w, EntityPlayer ply, int x, int y, int z) {

		if (ply.worldObj.isRemote) {
			if (ply.capabilities.isCreativeMode) {
				updateBreakingPart(x, y, z);
			}
			breaking_part.remove(ply);
			// on the client, do nothing (let the server handle
			// multipart breaking)
			// TODO: client-side prediction of multipart
			// breaking
			return true;
		}

		// remove the part the player was breaking
		PartCoordinates coord = getBreakingPart(ply);
		breaking_part.remove(ply);

		if (coord == null || coord.x != x || coord.y != y || coord.z != z)
			return false;

		TileEntity te = w.getBlockTileEntity(x, y, z);

		if (!(te instanceof IMultipartTile)) {
			return false;
		}

		if (!coord.isCoverSystemPart) {
			lastDrop = ((IMultipartTile) te).removePartByPlayer(ply, coord.part);
		} else {
			lastDrop = ((IMultipartTile) te).getCoverSystem().removePartByPlayer(ply, coord.part);
		}

		return true;
	}

	static PartCoordinates getBreakingPart(EntityPlayer ply) {
		return breaking_part.get(ply);
	}

	@Override
	public void onBlockClicked(World w, int i, int j, int k, EntityPlayer ply) {
		onBlockClickedStatic(w, ply);
	}

	public static void onBlockClickedStatic(World w, EntityPlayer ply) {
		if (w.isRemote)
			// ensures a PacketMicroblockDigStart will be sent immediately
			breaking_part.remove(ply);
	}

	@SideOnly(Side.CLIENT)
	private static void sendDigStart() {
		PartCoordinates coord = getBreakingPart(Minecraft.getMinecraft().thePlayer);
		if (coord != null) {
			MicroblockBreakingPacket packet = PacketHandler.getPacket(MicroblockBreakingPacket.class);
			packet.setIsCSPart(coord.isCoverSystemPart);
			packet.part = coord.part;
			packet.posX = coord.x;
			packet.posY = coord.y;
			packet.posZ = coord.z;
			BasicUtils.sendPacketToServer(packet.getPacket());
		}
	}

	/**
	 * @param x
	 *            The X coordinate of the block the player should be breaking.
	 * @param y
	 *            The Y coordinate of the block the player should be breaking.
	 * @param z
	 *            The Z coordinate of the block the player should be breaking.
	 * @return True if the player is breaking a valid part of this block.
	 */
	@SideOnly(Side.CLIENT)
	private static boolean updateBreakingPart(int x, int y, int z) {

		// System.out.println("updateBreakingPart "+x+","+y+","+z);

		EntityPlayer player = Minecraft.getMinecraft().thePlayer;
		PartCoordinates old = getBreakingPart(player);

		MovingObjectPosition ray = player.rayTrace(BasicUtils.getPlayerReach(player), 0);
		PartCoordinates _new = null;
		if (ray == null || ray.typeOfHit != EnumMovingObjectType.TILE || ray.blockX != x || ray.blockY != y || ray.blockZ != z) {
			breaking_part.remove(player);

		} else {
			if (ray.subHit >= 0) {
				breaking_part.put(player, _new = new PartCoordinates(x, y, z, ray.subHit, false));
			} else {
				breaking_part.put(player, _new = new PartCoordinates(x, y, z, -1 - ray.subHit, true));
			}
		}

		boolean changed = (old == null && _new != null) || (old != null && !old.equals(_new));

		sendDigStart();
		if (changed) {
			resetBreakProgress(player);
		}

		return _new != null;
	}

	private static void resetBreakProgress(EntityPlayer ply) {
		// Need to reset the block damage, but that doesn't seem to be possible
		// Even RP2's covers don't do that
		// TODO: this was not edited since 1.2.5, is it possible now?
		/*
		 * PlayerController pc =
		 * ModLoader.getMinecraftInstance().playerController;
		 * pc.resetBlockRemoving(); pc.updateController();
		 */
	}

	@Override
	public final float getPlayerRelativeBlockHardness(EntityPlayer ply, World world, int x, int y, int z) {
		return getPlayerRelativeBlockHardnessStatic(ply, world, x, y, z);
	}

	public static float getPlayerRelativeBlockHardnessStatic(EntityPlayer ply, World world, int x, int y, int z) {

		if (world.isRemote)
			updateBreakingPart(x, y, z);

		PartCoordinates part = getBreakingPart(ply);
		if (part == null || part.x != x || part.y != y || part.z != z) {
			return 0;
		}

		TileEntity te = world.getBlockTileEntity(x, y, z);
		if (te == null || !(te instanceof IMultipartTile)) {
			return 0.01f;
		}

		else if (!part.isCoverSystemPart) {
			return ((IMultipartTile) te).getPlayerRelativePartHardness(ply, part.part);

		} else {
			ICoverSystem ci = ((IMultipartTile) te).getCoverSystem();
			return ci == null ? -1 : ci.getPlayerRelativePartHardness(ply, part.part);
		}
	}

	private static List<ItemStack> lastDrop = null;

	@Override
	public final ArrayList<ItemStack> getBlockDropped(World world, int x, int y, int z, int metadata, int fortune) {
		return getBlockDroppedStatic();
	}

	public static final ArrayList<ItemStack> getBlockDroppedStatic() {
		if (lastDrop == null)
			return new ArrayList<ItemStack>();

		ArrayList<ItemStack> rv = new ArrayList<ItemStack>(lastDrop);
		lastDrop = null;
		return rv;
	}

	void setAABB(AxisAlignedBB aabb) {
		minX = aabb.minX;
		minY = aabb.minY;
		minZ = aabb.minZ;
		maxX = aabb.maxX;
		maxY = aabb.maxY;
		maxZ = aabb.maxZ;
	}

	protected BlockMultipartBase(int id, Material mat) {
		super(id, mat);
	}

	/*
	 * @Override public final String getTextureFile() { return
	 * useWrappedRenderType ? wrappedGetTextureFile() : "/terrain.png"; }
	 * 
	 * private String wrappedTextureFile = "/terrain.png"; public String
	 * wrappedGetTextureFile() { return wrappedTextureFile; }
	 * 
	 * @Override public final Block setTextureFile(String s) {
	 * wrappedTextureFile = s; return this; }
	 */

	/**
	 * Override this for custom collision ray tracing. Return the ray trace
	 * result, or null if nothing intersects the ray. If the return value is
	 * non-null, its {@link MovingObjectPosition#subHit} field should be
	 * non-negative. If the return value is null, the tile entity's
	 * collisionRayTrace method is consulted instead.
	 */
	public MovingObjectPosition wrappedCollisionRayTrace(World world, int i, int j, int k, Vec3 vec3d, Vec3 vec3d1) {
		return null;
	}

	@Override
	public final MovingObjectPosition collisionRayTrace(World world, int x, int y, int z, Vec3 src, Vec3 dst) {
		try {
			IMultipartTile tile = (IMultipartTile) world.getBlockTileEntity(x, y, z);

			ICoverSystem ci = tile.getCoverSystem();
			MovingObjectPosition ciPos = ci == null ? null : ci.collisionRayTrace(src, dst);

			MovingObjectPosition tilePos = wrappedCollisionRayTrace(world, x, y, z, src, dst);

			if (tilePos != null && tilePos.subHit < 0)
				throw new AssertionError("wrappedCollisionRayTrace must return a non-negative subHit");

			if (tilePos == null)
				tilePos = tile.collisionRayTrace(src, dst);

			if (tilePos != null && tilePos.subHit < 0)
				throw new AssertionError("ICoverableTile.collisionRayTrace must return a non-negative subHit");

			if (tilePos == null)
				return ciPos;
			if (ciPos == null)
				return tilePos;

			double ciDist = ciPos.hitVec.squareDistanceTo(src);
			double tileDist = tilePos.hitVec.squareDistanceTo(src);

			return ciDist < tileDist ? ciPos : tilePos;

		} catch (ClassCastException e) {
			world.setBlock(x, y, z, 0, 0, 2);
			return super.collisionRayTrace(world, x, y, z, src, dst);
		}
	}

	@Override
	public final boolean isOpaqueCube() {
		return false;
	}

	@Override
	public final boolean renderAsNormalBlock() {
		return false;
	}

	static boolean useWrappedRenderType = false;

	/**
	 * Override this for custom rendering of the wrapped block.
	 */
	public int wrappedGetRenderType() {
		return 0;
	}

	@Override
	public final int getRenderType() {
		return useWrappedRenderType ? wrappedGetRenderType() : RenderIDs.renderIdMicroblock;
	}

	// for MicroblockSupporterTransformer
	public static int getRenderTypeStatic(int i) {
		return useWrappedRenderType ? i : RenderIDs.renderIdMicroblock;
	}

	public static String getTextureFileStatic(String s) {
		return useWrappedRenderType ? s : "/terrain.png";
	}

	public static void setBreakingPart(EntityPlayer source, PartCoordinates part) {
		if (part == null)
			breaking_part.remove(source);
		else
			breaking_part.put(source, part);

		if (source.capabilities.isCreativeMode) {
			removeBlockByPlayerStatic(source.worldObj, source, part.x, part.y, part.z);
		}
	}

	@Override
	public final int getDamageValue(World par1World, int par2, int par3, int par4) {
		// we don't know which part the player was looking at
		return 0;
	}

	@Override
	public final ItemStack getPickBlock(MovingObjectPosition target, World world, int x, int y, int z) {
		IMultipartTile te = (IMultipartTile) world.getBlockTileEntity(x, y, z);
		if (te == null)
			return null;

		if (target.subHit >= 0)
			return te.pickPart(target, target.subHit);
		ICoverSystem ci = te.getCoverSystem();
		return ci == null ? null : ci.pickPart(target, -1 - target.subHit);
	}

	@Override
	public boolean shouldSideBeRendered(IBlockAccess par1iBlockAccess, int par2, int par3, int par4, int par5) {
		return true;
	}

	@Override
	public boolean isBlockSolidOnSide(World world, int x, int y, int z, ForgeDirection side) {
		IMultipartTile imt = ((IMultipartTile) world.getBlockTileEntity(x, y, z));
		if (imt.isSolidOnSide(side))
			return true;
		ICoverSystem ci = imt.getCoverSystem();
		return ci != null && ci.isSolidOnSide(side);
	}

	@SideOnly(Side.CLIENT)
	public void renderInvBlock(RenderBlocks render, int meta) {
		renderInvBlockStatic(render, this, meta);
	}

	public static void renderInvBlockStatic(RenderBlocks render, Block block, int meta) {
		useWrappedRenderType = true;
		render.renderBlockAsItem(block, meta, 1);
		useWrappedRenderType = false;
	}

	public static void renderBlockStatic(RenderBlocks render, Block block, int x, int y, int z) {
		useWrappedRenderType = true;
		render.renderBlockByRenderType(block, x, y, z);
		useWrappedRenderType = false;
	}

	public static Iterable<Map.Entry<EntityPlayer, PartCoordinates>> getBreakingParts() {
		return breaking_part.entries();
	}
}
