package mrtjp.projectred.multipart.microblocks;

import java.util.List;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.MicroblockPlacementPacket;
import mrtjp.projectred.utils.BasicUtils;
import mrtjp.projectred.utils.Dir;
import net.minecraft.block.Block;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemBlock;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.world.World;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class ItemMicroblock extends ItemBlock {

	private BlockMultipartBase block;
	private static final boolean DEBUG = Configurator.debugMode.getBoolean(false);


	public ItemMicroblock(int id) {
		super(id);
		block = (BlockMultipartBase) Block.blocksList[itemID];
	}

	public static class Placement {
		public final int x, y, z;
		public final EnumPosition pos;

		public Placement(int x, int y, int z, EnumPosition pos) {
			this.x = x;
			this.y = y;
			this.z = z;
			this.pos = pos;
		}
	}

	@SideOnly(Side.CLIENT)
	public static Placement getPlacement(ItemStack itemstack, EntityPlayer player, World world, int x, int y, int z, int dir) {
		MovingObjectPosition ray = player.rayTrace(BasicUtils.getPlayerReach(player), 0);
		if (ray == null) {
			if (DEBUG)
				System.out.println("null raytrace");
			return null;
		}

		x = ray.blockX;
		y = ray.blockY;
		z = ray.blockZ;
		dir = ray.sideHit;
		TileEntity rayTE = (world.getBlockTileEntity(ray.blockX, ray.blockY, ray.blockZ));
		EnumPosition rayPos = null;
		if (rayTE instanceof IMicroblockSupporterTile) {
			if (ray.subHit < 0)
				rayPos = ((IMicroblockSupporterTile) rayTE).getCoverSystem().getPartPosition(-1 - ray.subHit);
			else
				rayPos = ((IMicroblockSupporterTile) rayTE).getPartPosition(ray.subHit);
		}

		int oldblock = world.getBlockId(x, y, z);
		if (oldblock == Block.snow.blockID)
			dir = 0;
		else if (oldblock != Block.vine.blockID) {
			int dx = 0, dy = 0, dz = 0;
			switch (dir) {
			case Dir.NX:
				if (rayPos == null || rayPos.x.touchesNegative())
					dx = -1;
				break;
			case Dir.PX:
				if (rayPos == null || rayPos.x.touchesPositive())
					dx = 1;
				break;
			case Dir.NY:
				if (rayPos == null || rayPos.y.touchesNegative())
					dy = -1;
				break;
			case Dir.PY:
				if (rayPos == null || rayPos.y.touchesPositive())
					dy = 1;
				break;
			case Dir.NZ:
				if (rayPos == null || rayPos.z.touchesNegative())
					dz = -1;
				break;
			case Dir.PZ:
				if (rayPos == null || rayPos.z.touchesPositive())
					dz = 1;
				break;
			}
			if (dx != 0 || dy != 0 || dz != 0) {
				x += dx;
				y += dy;
				z += dz;
				rayPos = null;
			}
		}
		if (itemstack.stackSize == 0) {
			if (DEBUG)
				System.out.println("empty stack");
			return null;
		}

		PartType<?> type = MicroblockSystem.parts.get(getPartTypeID(itemstack));
		if (type == null) {
			// invalid item
			itemstack.stackSize = 0;
			if (DEBUG)
				System.out.println("invalid type");
			return null;
		}
		EnumPosition pos;
		EnumPartClass clazz = type.getPartClass();
		if (clazz == EnumPartClass.Panel || clazz == EnumPartClass.HollowPanel) {
			pos = MicroblockPlacementHighlightHandler.getPanelPlacement(player, ray, rayPos);
		} else if (clazz == EnumPartClass.Corner) {
			pos = MicroblockPlacementHighlightHandler.getCornerPlacement(player, ray, rayPos);
		} else if (clazz == EnumPartClass.Strip) {
			pos = MicroblockPlacementHighlightHandler.getStripPlacement(player, ray, rayPos);
		} else {
			if (DEBUG)
				System.out.println("invalid class");
			return null;
		}

		return new Placement(x, y, z, pos);
	}

	@Override
	public boolean onItemUse(ItemStack itemstack, EntityPlayer entityplayer, World world, int x, int y, int z, int dir, float x2, float y2, float z2) {
		if (world.isRemote) {

			Placement pl = getPlacement(itemstack, entityplayer, world, x, y, z, dir);

			if (pl == null)
				return false;

			if (!placeInBlock(world, pl.x, pl.y, pl.z, pl.pos, itemstack, true, dir)) {
				if (DEBUG)
					System.out.println("placeInBlock failed");
				return false;
			}

			return true;
		} else {
			// server
			return false;
		}
	}

	public boolean placeInBlock(World world, int x, int y, int z, EnumPosition pos, ItemStack itemstack, boolean doBlockUpdate, int sideClicked) {
		if (DEBUG) {
			System.out.println((world.isRemote ? "client" : "server") + " placeInBlock " + x + "," + y + "," + z + " " + pos);
			System.out.println("ID of microblock is " + getPartTypeID(itemstack));
		}
		if (world.isRemote) {
			MicroblockPlacementPacket packet = PacketHandler.getPacket(MicroblockPlacementPacket.class);
			packet.position = pos.ordinal();
			packet.side = sideClicked;
			packet.posX = x;
			packet.posY = y;
			packet.posZ = z;
			BasicUtils.sendPacketToServer(packet.getPacket());
			return true;
		}

		int d = getPartTypeID(itemstack);
		if (!MicroblockSystem.parts.containsKey(d)) {
			if (DEBUG)
				System.out.println("wrong part ID, got " + d);
			return false;
		}

		TileEntity newTE = world.getBlockTileEntity(x, y, z);
		boolean addedTE = false;
		if (newTE == null || !(newTE instanceof IMicroblockSupporterTile)) {
			Block replacing = Block.blocksList[world.getBlockId(x, y, z)];
			if (replacing != null && !replacing.isBlockReplaceable(world, x, y, z)) {
				if (DEBUG)
					System.out.println("not replaceable");
				return false;
			}
			if (!block.canPlaceBlockOnSide(world, x, y, z, 0)) {
				if (DEBUG)
					System.out.println("can't place on side");
				return false;
			}

			world.setBlock(x, y, z, block.blockID, 0, 0);
			newTE = new TileMicroblockContainer();
			world.setBlockTileEntity(x, y, z, newTE);
			addedTE = true;
		}

		PartType<?> type = MicroblockSystem.parts.get(d);
		assert type != null : "No part type with ID " + d;

		IMicroblockCoverSystem cover = ((IMicroblockSupporterTile) newTE).getCoverSystem();
		assert cover != null : "New tile entity has no cover system";

		if (!cover.addPart(type.createPart(pos))) {
			if (addedTE)
				world.setBlock(x, y, z, 0, 0, 0);

			if (DEBUG)
				System.out.println("addPart failed");
			return false;
		}
		if (DEBUG)
			System.out.println("addPart ok");

		if (doBlockUpdate) {
			if (newTE instanceof IMicroblockSupporterTile2)
				((IMicroblockSupporterTile2) newTE).onMicroblocksChanged();
			else {
				world.notifyBlocksOfNeighborChange(x, y, z, block.blockID);
				world.markBlockForUpdate(x, y, z);
			}
		}
		return true;
	}

	@Override
	public String getUnlocalizedName(ItemStack is) {
		PartType<?> pt = MicroblockSystem.instance.getPartTypeByID(getPartTypeID(is));
		return pt == null ? null : pt.getUnlocalizedName(is);
	}

	@Override
	public boolean getHasSubtypes() {
		return true;
	}

	@SideOnly(Side.CLIENT)
	@Override
	public boolean canPlaceItemBlockOnSide(World w, int x, int y, int z, int side, EntityPlayer ply, ItemStack stack) {
		return true;
	}

	@Override
	public boolean getShareTag() {
		return true;
	}

	public static int getPartTypeID(ItemStack stack) {
		NBTTagCompound tag = stack.stackTagCompound;
		if (tag != null && tag.hasKey("MicroType")) {
			return tag.getInteger("MicroType");
		}
		return stack.getItemDamage();
	}

	public static ItemStack getStackWithPartID(int id) {
		ItemStack rv = new ItemStack(ProjectRed.blockMicrocontainer, 1, 0);
		rv.stackTagCompound = new NBTTagCompound("tag");
		rv.stackTagCompound.setInteger("MicroType", id);
		return rv;
	}

	public static ItemStack getStackWithPartID(int partID, int stackSize) {
		ItemStack rv = getStackWithPartID(partID);
		rv.stackSize = stackSize;
		return rv;
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void addInformation(ItemStack par1ItemStack, EntityPlayer par2EntityPlayer, List par3List, boolean par4) {
		super.addInformation(par1ItemStack, par2EntityPlayer, par3List, par4);
		if (par4) {
			par3List.add("Part ID: " + getPartTypeID(par1ItemStack));
			par3List.add("Hex: " + Integer.toHexString(getPartTypeID(par1ItemStack)));
		}
	}
}
