package mrtjp.projectred.multipart.microblocks;




import java.util.List;

import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.util.Vec3;
import net.minecraftforge.common.ForgeDirection;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

/**
 * A tile that contains only microblocks.
 */
public class TileMicroblockContainer extends TileEntity implements IMicroblockSupporterTile {
	public MicroblockCoverSystem cover;
	
	public TileMicroblockContainer() {
		cover = new MicroblockCoverSystem(this);
	}
	
	@Override
	public final Packet getDescriptionPacket() {
		NBTTagCompound tag = new NBTTagCompound();
		tag.setByteArray("C", cover.writeDescriptionBytes());
		return new Packet132TileEntityData(xCoord, yCoord, zCoord, 0, tag);
	}
	
	@Override
	public void onDataPacket(INetworkManager net, Packet132TileEntityData pkt) {
		cover.readDescriptionBytes(pkt.customParam1.getByteArray("C"), 0);
	}

	@Override
	public IMicroblockCoverSystem getCoverSystem() {
		return cover;
	}
	
	@Override
	public void writeToNBT(NBTTagCompound tag) {
		super.writeToNBT(tag);
		cover.writeToNBT(tag);
	}
	
	@Override
	public void readFromNBT(NBTTagCompound tag) {
		super.readFromNBT(tag);
		cover.readFromNBT(tag);
	}

	@Override
	public boolean isPlacementBlockedByTile(PartType<?> type, EnumPosition pos) {
		return false;
	}
	
	@Override
	public boolean isPositionOccupiedByTile(EnumPosition pos) {
		return false;
	}
	
	@Override
	public void updateEntity() {
		if(!worldObj.isRemote && getClass() == TileMicroblockContainer.class && cover.parts.size() == 0) {
			worldObj.setBlock(xCoord, yCoord, zCoord, 0, 0, 2);
			return;
		}
	}
	
	@Override
	public boolean canUpdate() {
		return true;
	}

	@Override
	public MovingObjectPosition collisionRayTrace(Vec3 src, Vec3 dst) {
		return null;
	}

	@Override
	public EnumPosition getPartPosition(int subHit) {
		return null;
	}

	@Override
	public AxisAlignedBB getPartAABBFromPool(int subHit) {
		return null;
	}

	@Override
	public void getCollidingBoundingBoxes(AxisAlignedBB mask, List<AxisAlignedBB> list) {
	}

	@Override
	public ItemStack pickPart(MovingObjectPosition rayTrace, int part) {
		return null;
	}

	@Override
	public boolean isSolidOnSide(ForgeDirection side) {
		return false;
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void render(RenderBlocks render) {
		
	}

	@Override
	@SideOnly(Side.CLIENT)
	public void renderPart(RenderBlocks render, int part) {
		
	}

	@Override
	public List<ItemStack> removePartByPlayer(EntityPlayer ply, int part) {
		return null;
	}

	@Override
	public float getPlayerRelativePartHardness(EntityPlayer ply, int part) {
		return -1;
	}
}
