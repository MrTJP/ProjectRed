package mrtjp.projectred.multipart;

import java.util.List;

import mrtjp.projectred.multipart.microblocks.IMicroblockCoverSystem;
import mrtjp.projectred.multipart.microblocks.IMicroblockSupporterTile2;
import mrtjp.projectred.multipart.microblocks.IMicroblockLibrary;
import mrtjp.projectred.multipart.microblocks.MicroblockLibrary;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.INetworkManager;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.AxisAlignedBB;
import net.minecraft.util.MovingObjectPosition;
import net.minecraft.util.Vec3;

/**
 * Convenience class for microblock supporting tiles. Handles creating an
 * ICoverSystem, saving and loading it, and implements getCollidingBoundingBoxes
 * and collisionRayTrace.
 */
public abstract class TileCoverableBase extends TileEntity implements IMicroblockSupporterTile2 {

	protected IMicroblockCoverSystem cover;

	public TileCoverableBase() {
		IMicroblockLibrary ims = MicroblockLibrary.instance.getMicroblockSystem();
		if (ims != null)
			cover = ims.createMicroblockCoverSystem(this);
	}

	@Override
	public void writeToNBT(NBTTagCompound tag) {
		super.writeToNBT(tag);
		if (cover != null)
			cover.writeToNBT(tag);
	}

	@Override
	public Packet getDescriptionPacket() {
		if (cover == null)
			return null;

		Packet132TileEntityData p = new Packet132TileEntityData(xCoord, yCoord, zCoord, 0, new NBTTagCompound());
		p.customParam1.setByteArray("C", cover.writeDescriptionBytes());
		return p;
	}

	@Override
	public void onDataPacket(INetworkManager net, Packet132TileEntityData pkt) {
		if (cover != null)
			cover.readDescriptionBytes(pkt.customParam1.getByteArray("C"), 0);
	}

	@Override
	public void readFromNBT(NBTTagCompound tag) {
		super.readFromNBT(tag);
		if (cover != null)
			cover.readFromNBT(tag);
	}

	@Override
	public IMicroblockCoverSystem getCoverSystem() {
		return cover;
	}

	protected abstract int getNumTileOwnedParts();

	@Override
	public MovingObjectPosition collisionRayTrace(Vec3 src, Vec3 dst) {
		src = src.addVector(-xCoord, -yCoord, -zCoord);
		dst = dst.addVector(-xCoord, -yCoord, -zCoord);

		int numTOP = getNumTileOwnedParts();

		MovingObjectPosition best = null;
		double bestDist = 0;
		for (int k = 0; k < numTOP; k++) {
			AxisAlignedBB partBB = getPartAABBFromPool(k);
			if (partBB == null)
				continue;

			MovingObjectPosition _this = partBB.calculateIntercept(src, dst);
			if (_this != null) {
				double dist = _this.hitVec.squareDistanceTo(src);
				if (best == null || dist < bestDist) {
					bestDist = dist;
					best = _this;
					best.subHit = k;
				}
			}
		}

		if (best == null)
			return null;
		MovingObjectPosition result = new MovingObjectPosition(xCoord, yCoord, zCoord, best.sideHit, best.hitVec.addVector(xCoord, yCoord, zCoord));
		result.subHit = best.subHit;
		return result;
	}

	@Override
	public void getCollidingBoundingBoxes(AxisAlignedBB mask, List<AxisAlignedBB> list) {
		for (int k = 0; k < getNumTileOwnedParts(); k++) {
			AxisAlignedBB partBB = getPartAABBFromPool(k);
			if (partBB == null)
				continue;
			partBB = partBB.offset(xCoord, yCoord, zCoord);
			if (partBB.intersectsWith(mask))
				list.add(partBB);
		}
	}

	@Override
	public void onMicroblocksChanged() {
		worldObj.notifyBlocksOfNeighborChange(xCoord, yCoord, zCoord, getBlockType().blockID);
		worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
	}

}
