package mrtjp.projectred.multipart.wiring;

import net.minecraft.tileentity.TileEntity;

public class InvalidTile extends TileEntity {
	@Override
	public void updateEntity() {
		System.err.println("[RedLogic DEBUG] Invalid tile entity at "+xCoord+","+yCoord+","+zCoord+". Metadata value "+getBlockMetadata());
		worldObj.setBlockToAir(xCoord, yCoord, zCoord);
	}
}
