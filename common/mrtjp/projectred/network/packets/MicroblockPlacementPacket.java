package mrtjp.projectred.network.packets;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import mrtjp.projectred.multipart.microblocks.EnumPosition;
import mrtjp.projectred.multipart.microblocks.ItemMicroblock;
import mrtjp.projectred.network.abstractpackets.CoordinatesPacket;
import mrtjp.projectred.network.abstractpackets.ModernPacket;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public class MicroblockPlacementPacket extends CoordinatesPacket {

	public int position;
	public int side;
		
	public MicroblockPlacementPacket(int id) {
		super(id);
	}

	@Override
	public ModernPacket template() {
		return new MicroblockPlacementPacket(getID());
	}

	@Override
	public void processPacket(EntityPlayer player) {
		if(player != null) {
			if(position < 0 || position >= EnumPosition.values().length) {
				return;
			}
			EnumPosition pos = EnumPosition.values()[position];
			ItemStack h = player.getCurrentEquippedItem();
			if(h == null || !(Item.itemsList[h.itemID] instanceof ItemMicroblock)) {
				return;
			}
			ItemMicroblock i = (ItemMicroblock)Item.itemsList[h.itemID];

			if(i.placeInBlock(player.worldObj, getPosX(), getPosY(), getPosZ(), pos, h, true, side) && !player.capabilities.isCreativeMode) {
				h.stackSize--;
				if(h.stackSize == 0) {
					player.destroyCurrentEquippedItem();
				}
			}
		}
	}

	@Override
	public void writeData(DataOutputStream data) throws IOException {
		super.writeData(data);
		data.writeInt(position);
		data.writeInt(side);
	}

	@Override
	public void readData(DataInputStream data) throws IOException {
		super.readData(data);
		position = data.readInt();
		side = data.readInt();
	}
}

