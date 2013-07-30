package mrtjp.projectred.utils.gui;

import java.util.HashMap;
import java.util.Map;

import mrtjp.projectred.interfaces.ISyncedContainer;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.ContainersButtonPressedPacket;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.ICrafting;
import net.minecraft.item.ItemStack;
import net.minecraft.tileentity.TileEntity;

public class BaseContainer extends Container {

	protected EntityPlayer player;

	public BaseContainer(EntityPlayer player) {
		this.player = player;
	}

	@Override
	public boolean canInteractWith(EntityPlayer var1) {
		return true;
	}

	@Override
	public ItemStack transferStackInSlot(EntityPlayer pl, int slot) {
		return transferStackInSlot(slot);
	}

	public ItemStack transferStackInSlot(int slot) {
		return null;
	}

	@Override
	public ItemStack slotClick(int slot, int button, int shift, EntityPlayer player) {
		return null;
	}

	public void sendProgressBarUpdate(int index, int value) {
		for (Object o : crafters) {
			((ICrafting) o).sendProgressBarUpdate(this, index, value);
		}
	}

	private Map<Short, Short> prevBarValues = new HashMap<Short, Short>();

	protected void setProgressBar(int _index, int _value) {
		short index = (short) _index, value = (short) _value;
		Short prev = prevBarValues.get(index);
		if (prev == null || prev != value) {
			prevBarValues.put(index, value);
			sendProgressBarUpdate(index, value);
		}
	}
}
