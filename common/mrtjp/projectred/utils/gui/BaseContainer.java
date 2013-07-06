package mrtjp.projectred.utils.gui;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import mrtjp.projectred.interfaces.ISyncedContainer;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.network.packets.ContainersButtonPressedPacket;
import mrtjp.projectred.utils.BasicUtils;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.ICrafting;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;
import cpw.mods.fml.common.network.Player;

public class BaseContainer extends Container implements ISyncedContainer {

	protected EntityPlayer player;
	protected TileEntity inv;

	public BaseContainer(EntityPlayer player, TileEntity inv) {
		this.player = player;
		this.inv = inv;
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

	/** Sends a button-press packet. */
	public void sendButtonPressed(int id) {
		ContainersButtonPressedPacket packet = (PacketHandler.getPacket(ContainersButtonPressedPacket.class));
		packet.setButtonID(id);
		BasicUtils.sendPacketToServer(packet.getPacket());
	}
	
	/**
	 * Called when a button-press packet is received. It's like an action packet
	 * that carries a single int, for convenience (you don't have to make a
	 * packet class to wrap just one int)
	 */
	public void onButtonPressed(int id) {
		
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
