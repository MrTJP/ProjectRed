package mrtjp.projectred.integration;

import java.util.HashMap;
import java.util.Map;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.inventory.ICrafting;
import codechicken.multipart.TMultiPart;

public abstract class ContainerMultipart extends Container {
	public TMultiPart part;
	protected EntityPlayer player;

	public ContainerMultipart(EntityPlayer player, TMultiPart part) {
		this.part = part;
		this.player = player;
	}

	/**
	 * When ever a watcher changes something, these 2 ints are passed to
	 * everyone. They can be highjacked and used to update graphical elements on
	 * the clients.
	 */
	public abstract void interceptMessage(int ID, int message);
	
	/**
	 * Passses received message to interceptMessage.
	 */
	@Override
	public void updateProgressBar(int par1, int par2) {
		super.updateProgressBar(par1, par2);
		interceptMessage(par1, par2);
	}

	/**
	 * Uses vanilla updating method to give all players watching 2 int values.
	 * The first one is the ID, and the second is a value. These values are
	 * passed to updateProgressBar, which is where these values should be
	 * intercepted on the receiving end.This should be called from inside
	 * setNewMessages.
	 * 
	 * @param ID
	 * @param message
	 */
	protected void setGraphicalUpdate(int ID, int message) {
		short index = (short) ID, value = (short) message;
		Short prev = currentValues.get(index);
		if (prev == null || prev != value) {
			currentValues.put(index, value);
			updateAllWatchers(index, value);
		}
	}

	private Map<Short, Short> currentValues = new HashMap<Short, Short>();

	/**
	 * Should set all messages here. This should call setGraphicalUpdate;
	 */
	public abstract void setNewMessages();

	/**
	 * Pushes the message to all watchers, after deciding if its necessary.
	 * 
	 * @param ID
	 * @param message
	 */
	public void updateAllWatchers(int ID, int message) {
		for (Object o : crafters) {
			((ICrafting) o).sendProgressBarUpdate(this, ID, message);
		}
	}

	/**
	 * Sends updates, then updates client for next send.
	 */
	@Override
	public void detectAndSendChanges() {
		super.detectAndSendChanges();
		setNewMessages();
	}

	/**
	 * This is sent from the client whenever they push a button.
	 * 
	 * @param buttonID
	 */
	public abstract void handleActionFromClient(int buttonID);
}
