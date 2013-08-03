package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.GhostContainer;
import mrtjp.projectred.core.GuiIDs;
import mrtjp.projectred.core.PacketHandler;
import mrtjp.projectred.core.SimpleInventory;
import mrtjp.projectred.core.RestrictedSlot.ISlotCheck;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.expansion.packets.RotaryNBTPacket;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraftforge.common.ForgeDirection;

public class TileTurbineRotary extends TileMachineBase {


	float bladeRotation;
	float bladeSpeed;

	private SimpleInventory _inv = new SimpleInventory(1, "turbine", 64);

	public TileTurbineRotary() {
	}

	public Container getContainer(EntityPlayer player) {
		GhostContainer ghost = new GhostContainer(player.inventory, _inv);
		// Turbine slot
		ghost.addRestrictedSlot(0, _inv, 141, 47, new ISlotCheck() {
			@Override
			public boolean isSlotAllowed(ItemStack stack) {
				if (rotation == 0) {
					return stack != null && stack.getItem().itemID == ProjectRed.itemVAWT.itemID;
				} else {
					return false;
				}
			}
		});
		ghost.addNormalSlotsForPlayerInventory(8, 84);
		return ghost;
	}

	public boolean hasSail() {
		if (_inv.getStackInSlot(0) != null) {
			return true;
		}
		return false;
	}

	@Override
	public void writeToNBT(NBTTagCompound nbt) {
		super.writeToNBT(nbt);
		_inv.writeToNBT(nbt);
		nbt.setFloat("rot", bladeRotation);
		nbt.setFloat("speed", bladeSpeed);
	}

	@Override
	public void readFromNBT(NBTTagCompound nbt) {
		super.readFromNBT(nbt);
		_inv.readFromNBT(nbt);
		bladeRotation = nbt.getFloat("rot");
		bladeSpeed = nbt.getFloat("speed");
		updateNextTick = true;
	}

	@Override
	public void onBlockBreak() {
		BasicUtils.dropItem(worldObj, xCoord, yCoord, zCoord, new ItemStack(ProjectRed.blockMachines.blockID, 1, EnumMachine.TURBINEROTARY.meta));
		_inv.dropContents(worldObj, xCoord, yCoord, zCoord);
	}

	@Override
	public void onBlockClicked(EntityPlayer player) {
	}

	@Override
	public boolean onBlockActivated(EntityPlayer player) {
		if (!player.isSneaking()) {
			player.openGui(ProjectRed.instance, GuiIDs.ID_TurbineRotary, player.worldObj, xCoord, yCoord, zCoord);
			return true;
		}
		return false;
	}

	@Override
	public EnumMachine getType() {
		return EnumMachine.TURBINEROTARY;
	}

	@Override
	public Packet getDescriptionPacket() {
		RotaryNBTPacket packet = PacketHandler.getPacket(RotaryNBTPacket.class);
		packet.posX = xCoord;
		packet.posY = yCoord;
		packet.posZ = zCoord;
		NBTTagCompound nbt = new NBTTagCompound();
		writeToNBT(nbt);
		packet.tiledata = nbt;
		return packet.getPacket();
	}

	@Override
	public void updateEntity() {
		if (updateNextTick) {
			updateNextTick = false;
			worldObj.markBlockForUpdate(xCoord, yCoord, zCoord);
		}
	}

	@Override
	public int getIconForSide(int side) {
		if (ForgeDirection.OPPOSITES[rotation] == side) {
			return 0;
		}
		if (side == rotation) {
			return 2;
		}
		return 1;
	}

	@Override
	public void onBlockPlacedBy(EntityLivingBase player, ItemStack item) {
		if (player.rotationPitch > 75) {
			rotation = 0;
			return;
		} else {
			super.onBlockPlacedBy(player, item);
		}
	}
}
