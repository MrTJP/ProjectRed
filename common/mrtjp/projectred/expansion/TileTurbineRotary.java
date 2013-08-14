package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.GhostContainer;
import mrtjp.projectred.core.GuiRestrictedSlot.ISlotCheck;
import mrtjp.projectred.core.SimpleInventory;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.network.packet.Packet;
import net.minecraft.network.packet.Packet132TileEntityData;
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
                    return stack != null && stack.getItem().itemID == ProjectRedCore.itemVAWT.itemID;
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
        BasicUtils.dropItem(worldObj, xCoord, yCoord, zCoord, new ItemStack(ProjectRedCore.blockMachines.blockID, 1, EnumMachine.TURBINEROTARY.meta));
        _inv.dropContents(worldObj, xCoord, yCoord, zCoord);
    }

    @Override
    public void onBlockClicked(EntityPlayer player) {
    }

    @Override
    public boolean onBlockActivated(EntityPlayer player) {
        if (!player.isSneaking()) {
            player.openGui(ProjectRedCore.instance, ExpansionGuiHandler.rotaryID, player.worldObj, xCoord, yCoord, zCoord);
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
        NBTTagCompound nbt = new NBTTagCompound();
        writeToNBT(nbt);
        return new Packet132TileEntityData(this.xCoord, this.yCoord, this.zCoord, 3, nbt);
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
