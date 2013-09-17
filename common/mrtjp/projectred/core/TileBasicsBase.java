package mrtjp.projectred.core;

import mrtjp.projectred.core.BlockBasics.EnumBasics;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.network.packet.Packet;
import net.minecraft.tileentity.TileEntity;

public abstract class TileBasicsBase extends TileEntity {
    
    public TileBasicsBase() {}

    public abstract void onBlockBreak();

    public abstract void onBlockClicked(EntityPlayer player);
    
    public abstract boolean onBlockActivated(EntityPlayer player);

    public abstract EnumBasics getType();
    
    public abstract void onBlockPlacedBy(EntityLivingBase player, ItemStack item);

    @Override
    public abstract Packet getDescriptionPacket();
    
    @Override
    public abstract void updateEntity();
        
    public abstract int getLightLevel();
}
