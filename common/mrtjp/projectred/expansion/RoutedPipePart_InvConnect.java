package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.inventory.InventoryWrapper;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Icon;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;

public abstract class RoutedPipePart_InvConnect extends RoutedJunctionPipePart implements IInventoryProvider {

    public int inOutSide = 0;

    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("io", (byte) inOutSide);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        inOutSide = tag.getByte("io");
    }

    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(inOutSide);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        inOutSide = packet.readByte();
    }

    @Override
    public void read(MCDataInput packet, int switch_key) {
        if (switch_key == 15) {
            inOutSide = packet.readUByte();
            tile().markRender();
        }
        super.read(packet, switch_key);
    }
    
    @Override
    public void onNeighborChanged() {
        super.onNeighborChanged();
        shiftOrientation(false);
    }
    
    @Override
    public void onPartChanged(TMultiPart p) {
        super.onPartChanged(p);
        shiftOrientation(false);
    }
    
    @Override
    public void onAdded() {
        super.onAdded();
        shiftOrientation(false);
    }

    public void sendOrientUpdate() {
        tile().getWriteStream(this).writeByte(15).writeByte(inOutSide);
    }
    
    @Override
    public boolean connect(int absDir) {
        if (super.connect(absDir))
            return true;
        BlockCoord bc = new BlockCoord(tile()).offset(absDir);
        TileEntity t = BasicUtils.getTileEntity(world(), bc, TileEntity.class);

        return t instanceof IInventory;
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item) {
        if (super.activate(player, hit, item))
            return true;
        
        if (player.isSneaking()) {
            shiftOrientation(true);
            return true;
        }
        
        return false;
    }
    
    @Override
    public ForgeDirection getDirForIncomingItem(RoutedPayload r) {
        return ForgeDirection.getOrientation(inOutSide);
    }

    public void shiftOrientation(boolean force) {
        if (world().isRemote) return;
        boolean invalid = force  || !maskConnects(inOutSide) 
                || !(BasicUtils.getTileEntity(world(), 
                        new BlockCoord(tile()).offset(inOutSide), 
                        TileEntity.class) instanceof IInventory);
        if (!invalid) return;
        
        boolean found = false;
        int oldSide = inOutSide;
        
        for (int i = 0; i < 6; ++i) {
            inOutSide = (inOutSide + 1) % 6;

            if (!maskConnects(inOutSide))
                continue;

            BlockCoord bc = new BlockCoord(tile()).offset(inOutSide);
            TileEntity t = BasicUtils.getTileEntity(world(), bc, TileEntity.class);
            if (t instanceof IInventory) {
                found = true;
                break;
            }
        }
        
        if (!found) 
            inOutSide = -1;
        
        if (oldSide != inOutSide)
            sendOrientUpdate();
    }
    
    @Override
    public IInventory getInventory() {
        if (inOutSide < 0 || inOutSide > 5)
            return null;
        
        return InventoryWrapper.getInventory(world(), new BlockCoord(tile()).offset(inOutSide));
    }

    @Override
    public int getInterfacedSide() {
        return inOutSide < 0 || inOutSide > 5 ? -1 : inOutSide^1;
    }
    
    @Override
    public Icon getIcon(int side) {
        if (side == 6) 
            return super.getIcon(side);
        
        Icon[] array = side == inOutSide ? EnumPipe.ROUTEDINTERFACE.sprites : EnumPipe.ROUTEDJUNCTION.sprites;
        
        if ((linkMap&1<<side) != 0)
            return array[0];
        else
            return array[1];
    }

}
