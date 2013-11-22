package mrtjp.projectred.expansion;

import mrtjp.projectred.core.inventory.InventoryWrapper;
import mrtjp.projectred.expansion.RoutedPayload.SendPriority;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.vec.BlockCoord;

public class RoutedRequesterPipePart extends RoutedPipePart_InvConnect {

    @Override
    public void centerReached(RoutedPayload r) {
        //super.centerReached(r);
    }
    
    @Override
    public void endReached(RoutedPayload r) {
        super.endReached(r);
    }
    
    @Override
    public void resolveDestination(RoutedPayload r) {
        super.resolveDestination(r);
    }
    
    @Override
    public void injectPayload(RoutedPayload r, ForgeDirection in) {
        super.injectPayload(r, in);
    }
    
    @Override
    public boolean passToNextPipe(RoutedPayload r) {
        int conns = 0;
        for (int i = 0; i < 6; i++)
            if ((connMap&1<<i) != 0)
                conns++;
        if (conns <= 1) {
            r.move(-1);
            r.isEntering = true;
            r.setSpeed(.1f);
            itemFlow.scheduleRemoval(r);
            if (!world().isRemote)
                world().spawnEntityInWorld(r.getEntityForDrop());
            r.payload.stackSize = 0;
            return true;
        } else 
            return super.passToNextPipe(r);   
    }
    
    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item) {
        if (super.activate(player, hit, item))
            return true;
        
        if (!world().isRemote)
            openGui(player);
        return true;
    }
    
    private void openGui(EntityPlayer player) {
        PacketCustom packet = new PacketCustom(ExpansionSPH.channel, NetConstants.gui_Request_open);
        packet.writeCoord(x(), y(), z());
        packet.sendToPlayer(player);
    }
    
    @Override
    public ForgeDirection getDirForIncomingItem(RoutedPayload r) {
        ForgeDirection dir = ForgeDirection.getOrientation(inOutSide);
        if (dir == ForgeDirection.UNKNOWN)
            return r.input.getOpposite();
        return dir;
    }

    @Override
    public String getType() {
        return "pr_rrequest";
    }
}
