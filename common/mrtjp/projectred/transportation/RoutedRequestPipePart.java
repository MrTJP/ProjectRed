package mrtjp.projectred.transportation;

import mrtjp.projectred.core.utils.ItemKey;
import net.minecraft.entity.item.EntityItem;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.item.ItemStack;
import net.minecraft.util.MovingObjectPosition;
import net.minecraftforge.common.ForgeDirection;
import codechicken.lib.packet.PacketCustom;

public class RoutedRequestPipePart extends RoutedPipePart_Inv
{
    @Override
    public void centerReached(RoutedPayload r)
    {
        if (getLogic().centerReached(r))
            return;

        if (!maskConnects(r.output.ordinal()) && !world().isRemote)
            if (itemFlow.scheduleRemoval(r))
            {
                r.resetTrip();
                r.moveProgress(0.375F);
                r.speed = 0.075F;
                EntityItem ent = r.getEntityForDrop(x(), y(), z());
                ent.posY += 0.1F;
                world().spawnEntityInWorld(ent);
            }
    }

    @Override
    public void endReached(RoutedPayload r)
    {
        super.endReached(r);
    }

    @Override
    public void resolveDestination(RoutedPayload r)
    {
        super.resolveDestination(r);
    }

    @Override
    public void injectPayload(RoutedPayload r, ForgeDirection in)
    {
        super.injectPayload(r, in);
    }

    @Override
    public boolean activate(EntityPlayer player, MovingObjectPosition hit, ItemStack item)
    {
        if (super.activate(player, hit, item))
            return true;

        if (!world().isRemote)
            openGui(player);
        return true;
    }

    private void openGui(EntityPlayer player)
    {
        PacketCustom packet = new PacketCustom(TransportationSPH.channel, NetConstants.gui_Request_open);
        packet.writeCoord(x(), y(), z());
        packet.sendToPlayer(player);
    }

    @Override
    public ForgeDirection getDirForIncomingItem(RoutedPayload r)
    {
        ForgeDirection dir = ForgeDirection.getOrientation(inOutSide);
        if (dir == ForgeDirection.UNKNOWN)
        {
            int count = 0;
            for (int i = 0; i < 6; i++)
                if ((connMap & 1 << i) != 0)
                    count++;

            if (count <= 1)
                return r.input;
            else if (count == 2)
            {
                for (int i = 0; i < 6; i++)
                {
                    if (i == r.input.getOpposite().ordinal())
                        continue;
                    if ((connMap & 1 << i) != 0)
                        return ForgeDirection.getOrientation(i);
                }
            }
        }
        return dir;
    }

    @Override
    public int getActiveFreeSpace(ItemKey item)
    {
        if (getInventory() != null)
            return super.getActiveFreeSpace(item);

        return Integer.MAX_VALUE;
    }

    @Override
    public String getType()
    {
        return "pr_rrequest";
    }
}
