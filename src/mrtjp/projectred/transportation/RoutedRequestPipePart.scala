package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.util.MovingObjectPosition
import mrtjp.projectred.core.libmc.ItemKey
import net.minecraftforge.common.util.ForgeDirection

class RoutedRequestPipePart extends BasicPipeAbstraction with TNetworkPipe
{
    override def centerReached(r:PipePayload)
    {
        if (!maskConnects(r.output.ordinal) && !world.isRemote) if (itemFlow.scheduleRemoval(r))
        {
            r.resetTrip
            r.moveProgress(0.375F)
            r.speed = 0.075F
            val ent = r.getEntityForDrop(x, y, z)
            ent.posY += 0.1F
            world.spawnEntityInWorld(ent)
        }
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true
        if (!player.isSneaking)
        {
            openGui(player)
            true
        }
        else false
    }

    private def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_open)
        packet.writeCoord(x, y, z).sendToPlayer(player)
    }

    override def getDirForIncomingItem(r:PipePayload):ForgeDirection =
    {
        val dir = ForgeDirection.getOrientation(inOutSide)
        if (dir == ForgeDirection.UNKNOWN)
        {
            val count = Integer.bitCount(connMap&0x3F)

            if (count <= 1) return r.input
            else if (count == 2)
            {
                for (i <- 0 until 6) if (i != r.input.getOpposite.ordinal)
                    if ((connMap&1<<i) != 0) return ForgeDirection.getOrientation(i)
            }
        }
        dir
    }

    override def getActiveFreeSpace(item:ItemKey) =
    {
        if (getInventory != null) super.getActiveFreeSpace(item)
        else Integer.MAX_VALUE
    }
}