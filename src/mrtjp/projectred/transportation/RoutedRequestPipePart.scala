package mrtjp.projectred.transportation

import codechicken.lib.packet.PacketCustom
import codechicken.lib.raytracer.CuboidRayTraceResult
import mrtjp.core.item.ItemKey
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.util.EnumHand

class RoutedRequestPipePart extends AbstractNetPipe with TNetworkPipe
{
    override def centerReached(r:NetworkPayload)
    {
        if (!maskConnects(r.output) && !world.isRemote) if (itemFlow.scheduleRemoval(r))
        {
            r.resetTrip()
            r.moveProgress(0.375F)
            r.speed = 0.075F
            val ent = r.getEntityForDrop(x, y, z)
            ent.posY += 0.1F
            world.spawnEntityInWorld(ent)
        }
    }


    override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true
        if (!player.isSneaking) {
            openGui(player)
            true
        }
        else false
    }

    private def openGui(player:EntityPlayer)
    {
        if (world.isRemote) return
        val packet = new PacketCustom(TransportationSPH.channel, TransportationSPH.gui_Request_open)
        packet.writePos(pos).sendToPlayer(player)
    }

    override def getDirForIncomingItem(r:NetworkPayload):Int =
    {
        val dir = inOutSide
        if (dir == 6)
        {
            val count = Integer.bitCount(connMap&0x3F)

            if (count <= 1) return r.input
            else if (count == 2)
            {
                for (i <- 0 until 6) if (i != (r.input^1))
                    if ((connMap&1<<i) != 0) return i
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