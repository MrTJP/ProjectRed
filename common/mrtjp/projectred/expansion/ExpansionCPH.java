package mrtjp.projectred.expansion;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;

public class ExpansionCPH implements IClientPacketHandler {

    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
        EntityPlayer player = mc.thePlayer;
        World world = mc.theWorld;

        switch (packet.getType()) {
        case ExpansionNetworkConstants.alloySmelterWatcherUpdate:
            TileAlloySmelter t = BasicUtils.getTileEntity(world, packet.readCoord(), TileAlloySmelter.class);
            if (t != null) {
                t.heat = packet.readShort();
                t.progress = packet.readShort();
            }
            return;
            
        default: break;
        }

    }

}
