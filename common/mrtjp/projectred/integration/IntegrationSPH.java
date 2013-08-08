package mrtjp.projectred.integration;

import mrtjp.projectred.core.BasicUtils;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.NetServerHandler;
import net.minecraft.world.WorldServer;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IServerPacketHandler;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TileMultipart;

public class IntegrationSPH implements IServerPacketHandler {

    @Override
    public void handlePacket(PacketCustom packet, NetServerHandler nethandler, EntityPlayerMP player) {
        WorldServer world = (WorldServer) player.worldObj;

        switch (packet.getType()) {
        case IntegrationNetworkConstants.guiGateButtonPressed:
            BlockCoord b = packet.readCoord();
            int face = packet.readByte();
            String action = packet.readString();
            TileMultipart t = (TileMultipart) BasicUtils.getTileEntity(world, b, TileMultipart.class);
            GatePart g = (GatePart) t.partMap(face);
            ((GateLogic.WithGui) g.getLogic()).handleButtonPressed(action, g);
        }
    }
}
