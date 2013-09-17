package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import codechicken.lib.packet.ICustomPacketTile;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.lib.vec.BlockCoord;

public class CoreCPH implements IClientPacketHandler
{
    public static Object channel = ProjectRedCore.instance;

    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {
        World world = mc.theWorld;

        switch(packet.getType()) {
            case 1:
                handleTilePacket(world, packet, packet.readCoord());
                return;
            case 2:
                Messenger.addMessage(packet.readDouble(), packet.readDouble(), packet.readDouble(), packet.readString());
                return;
            case 3:
                TileAlloySmelter t = BasicUtils.getTileEntity(world, packet.readCoord(), TileAlloySmelter.class);
                if(t != null) {
                    t.heat = packet.readShort();
                    t.progress = packet.readShort();
                }
                return;
        }
    }

    private void handleTilePacket(World world, PacketCustom packet, BlockCoord pos) {
        TileEntity tile =  world.getBlockTileEntity(pos.x, pos.y, pos.z);
        
        if(tile instanceof ICustomPacketTile)
            ((ICustomPacketTile)tile).handleDescriptionPacket(packet);
    }
}
