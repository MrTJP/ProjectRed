package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.integration.GateLogic.ICounterGuiLogic;
import mrtjp.projectred.integration.GateLogic.ITimerGuiLogic;
import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.entity.player.EntityPlayerMP;
import net.minecraft.network.NetServerHandler;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IServerPacketHandler;
import codechicken.lib.vec.BlockCoord;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;

public class IntegrationSPH implements IServerPacketHandler {
    
    public static Object channel = ProjectRedIntegration.instance;
    
    @Override
    public void handlePacket(PacketCustom packet, NetServerHandler nethandler, EntityPlayerMP player) {
        switch (packet.getType()) {
            case 1:
                incrTimer(player.worldObj, packet);
                break;
            case 2:
                incCounter(player.worldObj, packet);
                break;
        }
    }
    
    private void incCounter(World world, PacketCustom packet) {
        TMultiPart part = readPartIndex(world, packet);
        if(part instanceof GatePart) {
            GatePart gate = (GatePart)part;
            if(gate.getLogic() instanceof ICounterGuiLogic) {
                ICounterGuiLogic t = (ICounterGuiLogic)gate.getLogic();
                int actionID = packet.readByte();
                
                if(actionID == 0)
                    t.setCounterMax(gate, t.getCounterMax()+packet.readShort());
                else if(actionID == 1)
                    t.setCounterIncr(gate, t.getCounterIncr()+packet.readShort());
                else if(actionID == 2)
                    t.setCounterDecr(gate, t.getCounterDecr()+packet.readShort());

            }
        }
    }

    private void incrTimer(World world, PacketCustom packet) {
        TMultiPart part = readPartIndex(world, packet);
        if(part instanceof GatePart) {
            GatePart gate = (GatePart)part;
            if(gate.getLogic() instanceof ITimerGuiLogic) {
                ITimerGuiLogic t = (ITimerGuiLogic)gate.getLogic();
                t.setTimerMax(gate, t.getTimerMax()+packet.readShort());
            }
        }
    }

    public static PacketCustom writePartIndex(PacketCustom out, TMultiPart part) {
        return out.writeCoord(new BlockCoord(part.tile()))
                .writeByte(part.tile().jPartList().indexOf(part));
    }
    
    public static TMultiPart readPartIndex(World world, PacketCustom in) {
        TileMultipart tile = BasicUtils.getMultipartTile(world, in.readCoord());
        try
        {
            return tile.jPartList().get(in.readUByte());
        }
        catch(NullPointerException e)
        {
            return null;
        }
        catch(IndexOutOfBoundsException e)
        {
            return null;
        }
    }

    public static void openTimerGui(EntityPlayer player, GatePart part) {
        PacketCustom packet = new PacketCustom(channel, 1);
        writePartIndex(packet, part);
        packet.sendToPlayer(player);
    }
    
    public static void openCounterGui(EntityPlayer player, GatePart part) {
        PacketCustom packet = new PacketCustom(channel, 2);
        writePartIndex(packet, part);
        packet.sendToPlayer(player);
    }
}
