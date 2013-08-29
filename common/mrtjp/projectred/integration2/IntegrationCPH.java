package mrtjp.projectred.integration2;

import static mrtjp.projectred.integration2.IntegrationSPH.readPartIndex;
import mrtjp.projectred.ProjectRedIntegration;
import mrtjp.projectred.integration2.GateLogic.ICounterGuiLogic;
import mrtjp.projectred.integration2.GateLogic.ITimerGuiLogic;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.NetClientHandler;
import net.minecraft.world.World;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.packet.PacketCustom.IClientPacketHandler;
import codechicken.multipart.TMultiPart;

public class IntegrationCPH implements IClientPacketHandler {
    public static Object channel = ProjectRedIntegration.instance;
    
    @Override
    public void handlePacket(PacketCustom packet, NetClientHandler nethandler, Minecraft mc) {

        switch (packet.getType()) {
            case 1:
                openTimerGui(mc, mc.theWorld, packet);
                break;
            case 2:
                openCounterGui(mc, mc.theWorld, packet);
                break;
        }
    }

    private void openTimerGui(Minecraft mc, World world, PacketCustom packet) {
        TMultiPart part = readPartIndex(world, packet);
        if(part instanceof GatePart) {
            GatePart gate = (GatePart)part;
            if(gate.getLogic() instanceof ITimerGuiLogic)
                mc.displayGuiScreen(new GuiTimer(gate));
        }
    }
    
    private void openCounterGui(Minecraft mc, World world, PacketCustom packet) {
        TMultiPart part = readPartIndex(world, packet);
        if(part instanceof GatePart) {
            GatePart gate = (GatePart)part;
            if(gate.getLogic() instanceof ICounterGuiLogic)
                mc.displayGuiScreen(new GuiCounter(gate));
        }
    }
}
