package mrtjp.projectred.core;

import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;

public class CoreClientProxy extends CoreProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForge.EVENT_BUS.register(new Messenger());
        PacketCustom.assignHandler(CoreCPH.channel, new CoreCPH());
    }
}
