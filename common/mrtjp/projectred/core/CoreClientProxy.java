package mrtjp.projectred.core;

import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;

public class CoreClientProxy extends CoreProxy {

    @Override
    public void init() {
        super.init();
        /*for (EnumPart part : EnumPart.VALID_PARTS) {
            LanguageRegistry.addName(new ItemStack(itemComponent, 1, part.meta), part.fullName);
        }
        
        LanguageRegistry.addName(itemDrawPlate, "Draw Plate");*/
                
        MinecraftForge.EVENT_BUS.register(new Messenger());
        PacketCustom.assignHandler(Configurator.corePacketChannel, 1, 32, new CoreCPH());
    }
}
