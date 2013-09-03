package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.exploration.ItemBackpack.EnumBackpack;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;

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
