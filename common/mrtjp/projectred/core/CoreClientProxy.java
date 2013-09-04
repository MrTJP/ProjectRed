package mrtjp.projectred.core;

<<<<<<< HEAD
=======
import static mrtjp.projectred.ProjectRedCore.blockMachines;
import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.item.ItemStack;
>>>>>>> upstream/master
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;

public class CoreClientProxy extends CoreProxy {

    @Override
    public void init() {
        super.init();
        /*for (EnumPart part : EnumPart.VALID_PARTS) {
            LanguageRegistry.addName(new ItemStack(itemComponent, 1, part.meta), part.fullName);
        }
        
<<<<<<< HEAD
        LanguageRegistry.addName(itemDrawPlate, "Draw Plate");*/
=======
        for (EnumBasics m : EnumBasics.VALID_MACHINES) {
            LanguageRegistry.addName(new ItemStack(blockMachines, 1, m.meta), m.fullname);
        }

        LanguageRegistry.addName(itemDrawPlate, "Draw Plate");
>>>>>>> upstream/master
                
        MinecraftForge.EVENT_BUS.register(new Messenger());
        PacketCustom.assignHandler(Configurator.corePacketChannel, 1, 32, new CoreCPH());
    }
}
