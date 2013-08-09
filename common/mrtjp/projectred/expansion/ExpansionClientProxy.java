package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRed.blockMachines;
import static mrtjp.projectred.ProjectRed.itemVAWT;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import net.minecraft.item.ItemStack;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExpansionClientProxy extends ExpansionProxy {

    @Override
    public void init() {
        PacketCustom.assignHandler(Configurator.expansionPacketChannel, 0, 32, new ExpansionCPH());
        
        for (EnumMachine m : EnumMachine.VALID_MACHINES) {
            LanguageRegistry.addName(new ItemStack(blockMachines, 1, m.meta), m.fullname);
        }
        LanguageRegistry.addName(new ItemStack(itemVAWT, 1, 0), "Vertical-Axis Wind Turbine");
    }
}
