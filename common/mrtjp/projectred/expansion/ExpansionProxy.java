package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRed.blockMachines;
import static mrtjp.projectred.ProjectRed.itemVAWT;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExpansionProxy implements IProxy {

    @Override
    public void preinit() {
        blockMachines = new BlockMachines(Configurator.block_machinesID.getInt());
        GameRegistry.registerBlock(blockMachines, ItemBlockMachines.class, "projectred.expansion.machines");
        for (EnumMachine m : EnumMachine.VALID_MACHINES) {
            GameRegistry.registerTileEntity(m.clazz, "tile.projectred.machines." + m.unlocalname);
        }

        itemVAWT = new ItemVAWT(Configurator.item_vawtID.getInt());
    }

    @Override
    public void init() {
        ExpansionRecipes.initRecipes();
    }

    @Override
    public void postinit() {
        
    }

}
