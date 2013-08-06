package mrtjp.projectred.expansion;

import net.minecraft.item.ItemStack;
import static mrtjp.projectred.ProjectRed.*;
import cpw.mods.fml.common.registry.GameRegistry;
import cpw.mods.fml.common.registry.LanguageRegistry;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;

public class ExpansionProxy implements IProxy {

	@Override
	public void preinit() {

	}

	@Override
	public void init() {
		blockMachines = new BlockMachines(Configurator.block_machinesID.getInt());
		GameRegistry.registerBlock(blockMachines, ItemBlockMachines.class, "projectred.machines");
		for (EnumMachine m : EnumMachine.VALID_MACHINES) {
			GameRegistry.registerTileEntity(m.clazz, "tile.projectred.machines." + m.unlocalname);
		}

		itemVAWT = new ItemVAWT(Configurator.item_vawtID.getInt());

	}

	@Override
	public void postinit() {
		
	}

}
