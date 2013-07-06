package mrtjp.projectred.crafting;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.multipart.microblocks.ItemMicroblock;
import mrtjp.projectred.multipart.microblocks.MicroblockSystem;
import codechicken.nei.MultiItemRange;
import codechicken.nei.api.API;
import codechicken.nei.api.IConfigureNEI;

public class NEIMicroblocksConfig implements IConfigureNEI {
	@Override
	public void loadConfig() {
		if (MicroblockSystem.instance != null) {
			try {
				if (Configurator.enableNEIMicroblock.getBoolean(false)) {
					API.setMaxDamageException(ProjectRed.blockMicrocontainer.blockID, 1);
					for (int i : MicroblockSystem.neiPartIDs)
						API.addNBTItem(ItemMicroblock.getStackWithPartID(i));
					API.addSetRange("Blocks.Microblocks", new MultiItemRange().add(ProjectRed.blockMicrocontainer.blockID));
				} else {
					API.hideItem(ProjectRed.blockMicrocontainer.blockID);
				}
			} catch (Throwable e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	public String getName() {
		return "Project Red";
	}

	@Override
	public String getVersion() {
		return Configurator.version;
	}
}
