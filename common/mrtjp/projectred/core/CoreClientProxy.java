package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.itemComponent;
import static mrtjp.projectred.ProjectRed.itemDrawPlate;
import mrtjp.projectred.items.ItemPart.EnumPart;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class CoreClientProxy extends CoreProxy {

	@Override
	public void init() {
		for (EnumPart part : EnumPart.VALID_PARTS) {
			LanguageRegistry.addName(new ItemStack(itemComponent, 1, part.meta), part.fullName);
		}
		LanguageRegistry.addName(itemDrawPlate, "Draw Plate");

	}
}
