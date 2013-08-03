package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.itemComponent;
import static mrtjp.projectred.ProjectRed.itemDrawPlate;
import net.minecraftforge.common.MinecraftForge;
import mrtjp.projectred.items.ItemDrawPlate;
import mrtjp.projectred.items.ItemPart;
import mrtjp.projectred.items.ItemPart.EnumPart;
import cpw.mods.fml.common.registry.LanguageRegistry;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class CoreProxy implements IProxy {

	@Override
	public void preinit() {}

	@Override
	public void init() {
		itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
		itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());

		EnumPart.initOreDictDefinitions();
		CoreRecipes.initCoreRecipes();
		
		MinecraftForge.EVENT_BUS.register(new Messenger());
		TickRegistry.registerTickHandler(ProjectRedTickHandler.instance, Side.CLIENT);
	}

	@Override
	public void postinit() {}

	@Override
	public void initRenderings() {}

	@Override
	public void registerEventsAndHandlers() {}

	@Override
	public void initOreDictionaryDefinitions() {}

}
