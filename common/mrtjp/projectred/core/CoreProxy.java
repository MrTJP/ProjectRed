package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.itemBackpack;
import static mrtjp.projectred.ProjectRed.itemComponent;
import static mrtjp.projectred.ProjectRed.itemDrawPlate;
import static mrtjp.projectred.ProjectRed.itemWoolGin;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.exploration.ItemWoolGin;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class CoreProxy implements IProxy {

    @Override
    public void preinit() {
    }

    @Override
    public void init() {
        itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());
        itemBackpack = new ItemBackpack(Configurator.item_backpackID.getInt());

        EnumPart.initOreDictDefinitions();
        CoreRecipes.initCoreRecipes();
    }

    @Override
    public void postinit() {
    }
}
