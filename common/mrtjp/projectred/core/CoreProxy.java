package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import static mrtjp.projectred.ProjectRedExploration.itemBackpack;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.exploration.ItemBackpack;

public class CoreProxy implements IProxy {
    public static final int messengerQueue = 2;

    @Override
    public void preinit() {
    }

    @Override
    public void init() {
        itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());

        EnumPart.initOreDictDefinitions();
    }

    @Override
    public void postinit() {
        CoreRecipes.initCoreRecipes();
    }
}
