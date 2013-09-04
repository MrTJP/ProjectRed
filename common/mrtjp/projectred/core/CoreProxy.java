package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.blockMachines;
import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.ItemPart.EnumPart;
import cpw.mods.fml.common.registry.GameRegistry;

public class CoreProxy implements IProxy {
    public static final int messengerQueue = 2;
    public static final int alloySmelterWatcherUpdate = 3;

    @Override
    public void preinit() {
    }

    @Override
    public void init() {
        itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());

        blockMachines = new BlockBasics(Configurator.block_machinesID.getInt());
        GameRegistry.registerBlock(blockMachines, ItemBlockBasics.class, "projectred.expansion.machines");
        for (EnumBasics m : EnumBasics.VALID_MACHINES) {
            GameRegistry.registerTileEntity(m.clazz, "tile.projectred.machines." + m.unlocalname);
        }

        EnumPart.initOreDictDefinitions();
    }

    @Override
    public void postinit() {
        CoreRecipes.initCoreRecipes();
    }
}
