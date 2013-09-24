package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.blockMachines;
import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import static mrtjp.projectred.ProjectRedCore.itemScrewdriver;
import static mrtjp.projectred.ProjectRedCore.itemWireDebugger;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.ItemPart.EnumPart;
import cpw.mods.fml.common.registry.GameRegistry;

public class CoreProxy implements IProxy {
    
    public static int basicRenderID = 0;

    @Override
    public void preinit() {
    }

    @Override
    public void init() {
        itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());
        itemScrewdriver = new ItemScrewdriver(Configurator.item_screwdriverID.getInt());
        itemWireDebugger = new ItemWireDebugger(Configurator.item_wireDebuggerID.getInt());

        blockMachines = new BlockBasics(Configurator.block_machinesID.getInt());
        GameRegistry.registerBlock(blockMachines, ItemBlockBasics.class, "projectred.core.appliance");
        
        for (EnumBasics m : EnumBasics.VALID_MACHINES)
            GameRegistry.registerTileEntity(m.clazz, "tile.projectred.core.appliance|" + m.meta);
        
        EnumPart.initOreDictDefinitions();
    }

    @Override
    public void postinit() {
        CoreRecipes.initCoreRecipes();
    }
}
