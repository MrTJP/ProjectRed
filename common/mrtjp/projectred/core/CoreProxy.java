package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.*;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.registry.TickRegistry;
import cpw.mods.fml.relauncher.Side;

public class CoreProxy implements IProxy
{
    @Override
    public void preinit()
    {
        MinecraftForge.EVENT_BUS.register(RetroactiveWorldGenerator.instance);
        TickRegistry.registerTickHandler(RetroactiveWorldGenerator.instance, Side.SERVER);
    }

    @Override
    public void init()
    {
        itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
        itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());
        itemScrewdriver = new ItemScrewdriver(Configurator.item_screwdriverID.getInt());
        itemWireDebugger = new ItemWireDebugger(Configurator.item_wireDebuggerID.getInt());
        itemDataCard = new ItemDataCard(Configurator.item_dataCardID.getInt());
        
        EnumPart.initOreDictDefinitions();
    }

    @Override
    public void postinit()
    {
        CoreRecipes.initCoreRecipes();
    }
}
