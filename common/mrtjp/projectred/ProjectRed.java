package mrtjp.projectred;

import java.util.ArrayList;

import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.CommandDebug;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.GuiHandler;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.ItemBackpack;
import mrtjp.projectred.core.ItemDrawPlate;
import mrtjp.projectred.core.ItemPart;
import mrtjp.projectred.core.ModuleCore;
import mrtjp.projectred.expansion.BlockMachines;
import mrtjp.projectred.expansion.ItemVAWT;
import mrtjp.projectred.expansion.ModuleExpansion;
import mrtjp.projectred.exploration.BlockOre;
import mrtjp.projectred.exploration.BlockSpecialStone;
import mrtjp.projectred.exploration.ItemGemAxe;
import mrtjp.projectred.exploration.ItemGemHoe;
import mrtjp.projectred.exploration.ItemGemPickaxe;
import mrtjp.projectred.exploration.ItemGemSaw;
import mrtjp.projectred.exploration.ItemGemShovel;
import mrtjp.projectred.exploration.ItemGemSickle;
import mrtjp.projectred.exploration.ItemGemSword;
import mrtjp.projectred.exploration.ItemWoolGin;
import mrtjp.projectred.exploration.ModuleExploration;
import mrtjp.projectred.illumination.ItemPartLamp;
import mrtjp.projectred.illumination.ItemPartLantern;
import mrtjp.projectred.illumination.ModuleIllumination;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.integration.ItemScrewdriver;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.transmission.ItemPartJacketedWire;
import mrtjp.projectred.transmission.ItemPartWire;
import mrtjp.projectred.transmission.ModuleTransmission;
import net.minecraft.item.EnumToolMaterial;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.network.NetworkMod;
import cpw.mods.fml.common.network.NetworkRegistry;

/**
 * "Project: Red" serves to provide a complete alternative for Eloraam's
 * RedPower 2 utilizing ForgeMultipart API created by Chickenbones.
 * 
 * @author MrTJP
 * 
 */
@Mod(modid = Configurator.modId, name = Configurator.modName, version = Configurator.version + "." + Configurator.buildnumber, acceptedMinecraftVersions = "[1.6.2]", 
dependencies = 
        "required-after:Forge@FORGE_VERSION@;" +
        "required-after:ForgeMultipart;" +
        "required-after:CodeChickenCore;" +
        "after:CCTurtle;" +
        "after:ComputerCraft;"
)
@NetworkMod(clientSideRequired = true, serverSideRequired = true)
public class ProjectRed {

    /** Multipart items **/
    public static ItemPartGate itemPartGate;
    public static ItemPartWire itemPartWire;
    public static ItemPartJacketedWire itemPartJacketedWire;
    public static ItemPartLantern itemPartLantern;
    public static ItemPartLantern itemPartInvLantern;
    public static ItemPartLamp itemPartLamp;
    public static ItemPartLamp itemPartInvLamp;
    
    /** Blocks **/
    public static BlockMachines blockMachines;
    public static BlockOre blockOres;
    public static BlockSpecialStone blockStones;

    /** Items **/
    public static ItemScrewdriver itemScrewdriver;
    public static ItemPart itemComponent;
    public static ItemDrawPlate itemDrawPlate;
    public static ItemWoolGin itemWoolGin;
    public static ItemBackpack itemBackpack;
    public static ItemVAWT itemVAWT;
    
    public static EnumToolMaterial toolMaterialRuby;
    public static EnumToolMaterial toolMaterialSapphire;
    public static EnumToolMaterial toolMaterialPeridot;
    
    public static ItemGemAxe itemRubyAxe;
    public static ItemGemAxe itemSapphireAxe;
    public static ItemGemAxe itemPeridotAxe;
    
    public static ItemGemHoe itemRubyHoe;
    public static ItemGemHoe itemSapphireHoe;
    public static ItemGemHoe itemPeridotHoe;
    
    public static ItemGemPickaxe itemRubyPickaxe;
    public static ItemGemPickaxe itemSapphirePickaxe;
    public static ItemGemPickaxe itemPeridotPickaxe;
    
    public static ItemGemShovel itemRubyShovel;
    public static ItemGemShovel itemSapphireShovel;
    public static ItemGemShovel itemPeridotShovel;
    
    public static ItemGemSword itemRubySword;
    public static ItemGemSword itemSapphireSword;
    public static ItemGemSword itemPeridotSword;
    
    public static ItemGemSaw itemWoodSaw;
    public static ItemGemSaw itemStoneSaw;
    public static ItemGemSaw itemIronSaw;
    public static ItemGemSaw itemGoldSaw;
    public static ItemGemSaw itemRubySaw;
    public static ItemGemSaw itemSapphireSaw;
    public static ItemGemSaw itemPeridotSaw;
    public static ItemGemSaw itemDiamondSaw;
    
    public static ItemGemSickle itemWoodSickle;
    public static ItemGemSickle itemStoneSickle;
    public static ItemGemSickle itemIronSickle;
    public static ItemGemSickle itemGoldSickle;
    public static ItemGemSickle itemRubySickle;
    public static ItemGemSickle itemSapphireSickle;
    public static ItemGemSickle itemPeridotSickle;
    public static ItemGemSickle itemDiamondSickle;
    


    @Instance("ProjectRed")
    public static ProjectRed instance;

    
    public static ArrayList<IProjectRedModule> registeredModules = new ArrayList<IProjectRedModule>();
    public static ArrayList<IProjectRedModule> initializedModules = new ArrayList<IProjectRedModule>();

    public static boolean registerModule(IProjectRedModule m) {
        if (m == null) {
            if (Configurator.module_Core.getBoolean(true)) {
                registerModule(new ModuleCore());
            }
            if (Configurator.module_Integration.getBoolean(true)) {
                registerModule(new ModuleIntegration());
            }
            if (Configurator.module_Transmission.getBoolean(true)) {
                registerModule(new ModuleTransmission());
            }
            if (Configurator.module_Illumination.getBoolean(true)) {
                registerModule(new ModuleIllumination());
            }
            if (Configurator.module_Expansion.getBoolean(true)) {
                registerModule(new ModuleExpansion());
            }
            if (Configurator.module_Exploration.getBoolean(true)) {
                registerModule(new ModuleExploration());
            }
            return false;
        }
        return registeredModules.add(m);
    }
    
    
    @Mod.EventHandler
    public void preInit(FMLPreInitializationEvent event) {
        Configurator.initConfig(event);
        registerModule(null);
        BasicUtils.proxy.preinit();
    }

    @Mod.EventHandler
    public void init(FMLInitializationEvent event) {
        MinecraftForge.EVENT_BUS.register(instance);
        MinecraftForge.EVENT_BUS.register(BasicUtils.proxy);
        BasicUtils.proxy.init();
        NetworkRegistry.instance().registerGuiHandler(this, new GuiHandler());
    }

    @Mod.EventHandler
    public void postInit(FMLPostInitializationEvent event) {
        BasicUtils.proxy.postinit();
    }

    @Mod.EventHandler
    public void onServerStarting(FMLServerStartingEvent event) {
        event.registerServerCommand(new CommandDebug());
    }

}
