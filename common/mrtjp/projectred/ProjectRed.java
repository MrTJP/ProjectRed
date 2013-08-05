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
import mrtjp.projectred.core.ItemWoolGin;
import mrtjp.projectred.core.ModuleCore;
import mrtjp.projectred.expansion.BlockMachines;
import mrtjp.projectred.expansion.BlockMachines.EnumMachine;
import mrtjp.projectred.expansion.ItemBlockMachines;
import mrtjp.projectred.expansion.ItemVAWT;
import mrtjp.projectred.illumination.ItemPartLamp;
import mrtjp.projectred.illumination.ItemPartLantern;
import mrtjp.projectred.illumination.ModuleIllumination;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.integration.ItemScrewdriver;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.transmission.ItemPartJacketedWire;
import mrtjp.projectred.transmission.ItemPartWire;
import mrtjp.projectred.transmission.ModuleTransmission;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.Mod;
import cpw.mods.fml.common.Mod.Instance;
import cpw.mods.fml.common.event.FMLInitializationEvent;
import cpw.mods.fml.common.event.FMLPostInitializationEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.event.FMLServerStartingEvent;
import cpw.mods.fml.common.network.NetworkMod;
import cpw.mods.fml.common.network.NetworkRegistry;
import cpw.mods.fml.common.registry.GameRegistry;
import cpw.mods.fml.common.registry.LanguageRegistry;

/**
 * "Project: Red" serves to provide a complete alternative for Eloraam's
 * RedPower 2 utilizing ForgeMultipart API created by Chickenbones.
 * 
 * @author MrTJP
 * 
 */
@Mod(modid = Configurator.modId, name = Configurator.modName, version = Configurator.version + "." + Configurator.buildnumber, dependencies = "")
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

	/** Items **/
	public static ItemScrewdriver itemScrewdriver;
	public static ItemPart itemComponent;
	public static ItemDrawPlate itemDrawPlate;
	public static ItemWoolGin itemWoolGin;
	public static ItemBackpack itemBackpack;
	public static ItemVAWT itemVAWT;


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
	
		/**	// Lamps
		if (Configurator.block_lampID.getInt() > 0) {
			blockLamp = new BlockLamp(Configurator.block_lampID.getInt());
			GameRegistry.registerBlock(blockLamp, ItemBlockLamp.class, "projred.lighting.lamp");
			GameRegistry.registerTileEntity(TileLamp.class, "tile.projectred.lighting.lamp");
			for (int i = 0; i < 32; i++) {
				String desc = i > 15 ? "Inverted " : "";
				int color = i > 15 ? i - 16 : i;
				LanguageRegistry.addName(new ItemStack(blockLamp, 1, i), desc + EnumLamp.get(color).fullName);
			}
		}

		// Lanterns
		if (Configurator.block_lanternID.getInt() > 0) {
			blockLantern = new BlockLantern(Configurator.block_lanternID.getInt());
			GameRegistry.registerBlock(blockLantern, ItemBlockLantern.class, "projred.lighting.lantern");
			GameRegistry.registerTileEntity(TileLantern.class, "tile.projectred.lighting.lantern");
			for (int i = 0; i < 32; i++) {
				String desc = i > 15 ? "Inverted " : "";
				int color = i > 15 ? i - 16 : i;
				LanguageRegistry.addName(new ItemStack(blockLantern, 1, i), desc + EnumLantern.get(color).fullName);
			}
		}
**/
		// Machines
		if (Configurator.block_machinesID.getInt() > 0) {
			blockMachines = new BlockMachines(Configurator.block_machinesID.getInt());
			GameRegistry.registerBlock(blockMachines, ItemBlockMachines.class, "projectred.machines");
			for (EnumMachine m : EnumMachine.VALID_MACHINES) {
				GameRegistry.registerTileEntity(m.clazz, "tile.projectred.machines." + m.unlocalname);
				LanguageRegistry.addName(new ItemStack(blockMachines, 1, m.meta), m.fullname);
			}
		}
		
		// VAWT
		if (Configurator.item_vawtID.getInt() > 0) {
			itemVAWT = new ItemVAWT(Configurator.item_vawtID.getInt());
			LanguageRegistry.addName(new ItemStack(itemVAWT, 1, 0), "Vertical-Axis Wind Turbine");
		}

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
