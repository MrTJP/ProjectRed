package mrtjp.projectred;

import java.util.ArrayList;

import mrtjp.projectred.blocks.BlockLamp;
import mrtjp.projectred.blocks.BlockLamp.EnumLamp;
import mrtjp.projectred.blocks.BlockLantern;
import mrtjp.projectred.blocks.BlockLantern.EnumLantern;
import mrtjp.projectred.blocks.BlockMachines;
import mrtjp.projectred.blocks.BlockMachines.EnumMachine;
import mrtjp.projectred.blocks.ItemBlockLamp;
import mrtjp.projectred.blocks.ItemBlockLantern;
import mrtjp.projectred.blocks.ItemBlockMachines;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProjectRedModule;
import mrtjp.projectred.core.ModuleCore;
import mrtjp.projectred.crafting.CraftingRecipeManager;
import mrtjp.projectred.integration.ItemPartGate;
import mrtjp.projectred.integration.ItemScrewdriver;
import mrtjp.projectred.integration.ModuleIntegration;
import mrtjp.projectred.items.ItemBackpack;
import mrtjp.projectred.items.ItemBackpack.EnumBackpack;
import mrtjp.projectred.items.ItemDrawPlate;
import mrtjp.projectred.items.ItemPart;
import mrtjp.projectred.items.ItemVAWT;
import mrtjp.projectred.items.ItemWoolGin;
import mrtjp.projectred.multipart.wiring.CommandDebug;
import mrtjp.projectred.network.GuiHandler;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.tiles.TileLantern;
import mrtjp.projectred.transmission.ItemPartJacketedWire;
import mrtjp.projectred.transmission.ItemPartWire;
import mrtjp.projectred.transmission.ModuleTransmission;
import mrtjp.projectred.utils.BasicUtils;
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
 * "Project: Red" serves to provide a somewhat decent replacement to Eloraam's
 * RedPower 2. Some of the code is derived from Immibis's mods. His link is
 * provided below.
 * 
 * Hopefully in the near future, I will be able to finish the rewrite.
 * But for now, some of the core functionality remains the same from
 * his mod.
 * 
 * http://www.minecraftforum.net/topic/1001131-152-immibiss-mods-smp-tubestuff-5502-core-5513-da-5500-infinitubes-5502-liquid-xp-5511-microblocks-5501/
 * 
 * @author MrTJP
 * 
 */
@Mod(modid = Configurator.modId, name = Configurator.modName, version = Configurator.version + "." + Configurator.buildnumber, dependencies = "")
@NetworkMod(clientSideRequired = true, serverSideRequired = false, channels = { Configurator.modNetworkChannel }, packetHandler = PacketHandler.class)
public class ProjectRed {

	/** Multipart items **/
	public static ItemPartGate itemPartGate;
	public static ItemPartWire itemPartWire;
	public static ItemPartJacketedWire itemPartJacketedWire;
	
	/** Blocks **/
	public static BlockLamp blockLamp;
	public static BlockMachines blockMachines;
	public static BlockLantern blockLantern;

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
		// Lamps
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

		// Machines
		if (Configurator.block_machinesID.getInt() > 0) {
			blockMachines = new BlockMachines(Configurator.block_machinesID.getInt());
			GameRegistry.registerBlock(blockMachines, ItemBlockMachines.class, "projectred.machines");
			for (EnumMachine m : EnumMachine.VALID_MACHINES) {
				GameRegistry.registerTileEntity(m.clazz, "tile.projectred.machines." + m.unlocalname);
				LanguageRegistry.addName(new ItemStack(blockMachines, 1, m.meta), m.fullname);
			}
		}
		
		// Wire block
		/**
		if (Configurator.block_wireID.getInt() > 0) {
			blockWire = new BlockWire(Configurator.block_wireID.getInt());
			GameRegistry.registerBlock(blockWire, ItemBlockWire.class, "projred.redwire");
			GameRegistry.registerTileEntity(TilePlainRedAlloy.class, "tile.projred.redwire.alloy");
			GameRegistry.registerTileEntity(TileInsulatedRedAlloy.class, "tile.projred.redwire.insulated");
			GameRegistry.registerTileEntity(TileBundled.class, "tile.projred.redwire.bundled");
			GameRegistry.registerTileEntity(InvalidTile.class, "tile.projectred.invalideErrorTile");
			for (EnumWire w : EnumWire.VALUES) {
				LanguageRegistry.addName(new ItemStack(blockWire, 1, w.ordinal()), w.name);
				if (w.hasJacketedForm()) {
					LanguageRegistry.addName(new ItemStack(blockWire, 1, w.ordinal() | WireDamageValues.DMG_FLAG_JACKETED), "Jacketed " + w.name);
				}
			}
		}
		*/
		
		// Wool Gin
		if (Configurator.item_woolginID.getInt() > 0) {
			itemWoolGin = new ItemWoolGin(Configurator.item_woolginID.getInt());
			LanguageRegistry.addName(itemWoolGin, "Wool Gin");
		}

		// Backpacks
		if (Configurator.item_backpackID.getInt() > 0) {
			itemBackpack = new ItemBackpack(Configurator.item_backpackID.getInt());
			for (EnumBackpack b : EnumBackpack.VALID_BP) {
				LanguageRegistry.addName(b.getItemStack(), b.fullname);
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

		BasicUtils.proxy.initRenderings();
		BasicUtils.proxy.registerEventsAndHandlers();
		NetworkRegistry.instance().registerGuiHandler(this, new GuiHandler());
		CraftingRecipeManager.initRecipes();
		BasicUtils.proxy.initOreDictionaryDefinitions();
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
