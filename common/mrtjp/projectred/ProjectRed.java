package mrtjp.projectred;

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
import mrtjp.projectred.crafting.CraftingRecipeManager;
import mrtjp.projectred.items.ItemBackpack;
import mrtjp.projectred.items.ItemBackpack.EnumBackpack;
import mrtjp.projectred.items.ItemDrawPlate;
import mrtjp.projectred.items.ItemPart;
import mrtjp.projectred.items.ItemPart.EnumPart;
import mrtjp.projectred.items.ItemSaw;
import mrtjp.projectred.items.ItemScrewdriver;
import mrtjp.projectred.items.ItemWoolGin;
import mrtjp.projectred.multipart.BlockMultipartBase;
import mrtjp.projectred.multipart.microblocks.BlockMicroblockContainer;
import mrtjp.projectred.multipart.microblocks.ItemBlockMicroblock;
import mrtjp.projectred.multipart.microblocks.MicroblockLibrary;
import mrtjp.projectred.multipart.microblocks.TileMicroblockContainer;
import mrtjp.projectred.multipart.wiring.CommandDebug;
import mrtjp.projectred.multipart.wiring.InvalidTile;
import mrtjp.projectred.multipart.wiring.gates.BlockGate;
import mrtjp.projectred.multipart.wiring.gates.EnumGate;
import mrtjp.projectred.multipart.wiring.gates.ItemBlockGate;
import mrtjp.projectred.multipart.wiring.gates.TileGate;
import mrtjp.projectred.multipart.wiring.wires.BlockWire;
import mrtjp.projectred.multipart.wiring.wires.EnumWire;
import mrtjp.projectred.multipart.wiring.wires.EnumWire.WireDamageValues;
import mrtjp.projectred.multipart.wiring.wires.ItemBlockWire;
import mrtjp.projectred.multipart.wiring.wires.TileBundled;
import mrtjp.projectred.multipart.wiring.wires.TileInsulatedRedAlloy;
import mrtjp.projectred.multipart.wiring.wires.TilePlainRedAlloy;
import mrtjp.projectred.network.GuiHandler;
import mrtjp.projectred.network.PacketHandler;
import mrtjp.projectred.tiles.TileLamp;
import mrtjp.projectred.tiles.TileLantern;
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
 * RedPower 2. Most of the code behind the multipart blocks are derived from
 * Immibis's mods. His link is provided below.
 * http://www.minecraftforum.net/topic
 * /1001131-152-immibiss-mods-smp-tubestuff-5502
 * -core-5513-da-5500-infinitubes-5502-liquid-xp-5511-microblocks-5501/
 * Hopefully in the near future, I will be able to rewrite everything into my
 * own code. But for now, most of the core functionality remains the same from
 * his mod.
 * 
 * @author MrTJP
 * 
 */
@Mod(modid = Configurator.modId, name = Configurator.modName, version = Configurator.version + "." + Configurator.buildnumber, dependencies = "")
@NetworkMod(clientSideRequired = true, serverSideRequired = false, channels = { Configurator.modNetworkChannel }, packetHandler = PacketHandler.class)
public class ProjectRed {

	/** Blocks **/
	public static BlockMultipartBase blockMicrocontainer;
	public static BlockGate blockGate;
	public static BlockWire blockWire;
	public static BlockLamp blockLamp;
	public static BlockMachines blockMachines;
	public static BlockLantern blockLantern;

	/** Items **/
	public static ItemScrewdriver itemScrewdriver;
	public static ItemSaw itemSaw;
	public static ItemPart itemComponent;
	public static ItemDrawPlate itemDrawPlate;
	public static ItemWoolGin itemWoolGin;
	public static ItemBackpack itemBackpack;

	@Instance("ProjectRed")
	public static ProjectRed instance;

	@Mod.PreInit
	public void preInit(FMLPreInitializationEvent event) {
		Configurator.initConfig(event);
	}

	@Mod.Init
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

		// Component parts
		if (Configurator.item_componentsID.getInt() > 0) {
			itemComponent = new ItemPart(Configurator.item_componentsID.getInt());
			for (EnumPart part : EnumPart.VALID_PARTS) {
				LanguageRegistry.addName(new ItemStack(itemComponent, 1, part.meta), part.fullName);
			}
		}

		// Gate block
		if (Configurator.block_gateID.getInt() > 0) {
			blockGate = new BlockGate(Configurator.block_gateID.getInt());
			GameRegistry.registerBlock(blockGate, ItemBlockGate.class, "projred.gate");
			GameRegistry.registerTileEntity(TileGate.class, "tile.projred.gate");
			for (EnumGate g : EnumGate.VALUES) {
				LanguageRegistry.addName(new ItemStack(blockGate, 1, g.ordinal()), g.name);
			}
		}
		// Wire block
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
		// Microblock
		if (Configurator.block_microID.getInt() > 0) {
			blockMicrocontainer = new BlockMicroblockContainer(Configurator.block_microID.getInt());
			GameRegistry.registerBlock(blockMicrocontainer, ItemBlockMicroblock.class, "projred.microblock");
			GameRegistry.registerTileEntity(TileMicroblockContainer.class, "tile.projred.microblock");
		}

		// Saw
		if (Configurator.item_sawID.getInt() > 0) {
			itemSaw = new ItemSaw(Configurator.item_sawID.getInt());
			LanguageRegistry.addName(itemSaw, "Saw");
		}

		// Screwdriver
		if (Configurator.item_screwdriverID.getInt() > 0) {
			itemScrewdriver = new ItemScrewdriver(Configurator.item_screwdriverID.getInt());
			LanguageRegistry.addName(itemScrewdriver, "Screwdriver");
		}

		// Draw Plate
		if (Configurator.item_drawplateID.getInt() > 0) {
			itemDrawPlate = new ItemDrawPlate(Configurator.item_drawplateID.getInt());
			LanguageRegistry.addName(itemDrawPlate, "Draw Plate");
		}

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

		MinecraftForge.EVENT_BUS.register(instance);
		MinecraftForge.EVENT_BUS.register(BasicUtils.proxy);

		BasicUtils.proxy.initRenderings();
		BasicUtils.proxy.registerEventsAndHandlers();
		NetworkRegistry.instance().registerGuiHandler(this, new GuiHandler());
		CraftingRecipeManager.initRecipes();
		BasicUtils.proxy.initOreDictionaryDefinitions();
	}

	@Mod.PostInit
	public void postInit(FMLPostInitializationEvent event) {
		MicroblockLibrary.instance = new MicroblockLibrary();
		//MicroblockLibrary.instance.initializeParts();
		MicroblockLibrary.instance.initializeBlockScan();
	}

	@Mod.ServerStarting
	public void onServerStarting(FMLServerStartingEvent event) {
		event.registerServerCommand(new CommandDebug());
	}

}
