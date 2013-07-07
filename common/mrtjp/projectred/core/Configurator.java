package mrtjp.projectred.core;

import java.io.File;

import net.minecraftforge.common.Configuration;
import net.minecraftforge.common.Property;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;

public class Configurator {
	protected static File _configFolder;
	protected static File _configFile;
	
	/** Constants **/
	public static final String modId = "ProjectRed";
	public static final String modNetworkChannel = "ProjRed";
	public static final String version = "@VERSION@";
	public static final String buildnumber = "@BUILD@";
	public static final String modName = "Project: Red";


	/** Block IDs **/
	public static Property block_microID;
	public static Property block_wireID;
	public static Property block_gateID;
	public static Property block_lampID;
	public static Property block_machinesID;

	/** Item IDs**/
	public static Property item_sawID;
	public static Property item_screwdriverID;
	public static Property item_componentsID;
	public static Property item_drawplateID;
	public static Property item_woolginID;
	public static Property item_backpackID;

	/** Settings **/
	public static Property networkUpdateRange;
	public static Property enableNEIMicroblock;
	public static Property debugMode;

	
	public static void initConfig(FMLPreInitializationEvent event) {
		_configFolder = event.getModConfigurationDirectory();
		_configFile = new File(_configFolder.getAbsolutePath() + "/ProjectRed.cfg");
		loadPropertiesFromFile(_configFile);
	}
	
	public static void loadPropertiesFromFile(File file) {
		Configuration localConfig = new Configuration(file);
		localConfig.load();
		
		block_microID = localConfig.getBlock("block_microID", 3145);
		block_wireID = localConfig.getBlock("block_wireID", 3146);
		block_gateID = localConfig.getBlock("block_gateID", 3147);
		block_lampID = localConfig.getBlock("block_lightingID", 3128);
		block_machinesID = localConfig.getBlock("block_machinesID", 3129);

		item_sawID = localConfig.getItem("item_sawID", 12023);
		item_screwdriverID = localConfig.getItem("item_screwdriverID", 12024);
		item_componentsID = localConfig.getItem("item_componentsID", 12025);
		item_drawplateID = localConfig.getItem("item_drawplateID", 12026);
		item_woolginID = localConfig.getItem("item_woolgin", 12027);
		item_backpackID = localConfig.getItem("item_liquidSolidifierID", 12028);
		
		networkUpdateRange = localConfig.get("general", "Network Update Range", 50.0D);
		networkUpdateRange.comment = "This is the distance in which players will be notified.  Lower if you experience lag.";
		
		enableNEIMicroblock = localConfig.get("general", "Enable NEI Microblocks", false);
		enableNEIMicroblock.comment = "This allows for showing microblocks in NEI.";
		
		debugMode = localConfig.get("general", "Enable Debugging", false);
		debugMode.comment = "Enable advanced debugging, should ALWAYS be false.";
		
		localConfig.save();
	}
}