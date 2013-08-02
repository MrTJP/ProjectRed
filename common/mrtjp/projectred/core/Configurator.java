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
	public static Property block_lanternID;

	/** Item IDs**/
	public static Property item_sawID;
	public static Property item_screwdriverID;
	public static Property item_componentsID;
	public static Property item_drawplateID;
	public static Property item_woolginID;
	public static Property item_backpackID;
	public static Property item_vawtID;
        public static Property item_sickleID;

	/** Settings **/
	public static Property networkUpdateRange;
	public static Property debugMode;

	
	public static void initConfig(FMLPreInitializationEvent event) {
		_configFolder = event.getModConfigurationDirectory();
		_configFile = new File(_configFolder.getAbsolutePath() + "/ProjectRed.cfg");
		loadPropertiesFromFile(_configFile);
	}
	
	public static void loadPropertiesFromFile(File file) {
		Configuration localConfig = new Configuration(file);
		localConfig.load();
		
		block_microID = localConfig.getBlock("block_microID", 2145);
		block_wireID = localConfig.getBlock("block_wireID", 2146);
		block_gateID = localConfig.getBlock("block_gateID", 2147);
		block_lampID = localConfig.getBlock("block_lightingID", 2128);
		block_machinesID = localConfig.getBlock("block_machinesID", 2129);
		block_lanternID = localConfig.getBlock("block_lanternID", 2130);

		item_sawID = localConfig.getItem("item_sawID", 9023);
		item_screwdriverID = localConfig.getItem("item_screwdriverID", 9024);
		item_componentsID = localConfig.getItem("item_componentsID", 9025);
		item_drawplateID = localConfig.getItem("item_drawplateID", 9026);
		item_woolginID = localConfig.getItem("item_woolginID", 9027);
		item_backpackID = localConfig.getItem("item_backpackID", 9028);
		item_vawtID = localConfig.getItem("item_turbineID", 9029);
                item_sickleID = localConfig.getItem("item_sickleID", 9030);
		
		networkUpdateRange = localConfig.get("general", "Network Update Range", 50.0D);
		networkUpdateRange.comment = "This is the distance in which players will be notified.  Lower if you experience lag.";
		
		debugMode = localConfig.get("general", "Enable Debugging", false);
		debugMode.comment = "Enable advanced debugging, should ALWAYS be false.";
		
		localConfig.save();
	}
}