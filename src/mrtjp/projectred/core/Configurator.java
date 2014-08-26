package mrtjp.projectred.core;

import cpw.mods.fml.client.event.ConfigChangedEvent;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import net.minecraftforge.common.config.Configuration;

import java.io.File;

public class Configurator
{
    protected static File _configFolder;
    protected static File _configFile;
    public static Configuration config;

    /** Constants **/
    public static final String modName = "Project Red";
    public static final String version = "@VERSION@";
    public static final String buildnumber = "@BUILD_NUMBER@";

    /** Generation **/
    public static boolean gen_MarbleCave;
    public static int gen_MarbleCave_resistance;
    public static boolean gen_Volcano;
    public static int gen_Volcano_resistance;
    public static boolean gen_Ruby;
    public static int gen_Ruby_resistance;
    public static boolean gen_Sapphire;
    public static int gen_Sapphire_resistance;
    public static boolean gen_Peridot;
    public static int gen_Peridot_resistance;
    public static boolean gen_SpreadingMoss;

    /** Settings **/
    public static boolean versionChecking;
    public static boolean versionCheckDevBuilds;

    public static boolean debugMode;
    public static boolean logicGateSounds;

    public static boolean simpleFramedWireRecipe;

    public static int maxDetectionCount;
    public static int detectionFrequency;
    public static int routerUpdateThreadCount;

    /** Retro Generation **/
    public static boolean retroGeneration;
    public static String retroGenID;

    /** Render **/
    public static boolean logicwires3D;
    public static boolean staticWires;
    public static boolean staticGates;
    public static int lightHaloMax;

    public static void initConfig(FMLPreInitializationEvent event)
    {
        _configFolder = event.getModConfigurationDirectory();
        _configFile = new File(_configFolder.getAbsolutePath() + "/ProjectRed.cfg");
        config = new Configuration(_configFile);
        updateConfig();
    }

    public static void updateConfig()
    {
        config.addCustomCategoryComment("World Generation", "Toggle generation of structures, or increase resistance to lessen chances for generation.");
        gen_Ruby = config.get("World Generation", "Ruby Ore", true).getBoolean(true);
        gen_Ruby_resistance = config.get("World Generation", "Ruby Ore resistance", 0).getInt();
        gen_Sapphire = config.get("World Generation", "Sapphire Ore", true).getBoolean(true);
        gen_Sapphire_resistance = config.get("World Generation", "Sapphire Ore resistance", 0).getInt();
        gen_Peridot = config.get("World Generation", "Peridot Ore", true).getBoolean(true);
        gen_Peridot_resistance = config.get("World Generation", "Peridot Ore resistance", 0).getInt();
        gen_MarbleCave = config.get("World Generation", "Marble Caves", true).getBoolean(true);
        gen_MarbleCave_resistance = config.get("World Generation", "Marble Caves resistance", 4).getInt();
        gen_Volcano = config.get("World Generation", "Volcanos", true).getBoolean(true);
        gen_Volcano_resistance = config.get("World Generation", "Volcano resistance", 16).getInt();
        gen_SpreadingMoss = config.get("World Generation", "Spreading Moss", true).getBoolean(true);

        versionChecking = config.get("general", "Enable Version checking", true, "If enabled, the player will be alerted if a newer version of PR is available").getBoolean(true);
        versionCheckDevBuilds = config.get("general", "Dev Version checking", false, "Include dev builds in version checks").getBoolean(false);

        debugMode = config.get("general", "Enable Debugging", false, "Enable advanced debugging, should ALWAYS be false.").getBoolean(false);
        logicGateSounds = config.get("general", "Logic Sounds", true, "If set to false, logic gates will not make sounds.").getBoolean(true);
        logicwires3D = config.get("general", "3Dlogicwires", true, "If set to false, flat wire textures will be used for logic gates. Significant performance improvement").getBoolean(true);
        staticWires = config.get("general", "renderStaticWires", true, "If set to false, wires will be rendered in the TESR rather than the WorldRenderer").getBoolean(true);
        staticGates = config.get("general", "renderStaticGates", true, "If set to false, gates will be rendered in the TESR rather than the WorldRenderer").getBoolean(true);
        retroGeneration = config.get("general", "Retro Ore Generation", false, "If set to true, world generation for ProjectRed will attempt to run even in previously generated chunks.").getBoolean(false);
        retroGenID = config.get("general", "Retro Ore Gen ID", "prRG", "This ID is used to check if retro gen has been performed on a chunk. Changing it will reset retro gen status.").getString();
        lightHaloMax = config.get("general", "Light Halo Render Count", -1, "Number of lights to render, -1 for unlimited").getInt();

        maxDetectionCount = config.get("general", "Max Detection Count", 100, "Max number of links to explore when discovering new routers.").getInt();
        detectionFrequency = config.get("general", "Detection Frequency", 20, "Ticks between router searches.").getInt();
        routerUpdateThreadCount = config.get("general", "Router Update Thread Count", 4, "Number of active threads that update routing tables.").getInt();

        simpleFramedWireRecipe = config.get("general", "Simple Framed Wire recipe", false, "Use sticks instead of wood strips in framed wire recipes.").getBoolean();

        if (config.hasChanged())
            config.save();
    }

    @SubscribeEvent
    public void onConfigChanged(ConfigChangedEvent.OnConfigChangedEvent event)
    {
        if ("ProjRed|Core".equals(event.modID))
            updateConfig();
    }
}