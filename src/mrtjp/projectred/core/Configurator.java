package mrtjp.projectred.core;

import cpw.mods.fml.common.event.FMLPreInitializationEvent;
import net.minecraftforge.common.config.Configuration;

import java.io.File;

public class Configurator
{
    protected static File _configFolder;
    protected static File _configFile;

    /** Constants **/
    public static final String modName = "Project Red";
    public static final String version = "@VERSION@";
    public static final String buildnumber = "@BUILD_NUMBER@";

    /** Generation **/
    public static boolean gen_MarbleCave;
    public static boolean gen_Volcano;
    public static boolean gen_Ruby;
    public static boolean gen_Sapphire;
    public static boolean gen_Peridot;
    public static boolean gen_SpreadingMoss;
    public static boolean gen_dyeTrees;

    /** Settings **/
    public static boolean versionChecking;
    public static boolean versionCheckDevBuilds;

    public static boolean debugMode;
    public static boolean logicGateSounds;

    public static int[] backpackBlacklist;

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
        loadPropertiesFromFile(_configFile);
    }

    public static void loadPropertiesFromFile(File file)
    {
        Configuration localConfig = new Configuration(file);
        localConfig.load();

        gen_Ruby = localConfig.get("World Generation", "Ruby Ore", true).getBoolean(true);
        gen_Sapphire = localConfig.get("World Generation", "Sapphire Ore", true).getBoolean(true);
        gen_Peridot = localConfig.get("World Generation", "Peridot Ore", true).getBoolean(true);
        gen_MarbleCave = localConfig.get("World Generation", "Marble Caves", true).getBoolean(true);
        gen_Volcano = localConfig.get("World Generation", "Volcanos", true).getBoolean(true);
        gen_SpreadingMoss = localConfig.get("World Generation", "Spreading Moss", true).getBoolean(true);
        gen_dyeTrees = localConfig.get("World Generation", "Stained Trees", true).getBoolean(true);

        versionChecking = localConfig.get("general", "Enable Version checking", true, "If enabled, the player will be alerted if a newer version of PR is available").getBoolean(true);
        versionCheckDevBuilds = localConfig.get("general", "Dev Version checking", false, "Include dev builds in version checks").getBoolean(false);

        debugMode = localConfig.get("general", "Enable Debugging", false, "Enable advanced debugging, should ALWAYS be false.").getBoolean(false);
        logicGateSounds = localConfig.get("general", "Logic Sounds", true, "If set to false, logic gates will not make sounds.").getBoolean(true);
        logicwires3D = localConfig.get("general", "3Dlogicwires", true, "If set to false, flat wire textures will be used for logic gates. Significant performance improvement").getBoolean(true);
        staticWires = localConfig.get("general", "renderStaticWires", true, "If set to false, wires will be rendered in the TESR rather than the WorldRenderer").getBoolean(true);
        staticGates = localConfig.get("general", "renderStaticGates", true, "If set to false, gates will be rendered in the TESR rather than the WorldRenderer").getBoolean(true);
        retroGeneration = localConfig.get("general", "Retro Ore Generation", false, "If set to true, world generation for ProjectRed will attempt to run even in previously generated chunks.").getBoolean(false);
        retroGenID = localConfig.get("general", "Retro Ore Gen ID", "prRG", "This ID is used to check if retro gen has been performed on a chunk. Changing it will reset retro gen status.").getString();
        lightHaloMax = localConfig.get("general", "Light Halo Render Count", -1, "Number of lights to render, -1 for unlimited").getInt();

        maxDetectionCount = localConfig.get("general", "Max Detection Count", 100, "Max number of links to explore when discovering new routers.").getInt();
        detectionFrequency = localConfig.get("general", "Detection Frequency", 20, "Ticks between router searches.").getInt();
        routerUpdateThreadCount = localConfig.get("general", "Router Update Thread Count", 4, "Number of active threads that update routing tables.").getInt();

        String[] s = localConfig.get("general", "Backpack item blacklist", "", "Comma seperated list of item ids that cannot go in a backpack.").getString().split(",");
        int s1 = 0;
        backpackBlacklist = new int[s.length];
        for (String str : s)
        {
            String str2 = str.trim();
            try
            {
                backpackBlacklist[s1] = Integer.parseInt(str2);
            }
            catch (Throwable t){}
            s1++;
        }

        localConfig.save();
    }
}