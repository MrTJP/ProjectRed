package mrtjp.projectred.core;

import codechicken.lib.config.ConfigCategory;
import codechicken.lib.config.ConfigFile;

import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class Configurator {

    /* Gates */
    public static boolean  logicGateSounds = true;
    public static boolean  logicGateLights = true;
    public static int  minTimerTicks = 4;
    public static boolean  unbreakableScrewdriver = false;

    /* Machines */
    public static boolean enableDiamondBlockBreaker = false;
    public static int frameMoveLimit = 1024;

    /* Rendering */
    public static boolean logicwires3D = true;
    public static boolean staticWires = true;
    public static boolean staticGates = true;
    public static int lightHaloMax = -1;
    public static boolean fabulousLights = true;

    /* World Gen */
    public static String rubyOreKey = "ruby_ore";
    public static String sapphireOreKey = "sapphire_ore";
    public static String peridotOreKey = "peridot_ore";
    public static String tinOreKey = "tin_ore";
    public static String silverOreKey = "silver_ore";
    public static String electrotineOreKey = "electrotine_ore";
    public static String marbleCaveKey = "marble_cave";
    public static boolean gen_MarbleCave = true;
    public static boolean gen_Ruby = true;
    public static boolean gen_Sapphire = true;
    public static boolean gen_Peridot = true;
    public static boolean gen_Tin = true;
    public static boolean gen_Silver = true;
    public static boolean gen_Electrotine = true;
    public static Map<String, Boolean> worldFeatures = new HashMap<>();

    /* Compatibility */
    public static boolean compat_CCBundledCable = true;

    /* Fabrication */
    public static int autoCompileTileLimit = 20;

    public static void load() {
        ConfigFile configFile = new ConfigFile(MOD_ID).path(Paths.get("config/ProjectRed.cfg"));
        ConfigCategory config = configFile.load();
        loadValues(config);
        config.save();
    }

    private static boolean loadAndStoreFeature(ConfigCategory gen, String key, boolean def) {
        boolean value = gen.getValue(key).setDefaultBoolean(def).getBoolean();
        worldFeatures.put(key, value);
        return value;
    }

    private static void loadValues(ConfigCategory config) {

        //TODO remove later
        loadAndDeleteLegacyValues(config);

        ConfigCategory general = config.getCategory("general").setComment("General settings");
        logicGateSounds = general.getValue("gate_sounds").setDefaultBoolean(logicGateSounds).setComment("If set to false, logic gates will not make sounds").getBoolean();
        logicGateLights = general.getValue("gate_lights").setDefaultBoolean(logicGateLights).setComment("If set to false, logic gates will not emit light").getBoolean();
        minTimerTicks = general.getValue("gate_min_timer_ticks").setDefaultInt(minTimerTicks).setComment("Minimum amount of ticks the timer gates can be set to (min 4)").getInt();
        unbreakableScrewdriver = general.getValue("infinite_screwdriver").setDefaultBoolean(unbreakableScrewdriver).setComment("If set to true, the basic screwdriver will not take damage").getBoolean();

        ConfigCategory machines = config.getCategory("machines").setComment("Settings related to machines and devices");
        enableDiamondBlockBreaker = machines.getValue("diamond_block_breaker").setDefaultBoolean(enableDiamondBlockBreaker).setComment("Allow the Diamond Block Breaker to be crafted").getBoolean();
        frameMoveLimit = machines.getValue("frame_move_limit").setDefaultInt(frameMoveLimit).setComment("Max blocks in a moving frame structure").getInt();

        ConfigCategory rendering = config.getCategory("rendering").setComment("Client render settings");
        logicwires3D = rendering.getValue("gate_3d_wires").setDefaultBoolean(logicwires3D).setComment("If set to false, flat wire textures will be used for logic gates. Significant performance improvement").getBoolean();
        staticWires = rendering.getValue("static_wire_renderer").setDefaultBoolean(staticWires).setComment("If set to false, wires will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        staticGates = rendering.getValue("static_gate_renderer").setDefaultBoolean(staticGates).setComment("If set to false, gates will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        lightHaloMax = rendering.getValue("max_lights").setDefaultInt(lightHaloMax).setComment("Max lights on screen at a time, -1 for unlimited").getInt();
        fabulousLights = rendering.getValue("fabulous_lights").setDefaultBoolean(fabulousLights).setComment("Use fabulous shader pipeline for lights when on Fabulous Graphics mode").getBoolean();

        ConfigCategory gen = config.getCategory("world_gen").setComment("World gen settings");
        gen_Ruby = loadAndStoreFeature(gen, rubyOreKey, true);
        gen_Sapphire = loadAndStoreFeature(gen, sapphireOreKey, true);
        gen_Peridot = loadAndStoreFeature(gen, peridotOreKey, true);
        gen_Tin = loadAndStoreFeature(gen, tinOreKey, true);
        gen_Silver = loadAndStoreFeature(gen, silverOreKey, true);
        gen_Electrotine = loadAndStoreFeature(gen, electrotineOreKey, true);
        gen_MarbleCave = loadAndStoreFeature(gen, marbleCaveKey, true);

        ConfigCategory compat = config.getCategory("compatibility").setComment("Control the loading of various compatibility hooks. These settings are ignored unless the Compatibility module is installed.");
        compat_CCBundledCable = compat.getValue("computercraft").setDefaultBoolean(compat_CCBundledCable).setComment("This allows computers to connect to bundled cables with the RS API.").getBoolean();

        ConfigCategory fab = config.getCategory("fabrication").setComment("Settings for Fabrication circuit compilation");
        autoCompileTileLimit = fab.getValue("auto_compile_tile_limit").setDefaultInt(autoCompileTileLimit).setComment("Tile count before auto-compile becomes disallowed (-1 to always allow, 0 to never allow). Recommended to keep this very low on servers.").getInt();
    }

    private static void loadAndDeleteLegacyValues(ConfigCategory config) {

        if (!config.has("General Settings")) {
            return;
        }

        ProjectRedCore.LOGGER.warn("Legacy ProjectRed config file detected. Remapping values...");

        ConfigCategory general = config.getCategory("General Settings");
        logicGateSounds = general.getValue("Logic Sounds").setDefaultBoolean(logicGateSounds).getBoolean();
        logicGateLights = general.getValue("Logic Gate Lights").setDefaultBoolean(logicGateLights).getBoolean();
        minTimerTicks = general.getValue("Minimum Timer Ticks").setDefaultInt(minTimerTicks).getInt();
        unbreakableScrewdriver = general.getValue("Unbreakable Screwdriver").setDefaultBoolean(unbreakableScrewdriver).getBoolean();

        ConfigCategory machines = config.getCategory("Machine Settings");
        enableDiamondBlockBreaker = machines.getValue("Enable the Diamond Block Breaker").setDefaultBoolean(enableDiamondBlockBreaker).getBoolean();

        ConfigCategory rendering = config.getCategory("Render Settings").setComment("Client render settings");
        logicwires3D = rendering.getValue("3D Logic Wires").setDefaultBoolean(logicwires3D).getBoolean();
        staticWires = rendering.getValue("Static Wires").setDefaultBoolean(staticWires).getBoolean();
        staticGates = rendering.getValue("Static Gates").setDefaultBoolean(staticGates).getBoolean();
        lightHaloMax = rendering.getValue("Light Halo Render Count").setDefaultInt(lightHaloMax).getInt();

        ConfigCategory gen = config.getCategory("World Gen");
        gen_Ruby                = gen.getValue("Ruby Ore").setDefaultBoolean(gen_Ruby).getBoolean();
        gen_Sapphire            = gen.getValue("Sapphire Ore").setDefaultBoolean(gen_Sapphire).getBoolean();
        gen_Peridot             = gen.getValue("Peridot Ore").setDefaultBoolean(gen_Peridot).getBoolean();
        gen_Tin                 = gen.getValue("Tin Ore").setDefaultBoolean(gen_Tin).getBoolean();
        gen_Silver              = gen.getValue("Silver Ore").setDefaultBoolean(gen_Silver).getBoolean();
        gen_Electrotine         = gen.getValue("Electrotine Ore").setDefaultBoolean(gen_Electrotine).getBoolean();
        gen_MarbleCave          = gen.getValue("Marble Caves").setDefaultBoolean(gen_MarbleCave).getBoolean();

        ConfigCategory compat = config.getCategory("Compatibility");
        compat_CCBundledCable = compat.getValue("ComputerCraft: Bundled Cables").setDefaultBoolean(compat_CCBundledCable).getBoolean();

        // Delete all old categories
        config.delete("General Settings");
        config.delete("Machine Settings");
        config.delete("Render Settings");
        config.delete("World Gen");
        config.delete("Compatibility");
    }
}
