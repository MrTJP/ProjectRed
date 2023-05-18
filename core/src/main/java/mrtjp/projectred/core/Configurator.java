package mrtjp.projectred.core;

import codechicken.lib.config.ConfigCategory;
import codechicken.lib.config.ConfigFile;

import java.nio.file.Paths;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class Configurator {

    /* Gates */
    public static boolean  logicGateSounds = true;
    public static boolean  logicGateLights = true;
    public static int  minTimerTicks = 4;
    public static boolean  unbreakableScrewdriver = false;

    /* Machines */
    public static boolean enableDiamondBlockBreaker = false;

    /* Rendering */
    public static boolean logicwires3D = true;
    public static boolean staticWires = true;
    public static boolean staticGates = true;
    public static int lightHaloMax = -1;

    /* World Gen */
    public static boolean gen_MarbleCave = true;
    public static boolean gen_Ruby = true;
    public static int gen_RubyVeinSize = 8;
    public static boolean gen_Sapphire = true;
    public static int gen_SapphireVeinSize = 8;
    public static boolean gen_Peridot = true;
    public static int gen_PeridotVeinSize = 10;
    public static boolean gen_Copper = true;
    public static int gen_CopperVeinSize = 8;
    public static boolean gen_Tin = true;
    public static int gen_TinVeinSize = 8;
    public static boolean gen_Silver = true;
    public static int gen_SilverVeinSize = 9;
    public static boolean gen_Electrotine = true;
    public static int gen_ElectrotineVeinSize = 8;

    /* Compatibility */
    public static boolean compat_CCBundledCable = true;

    public static void load() {
        ConfigFile configFile = new ConfigFile(MOD_ID).path(Paths.get("config/ProjectRed.cfg"));
        ConfigCategory config = configFile.load();
        loadValues(config);
        config.save();
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

        ConfigCategory rendering = config.getCategory("rendering").setComment("Client render settings");
        logicwires3D = rendering.getValue("gate_3d_wires").setDefaultBoolean(logicwires3D).setComment("If set to false, flat wire textures will be used for logic gates. Significant performance improvement").getBoolean();
        staticWires = rendering.getValue("static_wire_renderer").setDefaultBoolean(staticWires).setComment("If set to false, wires will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        staticGates = rendering.getValue("static_gate_renderer").setDefaultBoolean(staticGates).setComment("If set to false, gates will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        lightHaloMax = rendering.getValue("max_light_halo_count").setDefaultInt(lightHaloMax).setComment("Number of lights to render, -1 for unlimited").getInt();

        ConfigCategory gen = config.getCategory("world_gen").setComment("World gen settings");
        gen_Ruby                = gen.getValue("ruby_ore").setDefaultBoolean(gen_Ruby).setComment("Enable Ruby Ore generation").getBoolean();
        gen_RubyVeinSize        = gen.getValue("ruby_ore_vein_size").setDefaultInt(gen_RubyVeinSize).setComment("Ruby Ore vein size").getInt();
        gen_Sapphire            = gen.getValue("sapphire_ore").setDefaultBoolean(gen_Sapphire).setComment("Enable Sapphire Ore generation").getBoolean();
        gen_SapphireVeinSize    = gen.getValue("sapphire_ore_vein_size").setDefaultInt(gen_SapphireVeinSize).setComment("Sapphire Ore vein size").getInt();
        gen_Peridot             = gen.getValue("peridot_ore").setDefaultBoolean(gen_Peridot).setComment("Enable Peridot Ore generation").getBoolean();
        gen_PeridotVeinSize     = gen.getValue("peridot_ore_vein_size").setDefaultInt(gen_PeridotVeinSize).setComment("Peridot Ore vein size").getInt();
        gen_Copper              = gen.getValue("copper_ore").setDefaultBoolean(gen_Copper).setComment("Enable Copper Ore generation").getBoolean();
        gen_CopperVeinSize      = gen.getValue("copper_ore_vein_size").setDefaultInt(gen_CopperVeinSize).setComment("Copper Ore vein size").getInt();
        gen_Tin                 = gen.getValue("tin_ore").setDefaultBoolean(gen_Tin).setComment("Enable Tin Ore generation").getBoolean();
        gen_TinVeinSize         = gen.getValue("tin_ore_vein_size").setDefaultInt(gen_TinVeinSize).setComment("Tin Ore vein size").getInt();
        gen_Silver              = gen.getValue("silver_ore").setDefaultBoolean(gen_Silver).setComment("Enable Silver Ore generation").getBoolean();
        gen_SilverVeinSize      = gen.getValue("silver_ore_vein_size").setDefaultInt(gen_SilverVeinSize).setComment("Silver Ore vein size").getInt();
        gen_Electrotine         = gen.getValue("electrotine_ore").setDefaultBoolean(gen_Electrotine).setComment("Enable Electrotine Ore generation").getBoolean();
        gen_ElectrotineVeinSize = gen.getValue("electrotine_ore_vein_size").setDefaultInt(gen_ElectrotineVeinSize).setComment("Electrotine Ore vein size").getInt();
        gen_MarbleCave          = gen.getValue("marble_cave").setDefaultBoolean(gen_MarbleCave).setComment("Enable Marble Cave generation").getBoolean();

        ConfigCategory compat = config.getCategory("compatibility").setComment("Control the loading of various compatibility hooks. These settings are ignored unless the Compatibility module is installed.");
        compat_CCBundledCable = compat.getValue("computercraft").setDefaultBoolean(compat_CCBundledCable).setComment("This allows computers to connect to bundled cables with the RS API.").getBoolean();
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
        gen_RubyVeinSize        = gen.getValue("Ruby Ore vein size").setDefaultInt(gen_RubyVeinSize).getInt();
        gen_Sapphire            = gen.getValue("Sapphire Ore").setDefaultBoolean(gen_Sapphire).getBoolean();
        gen_SapphireVeinSize    = gen.getValue("Sapphire Ore vein size").setDefaultInt(gen_SapphireVeinSize).getInt();
        gen_Peridot             = gen.getValue("Peridot Ore").setDefaultBoolean(gen_Peridot).getBoolean();
        gen_PeridotVeinSize     = gen.getValue("Peridot Ore vein size").setDefaultInt(gen_PeridotVeinSize).getInt();
        gen_Copper              = gen.getValue("Copper Ore").setDefaultBoolean(gen_Copper).getBoolean();
        gen_CopperVeinSize      = gen.getValue("Copper Ore vein size").setDefaultInt(gen_CopperVeinSize).getInt();
        gen_Tin                 = gen.getValue("Tin Ore").setDefaultBoolean(gen_Tin).getBoolean();
        gen_TinVeinSize         = gen.getValue("Tin Ore vein size").setDefaultInt(gen_TinVeinSize).getInt();
        gen_Silver              = gen.getValue("Silver Ore").setDefaultBoolean(gen_Silver).getBoolean();
        gen_SilverVeinSize      = gen.getValue("Silver Ore vein size").setDefaultInt(gen_SilverVeinSize).getInt();
        gen_Electrotine         = gen.getValue("Electrotine Ore").setDefaultBoolean(gen_Electrotine).getBoolean();
        gen_ElectrotineVeinSize = gen.getValue("Electrotine Ore vein size").setDefaultInt(gen_ElectrotineVeinSize).getInt();
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
