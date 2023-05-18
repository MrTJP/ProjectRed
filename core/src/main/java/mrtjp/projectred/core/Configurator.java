package mrtjp.projectred.core;

import codechicken.lib.config.ConfigTag;
import codechicken.lib.config.StandardConfigFile;

import java.nio.file.Paths;

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
        StandardConfigFile configFile = new StandardConfigFile(Paths.get("./config/ProjectRed.cfg"));
        ConfigTag config = configFile.load();
        loadValues(config);
        config.save();
    }

    private static void loadValues(ConfigTag config) {

        //TODO remove later
        loadAndDeleteLegacyValues(config);

        ConfigTag general = config.getTag("general").setComment("General settings");
        logicGateSounds = general.getTag("gate_sounds").setDefaultBoolean(logicGateSounds).setComment("If set to false, logic gates will not make sounds").getBoolean();
        logicGateLights = general.getTag("gate_lights").setDefaultBoolean(logicGateLights).setComment("If set to false, logic gates will not emit light").getBoolean();
        minTimerTicks = general.getTag("gate_min_timer_ticks").setDefaultInt(minTimerTicks).setComment("Minimum amount of ticks the timer gates can be set to (min 4)").getInt();
        unbreakableScrewdriver = general.getTag("infinite_screwdriver").setDefaultBoolean(unbreakableScrewdriver).setComment("If set to true, the basic screwdriver will not take damage").getBoolean();

        ConfigTag machines = config.getTag("machines").setComment("Settings related to machines and devices");
        enableDiamondBlockBreaker = machines.getTag("diamond_block_breaker").setDefaultBoolean(enableDiamondBlockBreaker).setComment("Allow the Diamond Block Breaker to be crafted").getBoolean();

        ConfigTag rendering = config.getTag("rendering").setComment("Client render settings");
        logicwires3D = rendering.getTag("gate_3d_wires").setDefaultBoolean(logicwires3D).setComment("If set to false, flat wire textures will be used for logic gates. Significant performance improvement").getBoolean();
        staticWires = rendering.getTag("static_wire_renderer").setDefaultBoolean(staticWires).setComment("If set to false, wires will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        staticGates = rendering.getTag("static_gate_renderer").setDefaultBoolean(staticGates).setComment("If set to false, gates will be rendered in the TESR rather than the WorldRenderer").getBoolean();
        lightHaloMax = rendering.getTag("max_light_halo_count").setDefaultInt(lightHaloMax).setComment("Number of lights to render, -1 for unlimited").getInt();

        ConfigTag gen = config.getTag("world_gen").setComment("World gen settings");
        gen_Ruby                = gen.getTag("ruby_ore").setDefaultBoolean(gen_Ruby).setComment("Enable Ruby Ore generation").getBoolean();
        gen_RubyVeinSize        = gen.getTag("ruby_ore_vein_size").setDefaultInt(gen_RubyVeinSize).setComment("Ruby Ore vein size").getInt();
        gen_Sapphire            = gen.getTag("sapphire_ore").setDefaultBoolean(gen_Sapphire).setComment("Enable Sapphire Ore generation").getBoolean();
        gen_SapphireVeinSize    = gen.getTag("sapphire_ore_vein_size").setDefaultInt(gen_SapphireVeinSize).setComment("Sapphire Ore vein size").getInt();
        gen_Peridot             = gen.getTag("peridot_ore").setDefaultBoolean(gen_Peridot).setComment("Enable Peridot Ore generation").getBoolean();
        gen_PeridotVeinSize     = gen.getTag("peridot_ore_vein_size").setDefaultInt(gen_PeridotVeinSize).setComment("Peridot Ore vein size").getInt();
        gen_Copper              = gen.getTag("copper_ore").setDefaultBoolean(gen_Copper).setComment("Enable Copper Ore generation").getBoolean();
        gen_CopperVeinSize      = gen.getTag("copper_ore_vein_size").setDefaultInt(gen_CopperVeinSize).setComment("Copper Ore vein size").getInt();
        gen_Tin                 = gen.getTag("tin_ore").setDefaultBoolean(gen_Tin).setComment("Enable Tin Ore generation").getBoolean();
        gen_TinVeinSize         = gen.getTag("tin_ore_vein_size").setDefaultInt(gen_TinVeinSize).setComment("Tin Ore vein size").getInt();
        gen_Silver              = gen.getTag("silver_ore").setDefaultBoolean(gen_Silver).setComment("Enable Silver Ore generation").getBoolean();
        gen_SilverVeinSize      = gen.getTag("silver_ore_vein_size").setDefaultInt(gen_SilverVeinSize).setComment("Silver Ore vein size").getInt();
        gen_Electrotine         = gen.getTag("electrotine_ore").setDefaultBoolean(gen_Electrotine).setComment("Enable Electrotine Ore generation").getBoolean();
        gen_ElectrotineVeinSize = gen.getTag("electrotine_ore_vein_size").setDefaultInt(gen_ElectrotineVeinSize).setComment("Electrotine Ore vein size").getInt();
        gen_MarbleCave          = gen.getTag("marble_cave").setDefaultBoolean(gen_MarbleCave).setComment("Enable Marble Cave generation").getBoolean();

        ConfigTag compat = config.getTag("compatibility").setComment("Control the loading of various compatibility hooks. These settings are ignored unless the Compatibility module is installed.");
        compat_CCBundledCable = compat.getTag("computercraft").setDefaultBoolean(compat_CCBundledCable).setComment("This allows computers to connect to bundled cables with the RS API.").getBoolean();
    }

    private static void loadAndDeleteLegacyValues(ConfigTag config) {

        if (!config.hasTag("General Settings")) {
            return;
        }

        ProjectRedCore.LOGGER.warn("Legacy ProjectRed config file detected. Remapping values...");

        ConfigTag general = config.getTag("General Settings");
        logicGateSounds = general.getTag("Logic Sounds").setDefaultBoolean(logicGateSounds).getBoolean();
        logicGateLights = general.getTag("Logic Gate Lights").setDefaultBoolean(logicGateLights).getBoolean();
        minTimerTicks = general.getTag("Minimum Timer Ticks").setDefaultInt(minTimerTicks).getInt();
        unbreakableScrewdriver = general.getTag("Unbreakable Screwdriver").setDefaultBoolean(unbreakableScrewdriver).getBoolean();

        ConfigTag machines = config.getTag("Machine Settings");
        enableDiamondBlockBreaker = machines.getTag("Enable the Diamond Block Breaker").setDefaultBoolean(enableDiamondBlockBreaker).getBoolean();

        ConfigTag rendering = config.getTag("Render Settings").setComment("Client render settings");
        logicwires3D = rendering.getTag("3D Logic Wires").setDefaultBoolean(logicwires3D).getBoolean();
        staticWires = rendering.getTag("Static Wires").setDefaultBoolean(staticWires).getBoolean();
        staticGates = rendering.getTag("Static Gates").setDefaultBoolean(staticGates).getBoolean();
        lightHaloMax = rendering.getTag("Light Halo Render Count").setDefaultInt(lightHaloMax).getInt();

        ConfigTag gen = config.getTag("World Gen");
        gen_Ruby                = gen.getTag("Ruby Ore").setDefaultBoolean(gen_Ruby).getBoolean();
        gen_RubyVeinSize        = gen.getTag("Ruby Ore vein size").setDefaultInt(gen_RubyVeinSize).getInt();
        gen_Sapphire            = gen.getTag("Sapphire Ore").setDefaultBoolean(gen_Sapphire).getBoolean();
        gen_SapphireVeinSize    = gen.getTag("Sapphire Ore vein size").setDefaultInt(gen_SapphireVeinSize).getInt();
        gen_Peridot             = gen.getTag("Peridot Ore").setDefaultBoolean(gen_Peridot).getBoolean();
        gen_PeridotVeinSize     = gen.getTag("Peridot Ore vein size").setDefaultInt(gen_PeridotVeinSize).getInt();
        gen_Copper              = gen.getTag("Copper Ore").setDefaultBoolean(gen_Copper).getBoolean();
        gen_CopperVeinSize      = gen.getTag("Copper Ore vein size").setDefaultInt(gen_CopperVeinSize).getInt();
        gen_Tin                 = gen.getTag("Tin Ore").setDefaultBoolean(gen_Tin).getBoolean();
        gen_TinVeinSize         = gen.getTag("Tin Ore vein size").setDefaultInt(gen_TinVeinSize).getInt();
        gen_Silver              = gen.getTag("Silver Ore").setDefaultBoolean(gen_Silver).getBoolean();
        gen_SilverVeinSize      = gen.getTag("Silver Ore vein size").setDefaultInt(gen_SilverVeinSize).getInt();
        gen_Electrotine         = gen.getTag("Electrotine Ore").setDefaultBoolean(gen_Electrotine).getBoolean();
        gen_ElectrotineVeinSize = gen.getTag("Electrotine Ore vein size").setDefaultInt(gen_ElectrotineVeinSize).getInt();
        gen_MarbleCave          = gen.getTag("Marble Caves").setDefaultBoolean(gen_MarbleCave).getBoolean();

        ConfigTag compat = config.getTag("Compatibility");
        compat_CCBundledCable = compat.getTag("ComputerCraft: Bundled Cables").setDefaultBoolean(compat_CCBundledCable).getBoolean();

        // Delete all old categories
        config.deleteTag("General Settings");
        config.deleteTag("Machine Settings");
        config.deleteTag("Render Settings");
        config.deleteTag("World Gen");
        config.deleteTag("Compatibility");
    }
}
