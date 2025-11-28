package mrtjp.projectred.core;

import codechicken.lib.config.ConfigCategory;
import codechicken.lib.config.ConfigFile;
import codechicken.lib.config.ConfigValue;
import net.neoforged.bus.api.SubscribeEvent;
import net.neoforged.fml.event.config.ModConfigEvent;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.neoforged.neoforge.common.ModConfigSpec.BooleanValue;
import net.neoforged.neoforge.common.ModConfigSpec.IntValue;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import static mrtjp.projectred.core.ProjectRedCore.MOD_ID;

public class Configurator {

    private static final Logger LOGGER = LogManager.getLogger();

    // Public spec for registration
    static final ModConfigSpec serverSpec;
    public static final Server SERVER;

    static final ModConfigSpec clientSpec;
    public static final Client CLIENT;

    static {
        final Pair<Server, ModConfigSpec> specPair = new ModConfigSpec.Builder().configure(Server::new);
        serverSpec = specPair.getRight();
        SERVER = specPair.getLeft();

        final Pair<Client, ModConfigSpec> clientSpecPair = new ModConfigSpec.Builder().configure(Client::new);
        clientSpec = clientSpecPair.getRight();
        CLIENT = clientSpecPair.getLeft();
    }

    /**
     * Common configuration - synchronized and available on both sides
     */
    public static class Server {

        // World Gen feature keys
        public static final String RUBY_ORE_KEY = "ruby_ore";
        public static final String SAPPHIRE_ORE_KEY = "sapphire_ore";
        public static final String PERIDOT_ORE_KEY = "peridot_ore";
        public static final String TIN_ORE_KEY = "tin_ore";
        public static final String SILVER_ORE_KEY = "silver_ore";
        public static final String ELECTROTINE_ORE_KEY = "electrotine_ore";
        public static final String MARBLE_CAVE_KEY = "marble_cave";

        // General
        public final BooleanValue logicGateSounds;
        public final BooleanValue unbreakableScrewdriver;

        // Performance
        public final BooleanValue logicGateLights;
        public final IntValue minTimerTicks;
        public final IntValue frameMoveLimit;
        public final IntValue autoCompileTileLimit;

        // World Gen category - feature toggles
        public final BooleanValue generateRubyOres;
        public final BooleanValue generateSapphireOres;
        public final BooleanValue generatePeridotOres;
        public final BooleanValue generateTinOres;
        public final BooleanValue generateSilverOres;
        public final BooleanValue generateElectrotineOres;
        public final BooleanValue generateMarbleCaves;

        // World features map (internal)
        private final Map<String, BooleanValue> worldFeatures = new HashMap<>();

        private Server(ModConfigSpec.Builder builder) {

            // Gameplay settings
            builder.comment("Settings that effect gameplay, balance, progression, etc.")
                    .push("gameplay");

            unbreakableScrewdriver = builder
                    .comment("If set to true, the basic screwdriver will not take damage")
                    .define("infinite_screwdriver", false);

            logicGateSounds = builder
                    .comment("If set to false, logic gates will not make sounds")
                    .define("gate_sounds", true);

            builder.pop();

            // Performance settings
            builder.comment("Settings that effect performance")
                    .push("performance");

            logicGateLights = builder
                    .comment("If set to false, logic gates will not emit light. Can help reduce light updates "
                            + "on particularly large and fast-updating redstone circuits.")
                    .define("gate_lights", true);

            minTimerTicks = builder
                    .comment("Minimum amount of ticks the timer gates can be set to (min 4). Can be used to enforce "
                            + "lower update rates.")
                    .defineInRange("gate_min_timer_ticks", 4, 4, Integer.MAX_VALUE);

            frameMoveLimit = builder
                    .comment("Max blocks in a moving frame structure. Limiting this can improve performance on servers where "
                            + "lots of structures are being moved.")
                    .defineInRange("frame_move_limit", 1024, 1, 4096);

            autoCompileTileLimit = builder
                    .comment("Max number of tiles allowed in IC Workbench before auto-compile becomes disallowed (-1 to always "
                            + "allow, 0 to never allow). Recommended to keep this very low on servers.")
                    .defineInRange("auto_compile_tile_limit", 20, -1, Integer.MAX_VALUE);

            builder.pop();

            // World generation settings
            builder.comment("World generation settings")
                    .push("world_gen");

            generateRubyOres = defineAndStoreFeature(builder, RUBY_ORE_KEY, true);
            generateSapphireOres = defineAndStoreFeature(builder, SAPPHIRE_ORE_KEY, true);
            generatePeridotOres = defineAndStoreFeature(builder, PERIDOT_ORE_KEY, true);
            generateTinOres = defineAndStoreFeature(builder, TIN_ORE_KEY, true);
            generateSilverOres = defineAndStoreFeature(builder, SILVER_ORE_KEY, true);
            generateElectrotineOres = defineAndStoreFeature(builder, ELECTROTINE_ORE_KEY, true);
            generateMarbleCaves = defineAndStoreFeature(builder, MARBLE_CAVE_KEY, true);

            builder.pop();
        }

        private BooleanValue defineAndStoreFeature(ModConfigSpec.Builder builder, String key, boolean defaultValue) {
            BooleanValue value = builder.define(key, defaultValue);
            worldFeatures.put(key, value);
            return value;
        }

        /**
         * Check if a world feature is enabled by its key
         * @param key The feature key from biome modifier JSON
         * @return true if the feature is enabled
         */
        public boolean isWorldFeatureEnabled(String key) {
            if (!worldFeatures.containsKey(key)) {
                LOGGER.warn("Requested world feature with unknown config key: {}", key);
                return false;
            }
            return worldFeatures.get(key).get();
        }
    }

    /**
     * Client-only configuration
     */
    public static class Client {
        // Rendering category
        public final BooleanValue logicwires3D;
        public final BooleanValue staticWires;
        public final BooleanValue staticGates;
        public final IntValue lightHaloMax;
        public final BooleanValue fabulousLights;

        private Client(ModConfigSpec.Builder builder) {
            // Rendering settings
            builder.comment("Render settings")
                    .push("rendering");

            logicwires3D = builder
                    .comment("If set to false, flat wire textures will be used for logic gates. Significant performance improvement")
                    .define("gate_3d_wires", true);

            staticWires = builder
                    .comment("If set to false, wires will be rendered in the TESR rather than the WorldRenderer")
                    .define("static_wire_renderer", true);

            staticGates = builder
                    .comment("If set to false, gates will be rendered in the TESR rather than the WorldRenderer")
                    .define("static_gate_renderer", true);

            builder.pop();

            // Lighting
            builder.comment("Lighting settings")
                    .push("lighting");

            lightHaloMax = builder
                    .comment("Max lights on screen at a time, -1 for unlimited. This limits the number of light halos that can be rendered"
                            + " around ProjectRed light sources. Lower values improve performance. ")
                    .defineInRange("max_lights", -1, -1, 1024);

            fabulousLights = builder
                    .comment("Use fabulous shader pipeline for lights when on Fabulous Graphics mode. This creates a screenspace"
                            + "blooming effect when looking towards ProjectRed light sources.")
                    .define("fabulous_lights", true);

            builder.pop();
        }
    }

    @SubscribeEvent
    public static void onLoad(final ModConfigEvent.Loading configEvent) {
        var loadedSpec = configEvent.getConfig().getSpec();
        if (loadedSpec != serverSpec && loadedSpec != clientSpec) {
            return;
        }

        try {
            // Check for old config
            Path oldConfigPath = Paths.get("config", "ProjectRed.cfg");
            if (!Files.exists(oldConfigPath)) {
                LOGGER.debug("ProjectRed config migration: No legacy config found, skipping migration...");
                return;
            } else {
                LOGGER.debug("ProjectRed config migration: Legacy config found, attempting migration...");
            }

            // Load legacy config using CodeChickenLib
            ConfigFile legacyConfig = new ConfigFile(MOD_ID).path(oldConfigPath);
            ConfigCategory root = legacyConfig.load();
            ConfigCategory migrationCategory = root
                    .getCategory("neoforge_config_migration")
                    .setComment("Records migration from legacy ProjectRed.cfg to NeoForge Config toml config");

            // Check if already migrated
            ConfigValue migrationValue = migrationCategory
                    .getValue(loadedSpec == serverSpec ? "server_migrated" : "client_migrated")
                    .setDefaultBoolean(false);

            if (migrationValue.getBoolean()) {
                LOGGER.debug("Legacy config indicates already migrated ({}). Skipping...", migrationValue.getName());
                return;
            }

            // Run appropriate migration function
            LOGGER.info("Migrating server settings {} -> {}", oldConfigPath.getFileName(), configEvent.getConfig().getFileName());
            if (loadedSpec == serverSpec) {
                migrateLegacyServerValues(root);
                serverSpec.save();
            } else {
                migrateLegacyClientValues(root);
                clientSpec.save();
            }

            // Mark as migrated to prevent re-migration
            migrationValue.setBoolean(true);
            migrationCategory.save();

        } catch (Exception e) {
            LOGGER.error("Failed to migrate legacy config, using defaults", e);
        }
    }

    private static void migrateLegacyServerValues(ConfigCategory root) {
        // Gates category
        ConfigCategory general = root.getCategory("general");
        SERVER.logicGateSounds.set(general.getValue("gate_sounds").setDefaultBoolean(true).getBoolean());
        SERVER.logicGateLights.set(general.getValue("gate_lights").setDefaultBoolean(true).getBoolean());
        SERVER.minTimerTicks.set(general.getValue("gate_min_timer_ticks").setDefaultInt(4).getInt());
        SERVER.unbreakableScrewdriver.set(general.getValue("infinite_screwdriver").setDefaultBoolean(false).getBoolean());

        // Machines category
        ConfigCategory machines = root.getCategory("machines");
        SERVER.frameMoveLimit.set(machines.getValue("frame_move_limit").setDefaultInt(1024).getInt());

        // World Gen category
        ConfigCategory gen = root.getCategory("world_gen");
        SERVER.generateRubyOres.set(gen.getValue("ruby_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generateSapphireOres.set(gen.getValue("sapphire_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generatePeridotOres.set(gen.getValue("peridot_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generateTinOres.set(gen.getValue("tin_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generateSilverOres.set(gen.getValue("silver_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generateElectrotineOres.set(gen.getValue("electrotine_ore").setDefaultBoolean(true).getBoolean());
        SERVER.generateMarbleCaves.set(gen.getValue("marble_cave").setDefaultBoolean(true).getBoolean());

        // Fabrication category
        ConfigCategory fab = root.getCategory("fabrication");
        SERVER.autoCompileTileLimit.set(fab.getValue("auto_compile_tile_limit").setDefaultInt(20).getInt());
    }

    private static void migrateLegacyClientValues(ConfigCategory root) {
        // Rendering category
        ConfigCategory rendering = root.getCategory("rendering");
        CLIENT.logicwires3D.set(rendering.getValue("gate_3d_wires").setDefaultBoolean(true).getBoolean());
        CLIENT.staticWires.set(rendering.getValue("static_wire_renderer").setDefaultBoolean(true).getBoolean());
        CLIENT.staticGates.set(rendering.getValue("static_gate_renderer").setDefaultBoolean(true).getBoolean());
        CLIENT.lightHaloMax.set(rendering.getValue("max_lights").setDefaultInt(-1).getInt());
        CLIENT.fabulousLights.set(rendering.getValue("fabulous_lights").setDefaultBoolean(true).getBoolean());
    }
}
