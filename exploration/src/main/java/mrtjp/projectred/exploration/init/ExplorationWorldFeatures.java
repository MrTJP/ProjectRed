package mrtjp.projectred.exploration.init;

import com.google.common.collect.ImmutableList;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.exploration.world.gen.MarbleCaveWorldCarver;
import net.minecraft.core.Holder;
import net.minecraft.data.BuiltinRegistries;
import net.minecraft.data.worldgen.features.FeatureUtils;
import net.minecraft.data.worldgen.features.OreFeatures;
import net.minecraft.data.worldgen.placement.PlacementUtils;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.valueproviders.UniformFloat;
import net.minecraft.world.level.biome.Biome;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.levelgen.GenerationStep;
import net.minecraft.world.level.levelgen.VerticalAnchor;
import net.minecraft.world.level.levelgen.carver.CarverDebugSettings;
import net.minecraft.world.level.levelgen.carver.CaveCarverConfiguration;
import net.minecraft.world.level.levelgen.carver.ConfiguredWorldCarver;
import net.minecraft.world.level.levelgen.carver.WorldCarver;
import net.minecraft.world.level.levelgen.feature.ConfiguredFeature;
import net.minecraft.world.level.levelgen.feature.Feature;
import net.minecraft.world.level.levelgen.feature.configurations.OreConfiguration;
import net.minecraft.world.level.levelgen.heightproviders.UniformHeight;
import net.minecraft.world.level.levelgen.placement.*;
import net.minecraftforge.common.world.BiomeGenerationSettingsBuilder;
import net.minecraftforge.event.world.BiomeLoadingEvent;
import net.minecraftforge.registries.RegistryObject;

import java.util.List;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.ProjectRedExploration.WORLD_CARVERS;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;
import static net.minecraft.world.level.biome.Biome.BiomeCategory.*;

public class ExplorationWorldFeatures {

    public static final String ID_MARBLE_CAVE_CARVER = "marble_cave";

    // Marble Cave world carver
    private static RegistryObject<WorldCarver<CaveCarverConfiguration>> MARBLE_CAVE_CARVER;

    // Ore placements
    public static Holder<PlacedFeature> RUBY_ORE_PLACED_FEATURE;
    public static Holder<PlacedFeature> SAPPHIRE_ORE_PLACED_FEATURE;
    public static Holder<PlacedFeature> PERIDOT_ORE_PLACED_FEATURE;
    public static Holder<PlacedFeature> ELECTROTINE_ORE_PLACED_FEATURE;
    public static Holder<PlacedFeature> TIN_ORE_PLACED_FEATURE;
    public static Holder<PlacedFeature> SILVER_ORE_PLACED_FEATURE;

    // Carver features
    private static Holder<ConfiguredWorldCarver<?>> MARBLE_CAVE_CONFIGURED_CARVER;

    public static void register() {

        MARBLE_CAVE_CARVER = WORLD_CARVERS.register(ID_MARBLE_CAVE_CARVER, () -> new MarbleCaveWorldCarver(CaveCarverConfiguration.CODEC));
    }

    public static void load() {
        // Load configurations of Ore features and world carvers. Happens during common init because there is no Forge registry for this

        // Create features
        Holder<ConfiguredFeature<OreConfiguration, ?>> RUBY_ORE_CONFIGURED_FEATURE          = registerOreConfiguration("ruby_ore",        RUBY_ORE_BLOCK, DEEPSLATE_RUBY_ORE_BLOCK,         Configurator.gen_RubyVeinSize);
        Holder<ConfiguredFeature<OreConfiguration, ?>> SAPPHIRE_ORE_CONFIGURED_FEATURE      = registerOreConfiguration("sapphire_ore",    SAPPHIRE_ORE_BLOCK, DEEPSLATE_SAPPHIRE_ORE_BLOCK, Configurator.gen_SapphireVeinSize);
        Holder<ConfiguredFeature<OreConfiguration, ?>> PERIDOT_ORE_CONFIGURED_FEATURE       = registerOreConfiguration("peridot_ore",     PERIDOT_ORE_BLOCK, DEEPSLATE_PERIDOT_ORE_BLOCK,   Configurator.gen_PeridotVeinSize);
        Holder<ConfiguredFeature<OreConfiguration, ?>> ELECTROTINE_ORE_CONFIGURED_FEATURE   = registerOreConfiguration("electrotine_ore", ELECTROTINE_ORE_BLOCK, DEEPSLATE_ELECTROTINE_ORE_BLOCK, Configurator.gen_ElectrotineVeinSize);
        Holder<ConfiguredFeature<OreConfiguration, ?>> TIN_ORE_CONFIGURED_FEATURE           = registerOreConfiguration("tin_ore",         TIN_ORE_BLOCK, DEEPSLATE_TIN_ORE_BLOCK,           Configurator.gen_TinVeinSize);
        Holder<ConfiguredFeature<OreConfiguration, ?>> SILVER_ORE_CONFIGURED_FEATURE        = registerOreConfiguration("silver_ore",      SILVER_ORE_BLOCK, DEEPSLATE_SILVER_ORE_BLOCK,     Configurator.gen_SilverVeinSize);

        // Create placements
        RUBY_ORE_PLACED_FEATURE         = createOrePlacements("ruby_ore",        RUBY_ORE_CONFIGURED_FEATURE,        -80, 80, 1);
        SAPPHIRE_ORE_PLACED_FEATURE     = createOrePlacements("sapphire_ore",    SAPPHIRE_ORE_CONFIGURED_FEATURE,    -80, 80, 1);
        PERIDOT_ORE_PLACED_FEATURE      = createOrePlacements("peridot_ore",     PERIDOT_ORE_CONFIGURED_FEATURE,     -80, 80, 1);
        ELECTROTINE_ORE_PLACED_FEATURE  = createOrePlacements("electrotine_ore", ELECTROTINE_ORE_CONFIGURED_FEATURE, -32, 32, 4);
        TIN_ORE_PLACED_FEATURE          = createOrePlacements("tin_ore",         TIN_ORE_CONFIGURED_FEATURE,         -24, 56, 8);
        SILVER_ORE_PLACED_FEATURE       = createOrePlacements("silver_ore",      SILVER_ORE_CONFIGURED_FEATURE,      -64, 32, 6);

        MARBLE_CAVE_CONFIGURED_CARVER = registerMarbleCaveCarverConfiguration(0.02F);
    }

    public static void onBiomeLoadingEvent(final BiomeLoadingEvent event) {
        Biome.BiomeCategory category = event.getCategory();
        BiomeGenerationSettingsBuilder builder = event.getGeneration();

        if (category != NONE && category != THEEND && category != NETHER) { // if overworld

            if (Configurator.gen_Ruby)        builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, RUBY_ORE_PLACED_FEATURE);
            if (Configurator.gen_Sapphire)    builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, SAPPHIRE_ORE_PLACED_FEATURE);
            if (Configurator.gen_Peridot)     builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, PERIDOT_ORE_PLACED_FEATURE);
            if (Configurator.gen_Electrotine) builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, ELECTROTINE_ORE_PLACED_FEATURE);
            if (Configurator.gen_Tin)         builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, TIN_ORE_PLACED_FEATURE);
            if (Configurator.gen_Silver)      builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, SILVER_ORE_PLACED_FEATURE);

            if (Configurator.gen_MarbleCave)  builder.addCarver(GenerationStep.Carving.AIR, MARBLE_CAVE_CONFIGURED_CARVER);
        }
    }

    // Registers the actual ore feature. This describes a single cluster of this specific ore type
    private static Holder<ConfiguredFeature<OreConfiguration, ?>> registerOreConfiguration(String id, Block standard, Block deepslate, int veinSize) {
        return FeatureUtils.register(
                modId(id),
                Feature.ORE,
                new OreConfiguration(ImmutableList.of(
                        OreConfiguration.target(OreFeatures.STONE_ORE_REPLACEABLES, standard.defaultBlockState()),
                        OreConfiguration.target(OreFeatures.DEEPSLATE_ORE_REPLACEABLES, deepslate.defaultBlockState())), veinSize));
    }

    // Registers a placement for the given feature. This controls how many of said features spawn and where
    private static Holder<PlacedFeature> createOrePlacements(String id, Holder<ConfiguredFeature<OreConfiguration, ?>> configuredFeature, int minY, int maxY, int count) {
        List<PlacementModifier> modifiers = ImmutableList.of(
                CountPlacement.of(count),
                InSquarePlacement.spread(),
                HeightRangePlacement.triangle(VerticalAnchor.absolute(minY), VerticalAnchor.absolute(maxY)));

        return PlacementUtils.register(
                modId(id),
                configuredFeature,
                modifiers);
    }

    private static Holder<ConfiguredWorldCarver<?>> registerMarbleCaveCarverConfiguration(float probability) {
        ConfiguredWorldCarver<CaveCarverConfiguration> configuredCarver = MARBLE_CAVE_CARVER.get().configured(new CaveCarverConfiguration(
                probability,
                UniformHeight.of(VerticalAnchor.aboveBottom(8), VerticalAnchor.absolute(180)), // Max/min heights
                UniformFloat.of(0.1F, 0.9F), // y scale
                VerticalAnchor.aboveBottom(8), // lava level
                CarverDebugSettings.of(false, Blocks.CRIMSON_BUTTON.defaultBlockState()), // debug settings (enable, air state)
                UniformFloat.of(0.7F, 1.4F), // horizontal radius
                UniformFloat.of(0.8F, 1.3F), // vertical radius
                UniformFloat.of(-1.0F, -0.4F))); // floor level

        return BuiltinRegistries.register(
                BuiltinRegistries.CONFIGURED_CARVER,
                new ResourceLocation(MOD_ID, ID_MARBLE_CAVE_CARVER),
                configuredCarver);
    }

    private static String modId(String id) {
        return MOD_ID + ":" + id;
    }
}
