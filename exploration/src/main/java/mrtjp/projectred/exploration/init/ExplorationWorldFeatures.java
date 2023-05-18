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

    // Ore features
    public static Holder<PlacedFeature> RUBY_ORE_FEATURE;
    public static Holder<PlacedFeature> SAPPHIRE_ORE_FEATURE;
    public static Holder<PlacedFeature> PERIDOT_ORE_FEATURE;
    public static Holder<PlacedFeature> COPPER_ORE_FEATURE;
    public static Holder<PlacedFeature> TIN_ORE_FEATURE;
    public static Holder<PlacedFeature> SILVER_ORE_FEATURE;
    public static Holder<PlacedFeature> ELECTROTINE_ORE_FEATURE;

    // Carver features
    private static Holder<ConfiguredWorldCarver<?>> MARBLE_CAVE_FEATURE;

    public static void register() {

        MARBLE_CAVE_CARVER = WORLD_CARVERS.register(ID_MARBLE_CAVE_CARVER, () -> new MarbleCaveWorldCarver(CaveCarverConfiguration.CODEC));
    }

    public static void load() {
        // Load configurations of Ore features and world carvers. Happens during common init because there is no Forge registry for this

        RUBY_ORE_FEATURE        = registerOreConfiguration(RUBY_ORE_BLOCK,          Configurator.gen_RubyVeinSize,        12, 20, 1);
        SAPPHIRE_ORE_FEATURE    = registerOreConfiguration(SAPPHIRE_ORE_BLOCK,      Configurator.gen_SapphireVeinSize,    12, 20, 1);
        PERIDOT_ORE_FEATURE     = registerOreConfiguration(PERIDOT_ORE_BLOCK,       Configurator.gen_PeridotVeinSize,     18, 26, 1);

        COPPER_ORE_FEATURE      = registerOreConfiguration(COPPER_ORE_BLOCK,        Configurator.gen_CopperVeinSize,      0, 64, 16);
        TIN_ORE_FEATURE         = registerOreConfiguration(TIN_ORE_BLOCK,           Configurator.gen_TinVeinSize,         0, 48, 10);
        SILVER_ORE_FEATURE      = registerOreConfiguration(SILVER_ORE_BLOCK,        Configurator.gen_SilverVeinSize,      0, 32, 8);

        ELECTROTINE_ORE_FEATURE = registerOreConfiguration(ELECTROTINE_ORE_BLOCK,   Configurator.gen_ElectrotineVeinSize, 0, 16, 4);

        MARBLE_CAVE_FEATURE = registerMarbleCaveCarverConfiguration(0.02F);
    }

    public static void onBiomeLoadingEvent(final BiomeLoadingEvent event) {
        Biome.BiomeCategory category = event.getCategory();
        BiomeGenerationSettingsBuilder builder = event.getGeneration();

        if (category != NONE && category != THEEND && category != NETHER) { // if overworld

            if (Configurator.gen_Ruby)        builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, RUBY_ORE_FEATURE);
            if (Configurator.gen_Sapphire)    builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, SAPPHIRE_ORE_FEATURE);
            if (Configurator.gen_Peridot)     builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, PERIDOT_ORE_FEATURE);
            if (Configurator.gen_Copper)      builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, COPPER_ORE_FEATURE);
            if (Configurator.gen_Tin)         builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, TIN_ORE_FEATURE);
            if (Configurator.gen_Silver)      builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, SILVER_ORE_FEATURE);
            if (Configurator.gen_Electrotine) builder.addFeature(GenerationStep.Decoration.UNDERGROUND_ORES, ELECTROTINE_ORE_FEATURE);

            if (Configurator.gen_MarbleCave)  builder.addCarver(GenerationStep.Carving.AIR, MARBLE_CAVE_FEATURE);
        }
    }

    private static Holder<PlacedFeature> registerOreConfiguration(Block block, int veinSize, int minY, int maxY, int count) {

        String id = new ResourceLocation(MOD_ID, block.getRegistryName().getPath()).toString();

        Holder<ConfiguredFeature<OreConfiguration, ?>> configuredFeature = FeatureUtils.register(
                id,
                Feature.ORE,
                new OreConfiguration(ImmutableList.of(OreConfiguration.target(OreFeatures.STONE_ORE_REPLACEABLES, block.defaultBlockState())), veinSize));

        List<PlacementModifier> modifiers = ImmutableList.of(
                CountPlacement.of(count),
                InSquarePlacement.spread(),
                HeightRangePlacement.uniform(VerticalAnchor.absolute(minY), VerticalAnchor.absolute(maxY)));

        return PlacementUtils.register(
                id,
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
}
